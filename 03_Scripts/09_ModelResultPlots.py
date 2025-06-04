import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import pandas as pd
import numpy as np
import flopy
import geopandas as gpd
from pathlib import Path
from hydroeval import evaluator, nse, kge, rmse, pbias

# import os
# os.chdir('../')

import sys
sys.path.append('./03_Scripts')
from HOB_weight_processing import hob_to_df, wt_dict, calculate_hob_weights

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Directories
data_dir = Path('01_Data/')
shp_dir = data_dir / 'shapefiles'
plt_dir = Path('04_Plots')
svihm_dir = Path('../SVIHM/')  # External to project, local SVIHM Git repo
svihm_ref_dir = svihm_dir / 'SVIHM_Input_Files/reference_data_for_plots/'
model_dir = Path('//BEHEMOTH/Users/lelan/Documents/ModelRuns/SVIHM/2025_t2p_calibration/10_init_iter4/SVIHM/MODFLOW/')
tex_file_dir = model_dir / '../preproc/'

# Texture files
tex_files = {
    'FINE': 't2p_FINE.csv',
    'MIXED_FINE': 't2p_MIXED_FINE.csv',
    'SAND': 't2p_SAND.csv',
    'MIXED_COARSE': 't2p_MIXED_COARSE.csv',
    'VERY_COARSE': 't2p_VERY_COARSE.csv'
}

# Shapefiles
sv_model_shp_file = shp_dir / 'grid_properties_rep.shp'

# Model Info
xoff = 499977
yoff = 4571330
origin_date = pd.to_datetime('1990-9-30')

# Outputs


# -------------------------------------------------------------------------------------------------------------------- #
# Classes/Functions
# -------------------------------------------------------------------------------------------------------------------- #

def convert_to_2d(geom):
    if geom.has_z:
        return gpd.GeoSeries([gpd.GeoSeries([geom]).apply(lambda g: g.__class__(tuple(coord[:2]) for coord in g.exterior.coords))[0]])[0]
    return geom

# -------------------------------------------------------------------------------------------------------------------- #

def upw_to_df(upw, ibound=None):
    """Convert UPW parameters (HK, VKA, SS, SY) and optionally IBOUND into a long-format DataFrame."""
    def to_series(arr, name):
        arr = np.asarray(arr)
        if arr.ndim == 2:  # single layer, expand to 3D
            arr = arr[np.newaxis, :, :]
        nlay, nrow, ncol = arr.shape
        lay, row, col = np.meshgrid(np.arange(nlay), np.arange(nrow), np.arange(ncol), indexing='ij')
        return pd.DataFrame({
            'Layer': lay.ravel() + 1,
            'Row': row.ravel() + 1,
            'Column': col.ravel() + 1,
            name: arr.ravel()
        })

    # Assemble individual DataFrames
    df = to_series(upw.hk.array, 'HK') \
        .merge(to_series(upw.vka.array, 'VK'), on=['Layer', 'Row', 'Column']) \
        .merge(to_series(upw.ss.array, 'SS'), on=['Layer', 'Row', 'Column']) \
        .merge(to_series(upw.sy.array, 'SY'), on=['Layer', 'Row', 'Column'])

    # Optionally add IBOUND
    if ibound is not None:
        df = df.merge(to_series(ibound, 'IBOUND'), on=['Layer', 'Row', 'Column'])

    return df

# -------------------------------------------------------------------------------------------------------------------- #

def calc_metrics(group, weight_col=None):
    obs = group['obsval'].values
    sim = group['simval'].values
    if weight_col is not None:
        # If the sum of weights is zero, remove
        if group[weight_col].max()==0:
            return pd.Series({'RMSE': np.nan, 'NSE': np.nan, 'KGE': np.nan, 'PBIAS': np.nan})
        # Otherwise use the weights to adjust the values
        sim *= group[weight_col].values
    if len(obs) < 5 or np.any(np.isnan(obs)) or np.any(np.isnan(sim)):
        return pd.Series({'RMSE': np.nan, 'NSE': np.nan, 'KGE': np.nan, 'PBIAS': np.nan})

    return pd.Series({
        'RMSE': evaluator(rmse, sim, obs)[0],
        'NSE': evaluator(nse, sim, obs)[0],
        'KGE': evaluator(kge, sim, obs)[0][0],
        'PBIAS': evaluator(pbias, sim, obs)[0]
    })

# -------------------------------------------------------------------------------------------------------------------- #

def calc_PEST_res(df):
    res_df = df.copy()
    res_df['res'] = res_df['obsval'] - res_df['simval']
    res_df['wtsqres'] = res_df['res']**2 * res_df['wt']**2
    return res_df

# -------------------------------------------------------------------------------------------------------------------- #

def plot_well_metrics(well_df, grid_df, metric, prop=None, cmap='viridis', breaks=None, ax=None):
    """
    Plot well metrics over a MODFLOW grid with optional hydraulic property coloring.

    Parameters:
        well_df: GeoDataFrame of wells (with metric column)
        grid_df: GeoDataFrame of grid cells (with hydraulic properties)
        metric: Name of the well metric to plot (e.g., 'RMSE', 'NSE', 'KGE')
        prop: Optional property in grid_df to show as background (e.g., 'HK')
        cmap: Colormap for the hydraulic property
        breaks: List or array of color breaks for the well metric (if None, uses auto)
        ax: Optional matplotlib Axes object
    """
    if ax is None:
        fig, ax = plt.subplots(figsize=(10, 8))

    # Plot model grid background
    if prop and prop in grid_df.columns:
        grid_df.plot(column=prop, ax=ax, cmap=cmap, edgecolor='none', legend=True)
    else:
        grid_df.plot(color='none', ax=ax, edgecolor='lightgrey')

    # Determine color breaks for well metrics
    if breaks is None:
        # Semi-automatic: Use quantiles with clamping at 95th percentile
        upper = well_df[metric].quantile(0.95)
        lower = well_df[metric].quantile(0.05)
        breaks = np.linspace(lower, upper, 6)
        breaks = np.append(breaks, well_df[metric].max())  # final bin for outliers

    # Define color normalization based on breaks
    norm = mcolors.BoundaryNorm(breaks, ncolors=len(breaks)-1)
    cmap_pts = plt.get_cmap('Reds', len(breaks)-1)

    # Plot well points
    well_df.plot(ax=ax, column=metric, cmap=cmap_pts, norm=norm, edgecolor='black', linewidth=0.5, markersize=40, legend=True)

    ax.set_title(f'{metric} per Well')
    ax.set_axis_off()
    plt.tight_layout()

# -------------------------------------------------------------------------------------------------------------------- #

def check_obs_above_ground(hob_df, gwf):
    """
    For each well in hob_df, check if any observation heads are above the model’s ground surface.
    Ground surface is taken from gwf.dis.top. Prints the well name and number of obs above ground
    if any are found.
    """
    top_array = gwf.dis.top.array  # 2D array of the top elevation for each cell
    for well_id, group in hob_df.groupby('wellid'):
        row = group['row'].iloc[0]
        col = group['col'].iloc[0]
        ground_surface = top_array[row, col]
        # Count how many observations exceed ground surface
        above_count = (group['obsval'] > ground_surface).sum()
        if above_count > 0:
            print(f"Well {well_id}: {above_count} observation(s) above ground surface of {ground_surface}.")

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in MODFLOW Model
print('Reading MODFLOW Model')
gwf = flopy.modflow.Modflow.load('SVIHM.nam', load_only=['dis','bas6','upw'], version='mfnwt', model_ws=model_dir)
gwf.modelgrid.set_coord_info(xoff=xoff, yoff=yoff)
print('Reading MODFLOW Hobs... (slow)')
hob = flopy.modflow.ModflowHob.load(model_dir / "svihm.hob", model=gwf)
print('Hobs read.')

# Read in hob key for XY locations...
hob_locs = pd.read_csv(svihm_ref_dir / '_hob_key.csv')

# Convert to DF, also reads in simulated values
hob_df = hob_to_df(hob, origin_date, model_dir/ 'HobData_SVIHM.dat')
hob_df = calculate_hob_weights(hob_df, wt_dict, gwf.get_package('BAS6'))

# Calculate metrics for HOB wells
well_metrics = hob_df.groupby('wellid').apply(calc_metrics, weight_col='wt', include_groups=False).reset_index()
well_metrics = well_metrics[~well_metrics['RMSE'].isna()]
well_metrics = pd.merge(well_metrics,
                        hob_locs[['well_id','x_proj','y_proj']].drop_duplicates(),
                        left_on='wellid', right_on='well_id')
# Convert to GDF
well_gdf = gpd.GeoDataFrame(well_metrics, geometry=gpd.points_from_xy(well_metrics.x_proj, well_metrics.y_proj))

# Read MODFLOW grid shapefile
grid = gpd.read_file(sv_model_shp_file)
#grid['geometry'] = grid['geometry'].apply(convert_to_2d)

# Create UPW properties DataFrame
upw_df = upw_to_df(gwf.upw, ibound=gwf.bas6.ibound.array)

# Merge in properties
grid = grid.merge(upw_df, how='left', on=['Layer', 'Row', 'Column'])

# Drop ibound==0 cells
grid = grid[grid['IBOUND']==1]

# How PEST sees it
PEST_res = calc_PEST_res(hob_df)
PEST_res.groupby('wellid')['wtsqres'].sum().sort_values().tail(10)

check_obs_above_ground(hob_df, gwf)

# -------------------------------------------------------------------------------------------------------------------- #

# Maps
plt.ion()
plot_well_metrics(well_gdf, grid, metric='RMSE', prop=None)
plot_well_metrics(well_gdf, grid, metric='KGE', prop=None)
plot_well_metrics(well_gdf, grid, metric='PBIAS', prop=None)