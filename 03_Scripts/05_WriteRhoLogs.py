import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt

import pandas as pd
import geopandas as gpd
from pathlib import Path
from pykrige.ok import OrdinaryKriging
import t2py

# Local
import sys
sys.path.append('./03_Scripts/')
from aem_read import read_xyz, aem_wide2long

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Directories
data_dir = Path('01_Data/')
shp_dir = data_dir / 'shapefiles'
out_dir = Path('05_Outputs')

# Files
aem_sharp_file = data_dir / 'SCI_Sharp_10_West_I01_MOD_inv.xyz'
aem_litho_file = data_dir / 'AEM_WELL_LITHOLOGY_csv_WO2_20220710_CVHM.csv'
wl_file = data_dir / 'WLs_Oct312021.csv'

# Shapefiles
aem_sharp_sv_file = shp_dir / 'aem_sv_Sharp_I01_MOD_inv_UTM10N_idwwl.shp'
aem_hqwells_file = shp_dir / 'aem_sv_HQ_LithologyWells_UTM10N.shp'
aem_wl_file = shp_dir / 'aem_sv_WaterLevel_Data_UTM10N.shp'
sv_model_domain_file = shp_dir / 'Model_Domain_20180222.shp'

# Inclusion Radius
svihm_buffer = 500   # meters

# -------------------------------------------------------------------------------------------------------------------- #
# Functions/Classes
# -------------------------------------------------------------------------------------------------------------------- #

def plot_intervals_below_water_table(aem_shp, aem_long, domain, buffer, title, output_file=None):
    """
    Plot the number of AEM depth intervals below the water table at each point.

    Parameters:
        aem_shp (GeoDataFrame): GeoDataFrame of AEM points with spatial information.
        aem_long (DataFrame): DataFrame of AEM depth intervals, including DEP_TOP and ok_dtw.
        domain (GeoDataFrame): GeoDataFrame of the SVIHM domain polygon.
        buffer (GeoSeries): GeoSeries of the SVIHM buffer polygon.
        title (str): Title of the plot.
        output_file (str, optional): Filepath to save the plot as an image. Defaults to None.
    """
    # Count intervals below the water table for each point
    below_water_counts = aem_long.loc[aem_long['DEP_TOP'] >= aem_long['ok_dtw']].groupby('FID').size()

    # Merge counts with aem_shp
    aem_shp['below_water_count'] = aem_shp['FID'].map(below_water_counts).fillna(0).astype(int)

    # Plot setup
    fig, ax = plt.subplots(figsize=(12, 8))

    # Plot domain and buffer
    domain.plot(ax=ax, color='none', edgecolor='black', linewidth=1, label='SVIHM Domain')
    buffer.boundary.plot(ax=ax, color='green', linestyle='--', linewidth=1, label='500m Buffer')

    # Plot AEM points, color-coded by interval counts
    aem_shp.plot(
        ax=ax,
        column='below_water_count',
        cmap='viridis',
        markersize=5,
        legend=True,
        legend_kwds={'label': 'Depth Intervals Below Water Table'},
    )

    # Customize the plot
    ax.set_title(title, fontsize=14)
    ax.set_xlabel('Easting (UTM)', fontsize=12)
    ax.set_ylabel('Northing (UTM)', fontsize=12)
    ax.legend()
    plt.grid()
    plt.tight_layout()

    # Save or show the plot
    if output_file:
        plt.savefig(output_file, dpi=300)
    else:
        plt.show()

# -------------------------------------------------------------------------------------------------------------------- #

def plot_minimum_depth_below_water_table(aem_shp, aem_long, domain, buffer, title, output_file=None):
    """
    Plot the minimum depth (DEP_TOP) of AEM data below the water table at each point.

    Parameters:
        aem_shp (GeoDataFrame): GeoDataFrame of AEM points with spatial information.
        aem_long (DataFrame): DataFrame of AEM depth intervals, including DEP_TOP and ok_dtw.
        domain (GeoDataFrame): GeoDataFrame of the SVIHM domain polygon.
        buffer (GeoSeries): GeoSeries of the SVIHM buffer polygon.
        title (str): Title of the plot.
        output_file (str, optional): Filepath to save the plot as an image. Defaults to None.
    """
    # Calculate the minimum DEP_TOP below the water table for each point
    min_depths = (
        aem_long.loc[aem_long['DEP_TOP'] >= aem_long['ok_dtw']]
        .groupby('FID')['DEP_TOP']
        .min()
    )

    # Merge minimum depths with aem_shp
    aem_shp['min_depth_below_wt'] = aem_shp['FID'].map(min_depths)

    # Plot setup
    fig, ax = plt.subplots(figsize=(12, 8))

    # Plot domain and buffer
    domain.plot(ax=ax, color='none', edgecolor='black', linewidth=1, label='SVIHM Domain')
    buffer.boundary.plot(ax=ax, color='green', linestyle='--', linewidth=1, label='500m Buffer')

    # Plot AEM points, color-coded by minimum depth
    aem_shp.plot(
        ax=ax,
        column='min_depth_below_wt',
        cmap='plasma',
        markersize=5,
        legend=True,
        legend_kwds={'label': 'Minimum Depth Below Water Table (m)'},
    )

    # Customize the plot
    ax.set_title(title, fontsize=14)
    ax.set_xlabel('Easting (UTM)', fontsize=12)
    ax.set_ylabel('Northing (UTM)', fontsize=12)
    ax.legend()
    plt.grid()
    plt.tight_layout()

    # Save or show the plot
    if output_file:
        plt.savefig(output_file, dpi=300)
    else:
        plt.show()

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in data
aem_wide = read_xyz(aem_sharp_file, 26, x_col='UTMX', y_col='UTMY', delim_whitespace=True)
obs_wls = pd.read_csv(wl_file)

# Read in shapefiles
aem_shp = gpd.read_file(aem_sharp_sv_file)
aem_shp['X'] = aem_shp.geometry.x
aem_shp['Y'] = aem_shp.geometry.y
svihm_domain = gpd.read_file(sv_model_domain_file)

# Create buffer
buffer_shp = svihm_domain.buffer(svihm_buffer)

# Limit to SV buffer (points inside the domain or buffer)
aem_shp = aem_shp[aem_shp.within(buffer_shp.union_all())]

# Get Water Levels at log locs
ok = OrdinaryKriging(obs_wls['UTM_x'], obs_wls['UTM_y'], obs_wls['DTW_m'],
                     variogram_model='spherical',
                     variogram_parameters=[42, 4500, 0.0], enable_plotting=False, nlags=30)
aem_shp['ok_dtw'], ok_ss = ok.execute('points', n_closest_points=18, backend='loop',
                           xpoints=aem_shp.geometry.x,
                           ypoints=aem_shp.geometry.y)

# Subset to Scott Valley, add various shp columns to wide
aem_wide = aem_wide[aem_wide['LINE_NO'].isin(aem_shp['LINE_NO'])]
aem_wide = aem_wide.merge(aem_shp[['LINE_NO', 'FID', 'X', 'Y', 'ok_dtw']], on=['LINE_NO','FID'], how='inner')

# Made data long (one point per row)
aem_long = aem_wide2long(aem_wide,
                         id_col_prefixes=['RHO_I', 'RHO_I_STD', 'SIGMA_I', 'DEP_TOP', 'DEP_BOT', 'THK', 'THK_STD', 'DEP_BOT_STD'],
                         line_col='LINE_NO')

#-- There's no bottom for point 30 at each point (lowest pixel), so drop those values
aem_long = aem_long.dropna(subset='DEP_BOT')

# Drop entries below DOI (conservative)
#aem_long = aem_long.loc[(aem_long['DEP_TOP'] < aem_long['DOI_CONSERVATIVE'])]

# Drop any NA Rho values
aem_long = aem_long.loc[~aem_long['RHO_I'].isna()]

plot_intervals_below_water_table(
    aem_shp=aem_shp,
    aem_long=aem_long,
    domain=svihm_domain,
    buffer=buffer_shp,
    title='Intervals Below the Water Table at AEM Points')

plot_minimum_depth_below_water_table(
    aem_shp=aem_shp,
    aem_long=aem_long,
    domain=svihm_domain,
    buffer=buffer_shp,
    title='Minimum Depth Below Water Table at AEM Points')

# Filter out unsaturated points (above water table)
aem_long = aem_long.loc[(aem_long['DEP_TOP'] >= aem_long['ok_dtw'])]

# Setup T2PY output & write
aem_long['line_id'] = aem_long.agg(lambda x: f"{x['LINE_NO']:g}_{x['FID']:g}", axis=1)
wells = t2py.Dataset(classes=['Rho'])
wells.add_wells_by_df(df = aem_long,
                      name_col='line_id',
                      zland_col='ELEVATION',
                      depth_col='DEP_BOT',
                      depth_top_col='DEP_TOP',
                      data_class_cols={'Rho': 'RHO_I'})

print('Writing File...')
out_dir.mkdir(parents=True, exist_ok=True)
wells.write_file(out_dir / 'AEMLog_noUnsat.dat', header=['Line', 'ID', 'n', 'X', 'Y', 'Zland', 'Depth', 'Rho'])
print('Done.')