import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import mapclassify
import re
import numpy as np
import flopy as fp
import pandas as pd
import geopandas as gpd
from tqdm import tqdm
from pathlib import Path

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Directories
shp_dir = Path('01_Data/') / 'shapefiles'
out_dir = Path('05_Outputs')
mod_dir = Path('./02_Models/SVIHM_MF_Calibrated/MODFLOW')
plt_dir = Path('./04_Plots/')

sfr_cols = ['layer', 'row', 'column', 'segment', 'reach', 'length', 'elevation', 'slope', 'thick', 'sbk']

# Model dis info
xoff, yoff = 499977, 4571330
angrot = 0.0
nam_file = 'svihm.nam'

# Files
sfr_file = mod_dir / 'svihm.sfr'

# Outputs
pst_run_name = 'calib_12_itermix'
sfr_shapefile   = shp_dir / f"sfr_properties_{pst_run_name}.shp"


# -------------------------------------------------------------------------------------------------------------------- #
# Classes/Functions
# -------------------------------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in model
mf = fp.modflow.Modflow.load(nam_file,
                             model_ws=mod_dir,
                             version='mfnwt',
                             load_only=['dis','upw','bas6'],
                             check=False)
mf.modelgrid.set_coord_info(xoff=xoff, yoff=yoff)

# Loop over layers
for k in tqdm(range(0, mf.nlay), desc='Processing Layer'):
    array_dict = {
        "IBOUND": mf.bas6.ibound.array[k],
        "top":    mf.modelgrid.top,
        "bot":    mf.modelgrid.botm[k],
        "thick":  mf.modelgrid.cell_thickness[k],
        "Kh":     mf.upw.hk.array[k],
        "Kv":     mf.upw.vka.array[k],
        "Ss":     mf.upw.ss.array[k],
        "Sy":     mf.upw.sy.array[k],
        'aniso': mf.upw.hk.array[k] / mf.upw.vka.array[k],
    }

    # Write out a temporary shapefile
    temp_name = 'grid_temp.shp'
    fp.export.shapefile_utils.write_grid_shapefile(
        path=shp_dir / temp_name,
        mg=mf.modelgrid,
        array_dict=array_dict,
        nan_val=np.nan,
        crs="EPSG:26910"
    )

    fname = f"mf_grid_{pst_run_name}_layer{k + 1}.shp"
    gdf = gpd.read_file(shp_dir / temp_name).set_crs(crs='EPSG:26910', allow_override=True)
    gdf = gdf[gdf.IBOUND != 0]        # keep only IBOUND>0
    gdf.to_file(shp_dir / fname)


# Re-Read MODFLOW grid shapefile we just wrote for layer 1
fname = f"mf_grid_{pst_run_name}_layer{1}.shp"
grid = gpd.read_file(shp_dir / fname)

# Read in SFR file - just the reach properties
sfr = pd.read_csv(sfr_file, sep='\\s+', skiprows=3, names=sfr_cols, nrows=1835)

# Merge
sfr_shp = grid.merge(sfr, 'inner', on=['row','column'])

# Write
sfr_shp.to_file(sfr_shapefile, driver='ESRI Shapefile')

# -------------------------------------------------------------------------------------------------------------------- #
# What if we just plotted it _all_ in python?

# Setup
grid_active = grid[grid['IBOUND'] == 1]
boundary = grid_active.geometry.union_all()
props   = ['Kh', 'Kv', 'Ss', 'Sy', 'aniso', 'sbk']
titles  = [
    'Horizontal Hydraulic\nConductivity (m/d)',
    'Vertical Hydraulic\nConductivity (m/d)',
    'Specific Storage\n(1/m)',
    'Specific Yield\n(–)',
    'Anisotropy\n(Kh / Kv)',
    'Streambed Conductance\n(m/d)'
]

fig, axes = plt.subplots(2, 3, figsize=(8.5, 11))
axes = axes.flatten()

for ax, prop, title in zip(axes, props, titles):
    # pick the correct GeoDataFrame
    gdf = grid_active if prop!='sbk' else sfr_shp
    data = gdf[prop]

    # build a Jenks classifier
    nj = mapclassify.NaturalBreaks(y=data, k=5)

    # true bin boundaries: [min, b1, b2, ..., b5]
    bins = [data.min()] + list(nj.bins)

    # make human-friendly labels
    labels = []
    for lo, hi in zip(bins[:-1], bins[1:]):
        if prop=='Ss':
            labels.append(f"{lo:.1e} – {hi:.1e}")
        else:
            labels.append(f"{lo:.2f} – {hi:.2f}")

    # plot with user‐defined breaks
    gdf.plot(
        column=prop,
        cmap='viridis',
        scheme='UserDefined',
        classification_kwds={'bins': nj.bins},
        legend=True,
        ax=ax,
        legend_kwds={
            'loc': 'lower left',
            'fontsize': 8,
        }
    )

    # grab & replace the legend labels
    leg = ax.get_legend()
    for txt, lab in zip(leg.get_texts(), labels):
        txt.set_text(lab)
    leg.set_frame_on(False)
    leg.get_frame().set_facecolor('none')

    # overlay boundary on the sbk map
    if prop=='sbk':
        gpd.GeoSeries([boundary], crs=grid_active.crs).plot(
            ax=ax, facecolor='silver', edgecolor='lightgrey', linewidth=0.5, zorder=0)
    ax.set_title(title, fontsize=12, y=0.95, fontfamily='Bahnschrift')
    ax.set_axis_off()

plt.tight_layout()
plt.savefig(plt_dir / 'SVIHM_Calibrated_Properties.png', dpi=300)
plt.show()