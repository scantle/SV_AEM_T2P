import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
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
mod_dir = Path('//BEHEMOTH/Users/lelan/Documents/ModelRuns/SVIHM/2025_t2p_calibration/10_init_iter4/SVIHM/MODFLOW')

# Texture files
tex_files = {
    'FINE': 't2p_FINE.csv',
    'MIXED_FINE': 't2p_MIXED_FINE.csv',
    'SAND': 't2p_SAND.csv',
    'MIXED_COARSE': 't2p_MIXED_COARSE.csv',
    'VERY_COARSE': 't2p_VERY_COARSE.csv'
}

sfr_cols = ['layer', 'row', 'column', 'segment', 'reach', 'length', 'elevation', 'slope', 'thick', 'sbk']

# Model dis info
xoff, yoff = 499977, 4571330
angrot = 0.0  # degrees clockwise
nam_file = 'svihm.nam'

# Files
sfr_file = mod_dir / 'svihm.sfr'
sv_model_shp_file = shp_dir / 'grid_properties_rep.shp'

# Outputs
pst_run_name = 'calib_10_iter4'
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