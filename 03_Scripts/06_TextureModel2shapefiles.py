import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import pandas as pd
import geopandas as gpd
from pathlib import Path

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Directories
data_dir = Path('01_Data/')
shp_dir = data_dir / 'shapefiles'
out_dir = Path('05_Outputs')
tex_file_dir = Path('02_Models/Texture2Par_onlytexture_2D/')

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

# Outputs
grid_layer1_shapefile = shp_dir / "texture_model_layer1.shp"
grid_layer2_shapefile = shp_dir / "texture_model_layer2.shp"
points_layer1_shapefile = shp_dir / "texture_model_totals_layer1.shp"
points_layer2_shapefile = shp_dir / "texture_model_totals_layer2.shp"

# -------------------------------------------------------------------------------------------------------------------- #
# Classes/Functions
# -------------------------------------------------------------------------------------------------------------------- #

def convert_to_2d(geom):
    if geom.has_z:
        return gpd.GeoSeries([gpd.GeoSeries([geom]).apply(lambda g: g.__class__(tuple(coord[:2]) for coord in g.exterior.coords))[0]])[0]
    return geom

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read MODFLOW grid shapefile
grid = gpd.read_file(sv_model_shp_file)
grid['geometry'] = grid['geometry'].apply(convert_to_2d)

# Separate Layers
grid_lay1 = grid[grid['Layer']==1]
grid_lay2 = grid[grid['Layer']==2]

# Read and sum textures for each layer
texture_data = {key: pd.read_csv(tex_file_dir / fname, na_values=-999) for key, fname in tex_files.items()}

# Process each texture file
layer1_df = None
layer2_df = None
for tex_name, df in texture_data.items():
    df = df.rename(columns={df.columns[0]: 'Row', df.columns[1]: 'Col'})

    # Sum texture values across all textures for each layer
    temp_lay1 = df[['Row', 'Col', df.columns[4]]].rename(columns={df.columns[4]: tex_name})  # Layer 1
    temp_lay2 = df[['Row', 'Col', df.columns[5]]].rename(columns={df.columns[5]: tex_name})  # Layer 2

    # Merge with existing dataframes
    if layer1_df is None:
        layer1_df = temp_lay1
        layer2_df = temp_lay2
    else:
        layer1_df = layer1_df.merge(temp_lay1, on=['Row', 'Col'], how='inner')
        layer2_df = layer2_df.merge(temp_lay2, on=['Row', 'Col'], how='inner')

layer1_df.fillna(0, inplace=True)
layer2_df.fillna(0, inplace=True)

# Compute total texture sum for each cell
layer1_df['Total'] = layer1_df.iloc[:, 2:].sum(axis=1)
layer2_df['Total'] = layer2_df.iloc[:, 2:].sum(axis=1)

# Determine dominant texture for each cell
layer1_df['Dominant'] = layer1_df.iloc[:, 2:-1].idxmax(axis=1)
layer2_df['Dominant'] = layer2_df.iloc[:, 2:-1].idxmax(axis=1)

# Handle ties: If multiple textures are dominant, mark as 'MIXED'
for layer_df in [layer1_df, layer2_df]:
    max_values = layer_df.iloc[:, 2:-2].max(axis=1)
    second_max_values = layer_df.iloc[:, 2:-2].apply(lambda x: sorted(x, reverse=True)[1], axis=1)
    layer_df.loc[max_values == second_max_values, 'Dominant'] = 'TIE'

# Merge with grid shapefile
texmod_layer1 = grid_lay1.merge(layer1_df, left_on=['Row', 'Column'], right_on=['Row', 'Col'], how='left')
texmod_layer2 = grid_lay2.merge(layer2_df, left_on=['Row', 'Column'], right_on=['Row', 'Col'], how='left')

# Filter to only include active model cells (IBound == 1.0)
texmod_layer1 = texmod_layer1[texmod_layer1['IBound'] == 1.0]
texmod_layer2 = texmod_layer2[texmod_layer2['IBound'] == 1.0]

# -------------------------------------------------------------------------------------------------------------------- #
# Write Shapefiles

# Export grid shapefiles (polygons with dominant texture)
texmod_layer1 = texmod_layer1.set_crs(epsg=26910)
texmod_layer2 = texmod_layer2.set_crs(epsg=26910)
texmod_layer1.to_file(grid_layer1_shapefile, driver='ESRI Shapefile', )
texmod_layer2.to_file(grid_layer2_shapefile, driver='ESRI Shapefile')

# Convert grid centroids to points for texture percentage export
texmod_layer1 = texmod_layer1.copy()
texmod_layer2 = texmod_layer2.copy()
texmod_layer1['geometry'] = texmod_layer1.geometry.centroid
texmod_layer2['geometry'] = texmod_layer2.geometry.centroid

# Export point shapefiles (with texture percentages and total sum)
texmod_layer1.to_file(points_layer1_shapefile, driver='ESRI Shapefile')
texmod_layer2.to_file(points_layer2_shapefile, driver='ESRI Shapefile')

# Print confirmation messages
print(f"Exported polygon shapefile: {grid_layer1_shapefile}")
print(f"Exported polygon shapefile: {grid_layer2_shapefile}")
print(f"Exported point shapefile: {points_layer1_shapefile}")
print(f"Exported point shapefile: {points_layer2_shapefile}")
# -------------------------------------------------------------------------------------------------------------------- #
