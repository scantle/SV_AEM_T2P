import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import pandas as pd
import geopandas as gpd
import t2py
from pathlib import Path

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

# Shapefiles
aem_hqwells_file = shp_dir / 'aem_sv_HQ_LithologyWells_UTM10N.shp'
aem_lqwells_file = shp_dir / 'aem_sv_LQ_LithologyWells_UTM10N.shp'
sv_model_domain_file = shp_dir / 'Model_Domain_20180222.shp'

# Inclusion Radius
svihm_buffer = 500   # meters

tailings_issue_logs = [18416, 18424, 18223, 18528, 18339]

# -------------------------------------------------------------------------------------------------------------------- #
# Classes/Functions
# -------------------------------------------------------------------------------------------------------------------- #

def reclassify_texture(intv):
    tex = intv['Texture']
    # Cluster 0
    if tex in ['shale','claystone']:
        tex = 'Fine'
    # Cluster 1
    elif tex in ['clay','silt','loam','top soil']:
        tex = 'Mixed_Fine'
    # Cluster 2
    elif tex in ['sand']:
        tex = 'Sand'
    # Cluster 3
    elif tex in ['gravel','rock','cobbles']:
        tex = 'Mixed_Coarse'
    # Cluster 4
    elif tex in ['boulders','sandstone','lava','lime']:
        tex = 'Very_Coarse'
    # elif tex == 'unknown':
    #     tex = -999
    else:
        print('UNKNOWN TEXTURE:', intv[['Texture','Primary_Texture_Modifier']])
    return tex

# -------------------------------------------------------------------------------------------------------------------- #

def plot_points_with_buffer(domain, buffer, points, location_col, title, output_file=None):
    """
    Plots points relative to a domain and buffer zone.

    Parameters:
        domain (GeoDataFrame): The main polygon (e.g., a study area).
        buffer (GeoSeries): The buffer zone around the domain.
        points (GeoDataFrame): Points to be plotted.
        location_col (str): Column in `points` indicating location categories (e.g., 'Inside Domain').
        title (str): Title of the plot.
        output_file (str, optional): Filepath to save the plot as an image. Defaults to None.
    """
    # Setup
    colors = {'Inside Domain': 'blue', 'Within Buffer': 'orange', 'Outside Buffer': 'red'}
    counts = points[location_col].value_counts()
    fig, ax = plt.subplots(figsize=(12, 8))
    domain.plot(ax=ax, color='none', edgecolor='black', linewidth=1, label='Domain')

    # Plot the buffer zone
    buffer.boundary.plot(ax=ax, color='green', linestyle='--', linewidth=1, label='500m Buffer')

    # Plot points by location
    for location, color in colors.items():
        subset = points[points[location_col] == location]
        count = counts.get(location, 0)  # Get count, default to 0 if location not present
        subset.plot(
            ax=ax,
            color=color,
            markersize=5,
            label=f"{location} ({count})"
        )

    # Customize the plot
    ax.set_title(title, fontsize=14)
    ax.set_xlabel('Easting (UTM)', fontsize=12)
    ax.set_ylabel('Northing (UTM)', fontsize=12)
    ax.legend()
    plt.grid()
    plt.tight_layout()

    # Save the plot if an output file is specified
    if output_file:
        plt.savefig(output_file, dpi=300)
    else:
        plt.show()

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in data
litho = pd.read_csv(aem_litho_file)

# Read in shapefiles
aem_hqwells_shp = gpd.read_file(aem_hqwells_file)
aem_hqwells_shp.set_index('WELLINFOID', inplace=True)
aem_lqwells_shp = gpd.read_file(aem_lqwells_file)
aem_lqwells_shp.set_index('WELLINFOID', inplace=True)
svihm_domain = gpd.read_file(sv_model_domain_file)

# Combine well shapefiles
litho_shp = pd.concat([aem_lqwells_shp, aem_hqwells_shp])

# Create buffer
buffer_shp = svihm_domain.buffer(svihm_buffer)

# Classify points: Inside domain, within buffer, or outside buffer
litho_shp['location'] = 'Outside Buffer'  # Default
litho_shp.loc[litho_shp.within(buffer_shp.union_all()), 'location'] = 'Within Buffer'
litho_shp.loc[litho_shp.within(svihm_domain.union_all()), 'location'] = 'Inside Domain'

# Plot!
plot_points_with_buffer(svihm_domain, buffer_shp, litho_shp,
                        'location', f'Points in SVIHM Domain & {svihm_buffer}m Buffer')

# Limit to SV buffer (points inside the domain or buffer)
litho_shp = litho_shp[litho_shp.within(buffer_shp.union_all())]

# Limit Texture data to Scott Valley buffer
litho = litho[litho.WELL_INFO_ID.isin(litho_shp.index)]

# Get XY data
litho_shp['X'] = litho_shp.geometry.x
litho_shp['Y'] = litho_shp.geometry.y
litho_shp['WELL_INFO_ID'] = litho_shp.index

# ...Copy to litho
litho = litho.merge(litho_shp[['WELL_INFO_ID','X','Y']], how='inner', on='WELL_INFO_ID')

# Apply texture revisions
litho['tex_rev'] = litho.apply(reclassify_texture, axis=1)

# Calculate the interval thickness
litho['thick'] = litho['LITH_BOT_DEPTH_m'] - litho['LITH_TOP_DEPTH_m']

# Group by reclassified texture and sum the interval thicknesses
thickness_by_texture = litho.groupby('tex_rev')['thick'].sum()

# Create histogram
plt.figure(figsize=(10, 6))
thickness_by_texture.sort_values(ascending=False).plot(kind='bar')
plt.title('Histogram of Interval Thickness by Texture', fontsize=14)
plt.xlabel('Reclassified Texture', fontsize=12)
plt.ylabel('Total Interval Thickness (m)', fontsize=12)
plt.xticks(rotation=45, ha='right')
plt.tight_layout()
plt.show()

# Adjust some tailings logs that are skewing the south tailings area "fine"
# We know this area to actually be very, very coarse
# Some of the logs mention "fractured shale" although from site visits the area modeled is large cobbles...
# More investigation needed...
litho.loc[litho.WELL_INFO_ID.isin(tailings_issue_logs), 'tex_rev'] = 'Very_Coarse'

# Setup for t2py
#litho['Name'] = litho.agg(lambda x: f"{x['WELL_INFO_ID']:g}_{x['LITH_ID']:g}", axis=1)
for tex in ['Fine','Mixed_Fine','Sand','Mixed_Coarse','Very_Coarse']:
    litho[tex] = 0.0
    litho.loc[litho['tex_rev'] == tex, tex] = 1.0
    # litho.loc[litho['tex_rev'] == -999, tex] = None
    litho.loc[litho['tex_rev'] == 'unknown', tex] = None

# Create a well log file and write it out
log = t2py.Dataset(classes=['Fine','Mixed_Fine','Sand','Mixed_Coarse','Very_Coarse'])
log.add_wells_by_df(df=litho,
                    name_col='WELL_INFO_ID',
                    zland_col='GROUND_SURFACE_ELEVATION_m',
                    depth_col='LITH_BOT_DEPTH_m', depth_top_col='LITH_TOP_DEPTH_m')
log.write_file(out_dir / 'LithoLog_5classes.dat')