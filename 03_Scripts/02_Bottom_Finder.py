import matplotlib
matplotlib.use('TkAgg')

import numpy as np
import pandas as pd
import geopandas as gpd
import flopy
from tqdm import tqdm
from pathlib import Path
from matplotlib import pyplot as plt
from matplotlib.colors import LinearSegmentedColormap

import os
os.chdir("./03_Scripts/")

import sys
sys.path.append('./')
from aem_plot.utils import df2rectangles, plot_slice_rect, plot_doi, plot_wl
from aem_read import read_xyz, aem_wide2long, calc_line_geometry

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

data_dir = Path('../01_Data/')
shp_dir = data_dir / 'shapefiles'
plot_dir = Path('../04_Plots/bottom_finder/')
out_dir = Path('../05_Outputs')
mod_dir = data_dir / '../02_Models'

if not plot_dir.exists():
    plot_dir.mkdir()

# Files
line_bot_file = data_dir / 'bottom_scraper.in'
aem_sharp_file = data_dir / 'SCI_Sharp_10_West_I01_MOD_inv.xyz'
aem_litho_file = data_dir / 'AEM_WELL_LITHOLOGY_csv_WO2_20220710_CVHM.csv'
aem_tprobs_file = mod_dir / 'AEM2Texture' / 'AEM_TextureProbs.dat'
aem_cf_file = data_dir / 'West_Scott_CF_ctg.xyz'

# Shapefiles
aem_sharp_sv_file = shp_dir / 'aem_sv_Sharp_I01_MOD_inv_UTM10N_idwwl.shp'
aem_hqwells_file = shp_dir / 'aem_sv_HQ_LithologyWells_UTM10N.shp'
aem_lines_file = shp_dir / 'aem_sv_FlownLines_UTM10N_split.shp'
sv_model_domain_file = shp_dir / 'Model_Domain_20180222.shp'

window_size = 5
min_points = 3
bedrock_thk_cutoff = 75

use_mf_top_bot = False

# Models
base_dir = mod_dir / 'SVIHM_MF_orig'

# -------------------------------------------------------------------------------------------------------------------- #
# Functions/Classes
# -------------------------------------------------------------------------------------------------------------------- #

def get_mltex_by_elev(df, elev):
    closest = (df['MID_ELEV'] - elev).abs().idxmin()
    point = df.loc[closest,'POINT']
    tex = df.loc[closest, tex_classes].idxmax()
    return point, tex

def get_pid_by_elev(df, elev):
    closest = (df['BOT_ELEV'] - elev).abs().idxmin()
    return df.loc[closest,'POINT']

def get_mltex_by_point(df, point):
    pidx = df[df['POINT'] == point].index[0]
    return df.loc[pidx, tex_classes].idxmax()

def nearest_cell(x, mf):
    #print(geometry.values.to_numpy())
    inter = flopy.utils.GridIntersect(mf.modelgrid).intersect(x.geometry)[0][0]
    return inter

def mf_thickness(x, mf):
    bot = mf.dis.getbotm(k=x['layer'])[x['row'], x['col']]
    if x['layer']==0:
        top = mf.dis.gettop()[x['row'], x['col']]
    else:
        top = mf.dis.getbotm(k=(x['layer']-1))[x['row'], x['col']]
    return bot, top - bot

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in data
print('Reading Data...')
line_bot = pd.read_csv(line_bot_file)
aem_wide = read_xyz(aem_sharp_file, 26, x_col='UTMX', y_col='UTMY', delim_whitespace=True)
tprobs = pd.read_csv(aem_tprobs_file, sep='\\s+')
tex_classes = tprobs.columns[7:].tolist()
#litho = pd.read_csv(aem_litho_file)

# Read in CF file (has Ramboll est. bottoms)
aem_cf = pd.read_csv(aem_cf_file, sep='\\s+')
cf_bot = aem_cf.groupby('ModIndex')['IntvEnd'].max().reset_index()

# Read in shapefiles
aem_shp = gpd.read_file(aem_sharp_sv_file)
aem_line_shp = gpd.read_file(aem_lines_file)
aem_hqwells_shp = gpd.read_file(aem_hqwells_file)
aem_hqwells_shp.set_index('WELLINFOID', inplace=True)
svihm_domain = gpd.read_file(sv_model_domain_file)

# Subset AEM data to SV using shapefile
aem_wide = aem_wide[aem_wide['LINE_NO'].isin(aem_shp['LINE_NO'])]

# Add in cf bottoms
aem_wide = aem_wide.merge(cf_bot, how='left', left_on='FID', right_on='ModIndex')
aem_wide['bot_cf'] = aem_wide['ELEVATION'] - aem_wide['IntvEnd']

#-- Split line no & fid in rholog
tprobs[['LINE_NO','FID']] = tprobs['Line'].str.split('_', expand=True)
tprobs['LINE_NO'] = tprobs['LINE_NO'].map(int)
tprobs['FID'] = tprobs['FID'].map(int)

print('Loading MODFLOW Model...')
mf_org = flopy.modflow.Modflow.load(base_dir / 'SVIHM.nam', load_only=['dis'], version='mfnwt')
mf_org.modelgrid.set_coord_info(xoff=499977, yoff=4571330)

#-- Get MODFLOW cells for every AEM point location, add to aem_wide
# ALSO Add in SUBLINE_NO column (recategorized LINE_NO on main Scott River Stem) from aem_shp
aem_shp['row_col'] = aem_shp.apply(nearest_cell, mf=mf_org, axis=1)
aem_wide = aem_wide.merge(aem_shp[['LINE_NO','FID','SUBLINE_NO','row_col']], on=['LINE_NO','FID'])
aem_wide[['row','col']] = aem_wide['row_col'].to_list()
layer_bots = mf_org.dis.getbotm()
aem_wide['bot1'] = [layer_bots[0, row, col] for row, col in aem_wide[['row', 'col']].values]
aem_wide['bot2'] = [layer_bots[1, row, col] for row, col in aem_wide[['row', 'col']].values]

# Redo distances due to SUBLINE_NO splits
aem_wide = aem_wide.groupby('SUBLINE_NO', group_keys=False).apply(calc_line_geometry, 'UTMX', 'UTMY')

#-- Make AEM long version (every row is a pixel)
print('Combining & Longifying Datasets...')
aem_long = aem_wide2long(aem_wide,
                         id_col_prefixes=['RHO_I', 'RHO_I_STD', 'SIGMA_I', 'DEP_TOP',
                                          'DEP_BOT', 'THK', 'THK_STD', 'DEP_BOT_STD'],
                         line_col='LINE_NO')

#-- Merge in data from rholog
aem_long = aem_long.merge(tprobs[['LINE_NO','FID','Depth'] + tex_classes],
                          how='left',
                          left_on=['LINE_NO','FID','DEP_BOT'],
                          right_on=['LINE_NO','FID','Depth'])

#-- Add a bottom & Midpoint elevation column
aem_long['BOT_ELEV'] = aem_long['ELEVATION'] - aem_long['DEP_BOT']
aem_long['MID_ELEV'] = aem_long['BOT_ELEV'] + aem_long['THK']

#-- There's no bottom for point 30 at each point (lowest pixel), so drop those values
aem_long = aem_long.dropna(subset='BOT_ELEV')


# -------------------------------------------------------------------------------------------------------------------- #

print('Scraping the Bottom...')

bottoms = {'SUBLINE_NO': [], 'FID': [], 'BOT_EST_POINT': []}

# lne = 100201
# lne_df = aem_long[aem_long.SUBLINE_NO == lne]
# fj_sub = lne_df[lne_df.FID == int(lne_df.FID.mean())]

for (lne, lne_df) in aem_long.groupby('SUBLINE_NO'):
    print(lne)
    bot_texs = ['Very_Coarse']
    # Get settings for this line
    lne_opt = line_bot[line_bot.LINE_NO==lne]
    if lne_opt.BOTTOM.iloc[0] == 'FINE':
        bot_texs.append('Fine')
    # Loop over points getting initial estimates
    for idx, df in lne_df.groupby('FID'):

        #eoi = fj_sub['ELEVATION'].iloc[0] - fj_sub['DOI_CONSERVATIVE'].iloc[0]  # min elev of investigation
        doi_pid = get_pid_by_elev(df, df['ELEVATION'].iloc[0] - df['DOI_CONSERVATIVE'].iloc[0])

        # Get bottom start
        if lne_opt.INITIAL.iloc[0] == "SRT":
            eoi = df['bot_cf'].iloc[0]
            if np.isnan(eoi):
                # Let's use the BOT then...
                print(f'No SRT bot for {lne} {df.FID.iloc[0]}, using MF bot as init')
                eoi = df['bot2'].iloc[0]
        elif lne_opt.INITIAL.iloc[0] == "BOT":
            eoi = df['bot2'].iloc[0]
        elif lne_opt.INITIAL.iloc[0] == "NONE":
            bottoms['SUBLINE_NO'].append(lne)
            bottoms['FID'].append(idx)
            bottoms['BOT_EST_POINT'].append(np.nan)
            continue
        else:
            raise ValueError("Invalid Line Initial Option")

        # Skip if no texture data (== outside model)
        if np.isnan(df['Mixed_Coarse']).all():
            bot_est_point = np.nan
        else:
            # Check texture of eoi
            pid, ptex = get_mltex_by_elev(df, eoi)

            # Ignoring direction now - let's just try to get out of bottom textures
            if ptex in bot_texs:
                # Is it very coarse for a long distance, suggesting bedrock?
                qpid = get_pid_by_elev(df, df.loc[df.POINT == pid, 'MID_ELEV'] - bedrock_thk_cutoff)
                if df.loc[df[df['POINT'] == pid].index[0]:df[df['POINT'] == qpid].index[0], tex_classes].idxmax(axis=1).isin(bot_texs).all():
                    # Then I suppose we're going up...
                    while ptex in bot_texs:
                        pid = pid - 1
                        ptex = get_mltex_by_point(df, pid)
                        if pid == df.POINT.min():
                            # Huh, bottom textures to the top
                            print(f'Reached Surface... FID={df["FID"].iloc[0]}')
                            break
                else:
                    # There's hope of finding a bottom
                    while ptex not in bot_texs:
                        pid = pid + 1
                        ptex = get_mltex_by_point(df, pid)
                        if pid == df.POINT.max():
                            print(f'Reach Bottom... FID={df["FID"].iloc[0]}')
                            break
                    pid = pid - 1  # Previous line was last "aquifer"-type unit
            else:
                # There's hope of finding a bottom
                while ptex not in bot_texs:
                    pid = pid + 1
                    ptex = get_mltex_by_point(df, pid)
                    if pid == df.POINT.max():
                        print(f'Reach Bottom... FID={df["FID"].iloc[0]}')
                        break
                pid = pid - 1  # Previous line was last "aquifer"-type unit
            # PID is now the first different texture elevation
            bot_est_point = np.nan
            if 1 < pid <= doi_pid:
                bot_est_point = df.loc[df['POINT'] == pid, 'BOT_ELEV'].iloc[0]
        # Append results to the dictionary
        bottoms['SUBLINE_NO'].append(lne)
        bottoms['FID'].append(idx)
        bottoms['BOT_EST_POINT'].append(bot_est_point)

# Convert dictionary to DataFrame
bottoms_df = pd.DataFrame(bottoms)

# Moving average for each line
bottoms_df['BOT_EST_LNE'] = bottoms_df.groupby('SUBLINE_NO')['BOT_EST_POINT'].transform(lambda x: x.rolling(window=window_size, min_periods=min_points, center=True).mean())

# Move back into aem_long
#aem_long = aem_long.merge(bottoms_df, on=['LINE_NO', 'FID'], how='left')

# Instead, move useful stuff from aem_long into bottoms
bottoms_df = bottoms_df.merge(aem_long[['LINE_NO','UTMX','UTMY','SUBLINE_NO','FID','ELEVATION','LINE_DIST','LINE_WIDTH']].drop_duplicates(), on=['SUBLINE_NO', 'FID'], how='left')

print('Bottom Scraped.')

# -------------------------------------------------------------------------------------------------------------------- #
# Do some plotting

#-- Get Rectangles
aem_long['AEMRect'] = df2rectangles(aem_long, x_col='LINE_DIST', y_col='BOT_ELEV',
                               xthk_col='LINE_WIDTH', ythk_col='THK')

print('Starting Plotting...')
#plt.style.use('seaborn-v0_8-dark')
fig, axd = plt.subplot_mosaic([['p1', 'map'], ['p2', 'map'], ['p3', 'map'], ['p4', 'map'], ['p5', 'map'], ['p6', 'map']],
                              gridspec_kw={'width_ratios': [6, 2]},
                              constrained_layout=True, figsize=(14, 10))

#-- Setup map
lmap = svihm_domain.plot(color='lightgray', ax=axd['map'])
lmap = aem_line_shp.plot(color='darkgray', ax=axd['map'])
lmap.set_axis_off()
prob_cmaps = ['Blues', 'Greens', 'Oranges', 'Purples', 'Reds']

# lne = 100701
# fj_sub = aem_long[aem_long.SUBLINE_NO == lne]

# Last minute rename of Tex Classes
tex_classes_use = ['Fine-grained', 'Mixed Fine', 'Sand', 'Mixed Coarse', 'Very Coarse']

#-- Loop over lines plotting
for lne, df in tqdm(aem_long.groupby('SUBLINE_NO'), desc='Plotting Line: '):
    botdf = bottoms_df[bottoms_df['SUBLINE_NO']==lne]
    #ylim = (fj_sub.BOT_ELEV.min(), fj_sub.ELEVATION.max())
    if use_mf_top_bot:
        ylim = (df.bot2.min(), df.ELEVATION.max())
    else:
        ylim = (botdf['BOT_EST_POINT'].min()-10, df.ELEVATION.max())
        if np.isnan(ylim[0]):
            ylim = (df.ELEVATION.min(), df.ELEVATION.max())
            print(f'No bottom elevations for {lne} :(')
    line_max = df.LINE_DIST.iloc[df.LINE_DIST.abs().argmax()]
    line_min = df.LINE_DIST.iloc[df.LINE_DIST.abs().argmin()]
    line_max += 0.05 * np.sign(line_max)
    line_min -= 0.05 * np.sign(line_min)

    ln_list = []
    cb_list = []

    #-- Rho
    ln1, cb = plot_slice_rect(fig, axd['p1'], rect=df.AEMRect, values=df.RHO_I, cmap='turbo',
                    title=f'Flight Line: {lne}',
                    xlim=(line_min, line_max),
                    ylim=ylim,
                    ylabel='Elevation (m)',
                    colorbar_label='Resistivity (ohm-m)', hide_xticks=True)
    cb_list.append(cb)
    #plot_doi(axd['p1'], fj_sub.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_STANDARD', 'ELEVATION', fmt='k--')
    plot_doi(axd['p1'], df.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_CONSERVATIVE', 'ELEVATION', fmt='k:')
    plot_wl(axd['p1'], botdf, 'LINE_DIST', 'BOT_EST_POINT', None, fmt='r:', width_col='LINE_WIDTH', center=True)
    plot_wl(axd['p1'], botdf, 'LINE_DIST', 'BOT_EST_LNE', None, fmt='r--', width_col='LINE_WIDTH', center=True)
    # plot_wl(axd['p1'], fj_sub, 'LINE_DIST', 'bot_cf', None, fmt='m--', width_col='LINE_WIDTH', center=True)
    # plot_wl(axd['p1'], fj_sub, 'LINE_DIST', 'bot1', None, fmt='g--', width_col='LINE_WIDTH', center=True)
    plot_wl(axd['p1'], df, 'LINE_DIST', 'bot2', None, fmt='g--', width_col='LINE_WIDTH', center=True)

    #-- Loop over Textures
    for i, tex in enumerate(tex_classes):
        ln, cb = plot_slice_rect(fig, axd[f'p{i+2}'], rect=df.AEMRect, values=df[tex], cmap=prob_cmaps[i],
                        xlim=(line_min, line_max),
                        ylim=ylim,
                        ylabel='Elevation (m)',
                        colorbar_label=f'{tex_classes_use[i]} Prob.', hide_xticks=True, clim=(0,1))
        cb_list.append(cb)
        #plot_doi(axd[f'p{i+2}'], fj_sub.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_STANDARD', 'ELEVATION', fmt='k--')
        plot_doi(axd[f'p{i+2}'], df.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_CONSERVATIVE', 'ELEVATION', fmt='k:')
        plot_wl(axd[f'p{i+2}'], botdf, 'LINE_DIST', 'BOT_EST_POINT', None, fmt='r:', width_col='LINE_WIDTH', center=True)
        plot_wl(axd[f'p{i+2}'], botdf, 'LINE_DIST', 'BOT_EST_LNE', None, fmt='r--', width_col='LINE_WIDTH', center=True)
        # plot_wl(axd[f'p{i+2}'], fj_sub, 'LINE_DIST', 'bot_cf', None, fmt='m--', width_col='LINE_WIDTH', center=True)
        # plot_wl(axd[f'p{i+2}'], fj_sub, 'LINE_DIST', 'bot1', None, fmt='g--', width_col='LINE_WIDTH', center=True)
        plot_wl(axd[f'p{i+2}'], df, 'LINE_DIST', 'bot2', None, fmt='g--', width_col='LINE_WIDTH', center=True)

    #-- Show line on map
    aem_line_shp[aem_line_shp['SUBLINE_NO']==lne].plot(color='black', ax=axd['map'])
    spoint = aem_shp.loc[(aem_shp.SUBLINE_NO==lne) & (aem_shp.FID==df.loc[df.index[0],'FID'])]
    mappoint = axd['map'].plot(spoint.geometry.x, spoint.geometry.y, 'o', color='black')[0]

    #-- Save
    if use_mf_top_bot:
        fig.savefig(plot_dir / f'AEMLines_Bottom_winsize{window_size}_minpoints{min_points}_{len(tex_classes)}cat_{lne}_MF_TOPBOT.png', dpi=300)
    else:
        fig.savefig(plot_dir / f'AEMLines_Bottom_winsize{window_size}_minpoints{min_points}_{len(tex_classes)}cat_{lne}_MFlay_newbot.png', dpi=300)

    #-- Clear out
    for cb in cb_list:
        cb.remove()
        aem_line_shp.plot(color='darkgray', ax=axd['map'])
    mappoint.remove()

print('Done.')

# -------------------------------------------------------------------------------------------------------------------- #
# Save bottoms as shapefile

# Create a GeoDataFrame
bottoms_gdf = gpd.GeoDataFrame(
    bottoms_df,
    geometry=gpd.points_from_xy(bottoms_df['UTMX'], bottoms_df['UTMY']),
    crs="EPSG:3310"
)

bottoms_gdf['BOT_DEPTH'] = bottoms_gdf['ELEVATION'] - bottoms_gdf['BOT_EST_LNE']

# Export the GeoDataFrame to a shapefile
output_shapefile = out_dir / 'aem_bottoms.shp'
bottoms_gdf.to_file(output_shapefile, driver='ESRI Shapefile')

print(f'Shapefile saved to {output_shapefile}')

# -------------------------------------------------------------------------------------------------------------------- #
