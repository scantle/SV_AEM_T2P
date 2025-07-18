import matplotlib
matplotlib.use("TkAgg")

import numpy as np
import pandas as pd
import geopandas as gpd
import flopy
from tqdm import tqdm
from pathlib import Path
from matplotlib import pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.colors import to_rgb

import os
os.chdir("03_Scripts/")

import sys
sys.path.append('./')
from aem_plot.utils import df2rectangles, plot_slice_rect, plot_slice_rect_doi, plot_line_by_depth, plot_wl
from aem_read import read_xyz, aem_wide2long, calc_line_geometry

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

data_dir = Path('../01_Data/')
shp_dir = data_dir / 'shapefiles'
plot_dir = Path('../04_Plots/AEM_TextureProbs/')
out_dir = Path('../05_Outputs')
mod_dir = data_dir / '../02_Models'

if not plot_dir.exists():
    plot_dir.mkdir()

# Files
aem_sharp_file = data_dir / 'SCI_Sharp_10_West_I01_MOD_inv.xyz'
aem_litho_file = data_dir / 'AEM_WELL_LITHOLOGY_csv_WO2_20220710_CVHM.csv'
aem_tprobs_file = mod_dir / 'AEM2Texture' / 'AEM_TextureProbs.dat'

# Shapefiles
aem_sharp_sv_file = shp_dir / 'aem_sv_Sharp_I01_MOD_inv_UTM10N_idwwl.shp'
aem_hqwells_file = shp_dir / 'aem_sv_HQ_LithologyWells_UTM10N.shp'
aem_lines_file = shp_dir / 'aem_sv_FlownLines_UTM10N_split.shp'
sv_model_domain_file = shp_dir / 'Model_Domain_20180222.shp'

use_mf_top_bot = True

# Models
base_dir = mod_dir / 'SVIHM_MF' / 'MODFLOW'

cc = ['#df263e', '#e37e26', '#e3c128', '#6da14d', '#5289db']

# -------------------------------------------------------------------------------------------------------------------- #
# Functions/Classes
# -------------------------------------------------------------------------------------------------------------------- #

def nearest_cell(x, mf):
    #print(geometry.values.to_numpy())
    inter = flopy.utils.GridIntersect(mf.modelgrid).intersect(x.geometry)[0][0]
    return inter

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Read in data
print('Reading Data...')
aem_wide = read_xyz(aem_sharp_file, 26, x_col='UTMX', y_col='UTMY', delim_whitespace=True)
tprobs = pd.read_csv(aem_tprobs_file, sep='\\s+')
tex_classes = tprobs.columns[7:].tolist()
#litho = pd.read_csv(aem_litho_file)

# Read in shapefiles
aem_shp = gpd.read_file(aem_sharp_sv_file)
aem_line_shp = gpd.read_file(aem_lines_file)
aem_hqwells_shp = gpd.read_file(aem_hqwells_file)
aem_hqwells_shp.set_index('WELLINFOID', inplace=True)
svihm_domain = gpd.read_file(sv_model_domain_file)

# Subset AEM data to SV using shapefile
aem_wide = aem_wide[aem_wide['LINE_NO'].isin(aem_shp['LINE_NO'])]

# Join logs to AEM data based on nearest
# aem_hqwells_shp = aem_hqwells_shp.sjoin_nearest(aem_shp, how='inner',
#                                                 max_distance=500,
#                                                 rsuffix='aem',
#                                                 distance_col='dist')
#

print('Pre-plot calculations...')

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

#-- Add a bottom elevation column
aem_long['BOT_ELEV'] = aem_long['ELEVATION'] - aem_long['DEP_BOT']

#-- There's no bottom for point 30 at each point (lowest pixel), so drop those values
aem_long = aem_long.dropna(subset='BOT_ELEV')

#-- Get Rectangles
aem_long['AEMRect'] = df2rectangles(aem_long, x_col='LINE_DIST', y_col='BOT_ELEV',
                               xthk_col='LINE_WIDTH', ythk_col='THK')
aem_long['doi_elev'] = aem_long['ELEVATION'] - aem_long['DOI_CONSERVATIVE']

print('Starting Plotting...')
#plt.style.use('seaborn-v0_8-dark')
fig, axd = plt.subplot_mosaic([['p1', 'map'], ['p2', 'map'], ['p3', 'map'], ['p4', 'map'], ['p5', 'map'], ['p6', 'map']],
                              gridspec_kw={'width_ratios': [6, 2]},
                              constrained_layout=True, figsize=(14, 10))

#-- Setup map
lmap = svihm_domain.plot(color='lightgray', ax=axd['map'])
lmap = aem_line_shp.plot(color='darkgray', ax=axd['map'])
lmap.set_axis_off()
#prob_cmaps = ['Blues','Greens','Oranges','Purples','Reds']
prob_cmaps = [LinearSegmentedColormap.from_list(f'cmap_{c}', [(1,1,1), to_rgb(c)]) for c in cc]

# lne = 100701
# fj_sub = aem_long[aem_long.SUBLINE_NO == lne]

#-- Loop over lines plotting
for lne, df in tqdm(aem_long.groupby('SUBLINE_NO'), desc='Plotting Line: '):

    #ylim = (fj_sub.BOT_ELEV.min(), fj_sub.ELEVATION.max())
    if use_mf_top_bot:
        ylim = (df.bot2.min(), df.ELEVATION.max())
    else:
        ylim = (df.ELEVATION.min()-600, df.ELEVATION.max())
    line_max = df.LINE_DIST.iloc[df.LINE_DIST.abs().argmax()]
    line_min = df.LINE_DIST.iloc[df.LINE_DIST.abs().argmin()]
    line_max += 0.05 * np.sign(line_max)
    line_min -= 0.05 * np.sign(line_min)

    ln_list = []
    cb_list = []

    #-- Rho
    ln1, cb = plot_slice_rect_doi(fig, axd['p1'], rect=df.AEMRect, values=df.RHO_I, doi_values=df.doi_elev,
                                  cmap='turbo',
                                  title=f'Flight Line: {lne}',
                                  xlim=(line_min, line_max),
                                  ylim=ylim,
                                  clim=(1,1000),
                                  ylabel='Elevation (m)',
                                  colorbar_label='Resistivity (ohm-m)',
                                  doi_alpha=1.0,
                                  hide_xticks=True)
    cb_list.append(cb)
    #plot_doi(axd['p1'], fj_sub.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_STANDARD', 'ELEVATION', fmt='k--')
    plot_line_by_depth(axd['p1'], df.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_CONSERVATIVE', 'ELEVATION', fmt='k:')
    # plot_wl(axd['p1'], fj_sub.dropna(subset='RHO_I'), 'LINE_DIST', 'aemwlidw', 'ELEVATION', fmt='r--')
    #plot_wl(axd['p1'], fj_sub, 'LINE_DIST', 'bot1', None, fmt='k--', width_col='LINE_WIDTH', center=True)
    #plot_wl(axd['p1'], fj_sub, 'LINE_DIST', 'bot2', None, fmt='k--', width_col='LINE_WIDTH', center=True)

    #-- Loop over Textures
    for i, tex in enumerate(tex_classes):
        # ln, cb = plot_slice_rect(fig, axd[f'p{i+2}'], rect=fj_sub.AEMRect, values=fj_sub[tex], cmap=prob_cmaps[i],
        #                 xlim=(line_min, line_max),
        #                 ylim=ylim,
        #                 ylabel='Elevation (m)',
        #                 colorbar_label=f'{tex} Prob.', hide_xticks=True, clim=(0,1))
        ln, cb = plot_slice_rect_doi(fig, axd[f'p{i+2}'], rect=df.AEMRect, values=df[tex], cmap=prob_cmaps[i], doi_values=df.doi_elev,
                        xlim=(line_min, line_max),
                        ylim=ylim,
                        ylabel='Elevation (m)',
                        doi_alpha=1.0,
                        colorbar_label=f'{tex} Prob.', hide_xticks=True, clim=(0,1))
        cb_list.append(cb)
        #plot_line_by_depth(axd[f'p{i+2}'], fj_sub.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_STANDARD', 'ELEVATION', fmt='k--')
        plot_line_by_depth(axd[f'p{i+2}'], df.dropna(subset='RHO_I'), 'LINE_DIST', 'DOI_CONSERVATIVE', 'ELEVATION', fmt='k:')
        #plot_wl(axd[f'p{i+2}'], fj_sub, 'LINE_DIST', 'bot1', None, fmt='k--', width_col='LINE_WIDTH', center=True)
        #plot_wl(axd[f'p{i+2}'], fj_sub, 'LINE_DIST', 'bot2', None, fmt='k--', width_col='LINE_WIDTH', center=True)

    #-- Show line on map
    aem_line_shp[aem_line_shp['SUBLINE_NO']==lne].plot(color='black', ax=axd['map'])
    spoint = aem_shp.loc[(aem_shp.SUBLINE_NO==lne) & (aem_shp.FID==df.loc[df.index[0],'FID'])]
    mappoint = axd['map'].plot(spoint.geometry.x, spoint.geometry.y, 'o', color='black')[0]

    #-- Save
    if use_mf_top_bot:
        fig.savefig(plot_dir / f'AEMLines_TextureProbs_MFTOPBOT_{len(tex_classes)}cat_{lne}.png', dpi=300)  # bbox_inches='tight')
    else:
        fig.savefig(plot_dir / f'AEMLines_TextureProbs_{len(tex_classes)}cat_{lne}.png', dpi=300)  # bbox_inches='tight')
    #-- Clear out
    for cb in cb_list:
        cb.remove()
        aem_line_shp.plot(color='darkgray', ax=axd['map'])
    mappoint.remove()

print('Done.')
