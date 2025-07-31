'''
Script to evaluate/plot local sensitivities as reported by PEST.
Some code from https://github.com/gmdsi/GMDSI_notebooks/blob/main/tutorials/part1_11_local_and_global_sensitivity/freyberg_1_local_sensitivity.ipynb
Thanks Jeremy, Rui, Michael, Brioch, and Mike (and the rest of the pyemu contributors)
'''

import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyemu
import re
from pathlib import Path
from collections import defaultdict

# import os
# os.chdir('../')

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

pest_dir = Path('./06_PEST_Results/')
out_dir = Path('./05_Outputs/')
plt_dir = Path('./04_Plots/')

run_name = 'svihm_t2p12_iter6'

# -------------------------------------------------------------------------------------------------------------------- #
# Functions/Classes
# -------------------------------------------------------------------------------------------------------------------- #

def get_sens_dfs(sens_file, drop_reg=False, verbose=True):
    group_rows = []
    total_rows = []

    current_group = None
    capture = False

    with open(sens_file, 'r') as f:
        for line in f:
            # Detect group header
            m = re.search(r'Composite sensitivities for observation group "(.+)"', line)
            if m:
                current_group = m.group(1)
                capture = True
                for i in range(0,3):
                    next(f)
                continue
            # Detect start of TOTAL block
            if re.search(r'Composite sensitivities for all observations', line, re.IGNORECASE):
                current_group = "__TOTAL__"
                capture = True
                for i in range(0,3):
                    next(f)
                continue
            # Stop capture on blank line
            if capture and not line.strip():
                capture = False
                current_group = None
                continue
            # Grab parameter lines while capturing
            if capture and line.strip():
                parts = line.split()
                par, pgrp, val, css = parts
                row = (par, current_group, float(css))
                if current_group == "__TOTAL__":
                    total_rows.append(row)
                else:
                    group_rows.append(row)

    # Build DataFrames
    total_df = (pd.DataFrame(total_rows, columns=["parnme", "obsgroup", "css_total"])
                .drop(columns="obsgroup")
                .set_index('parnme', drop=True)
                .sort_values("parnme"))
    group_df = (pd.DataFrame(group_rows, columns=["parnme", "obsgroup", "css"])
                .pivot(index="parnme", columns="obsgroup", values="css")
                .sort_values("parnme"))
    if drop_reg:
        group_df = group_df.loc[:,~group_df.columns.str.startswith('regul')]
    if verbose:
        print(f'Read composite sensitivity values for {group_df.shape[0]} parameters and {group_df.shape[1]} groups')

    return total_df, group_df

# -------------------------------------------------------------------------------------------------------------------- #
# SEN Plot
# -------------------------------------------------------------------------------------------------------------------- #

pst = pyemu.Pst(str(pest_dir / f"{run_name}.pst"))
jco = pyemu.Jco.from_binary(str(pest_dir / f"{run_name}.jco"))
#jco_df = jco.to_dataframe()

# instantiate schur
sc = pyemu.Schur(jco=jco, pst=pst)

# calculate the parameter CSS
css_df = sc.get_par_css_dataframe()
css_df = css_df.sort_index()

# # Plot local sens
# css_df.sort_values(by='pest_css', ascending=False).plot(kind='bar', figsize=(13,3))
# plt.yscale('log')
#
# # Correlation between parameters
covar = pyemu.Cov(sc.xtqx.x, names=sc.xtqx.row_names)
R = covar.to_pearson()
# plt.imshow(R.df(), interpolation='nearest', cmap='viridis')
# plt.colorbar()
#
# # Specifically, what is kminvc1_m correlated to?
# cpar = 'kminvc1_m'
# R.df().loc[cpar][np.abs(R.df().loc[cpar])>.5]
# R_plot = R.as_2d.copy()
# R_plot[np.abs(R_plot)<0.5] = np.nan
# plt.imshow(R_plot, interpolation='nearest', cmap='viridis')
# plt.colorbar()

# Read in sens
_, sen_group = get_sens_dfs(pest_dir / f"{run_name}.sen", drop_reg=True)

# Divide by row total (total sen to group), multiply by total sen of parameter
sen_group_norm = sen_group.div(sen_group.sum(axis=1), axis=0).mul(np.sqrt(css_df['pest_css']), axis=0)

# Order
plot_df = sen_group_norm.loc[css_df.sort_values("pest_css", ascending=False).index]

# Build a dict mapping each group → list of its columns
group_cols = {
             'as':        ['as_low', 'as_med', 'as_high'],
             'by':        ['by_low', 'by_med', 'by_high'],
             'fj':        ['fj_low', 'fj_med', 'fj_high'],
             'fjmonvol':  ['fjmonvol_l', 'fjmonvol_m', 'fjmonvol_h', 'fjyrlyvol'],
             'heads':     ['sv_heads', 'qv_heads','head_diffs', 'vh_diffs'],
}

# Pick a continuous colormap for each multi‑member group
cmap_map = {
    'as'        : plt.cm.Blues,
    'by'        : plt.cm.Greys,
    'fj'        : plt.cm.Reds,
    'fjmonvol'  : plt.cm.Purples,
    'heads'     : plt.cm.Greens,
}

# Generate a color for each column
col_colors = {}
for grp, cols in group_cols.items():
    if grp in cmap_map and len(cols) > 1:
        cmap = cmap_map[grp]
        shades = cmap(np.linspace(0.35, 0.95, len(cols)))
        for col, shade in zip(cols, shades):
            col_colors[col] = shade

# Build a list of colors in the same order as your DataFrame’s columns
color_list = [col_colors[c] for c in plot_df.columns]

ordered_cols = []

for grp, cols in group_cols.items():          # keeps your hand‑made order
    ordered_cols += [c for c in cols if c in plot_df.columns]

# add any columns that weren’t listed in group_cols (qv_heads, etc.)
ordered_cols += [c for c in plot_df.columns if c not in ordered_cols]

# reindex the DF to that order
plot_df = plot_df[ordered_cols]

# also build the colour list in that SAME order
color_list = [col_colors[c] for c in ordered_cols]

# Plot!
ax = plot_df.plot(
    kind='bar',
    stacked=True,
    figsize=(18, 8),
    width=0.9,
    color=color_list
)
ax.set_ylabel("Sq.Rt. Composite Sensitivity (stacked by obs group)")
ax.set_xlabel("Parameter Name")
ax.legend(ncol=3, fontsize=8, frameon=False)
plt.tight_layout()
plt.savefig(plt_dir / f'{run_name}_sens.png')

# -------------------------------------------------------------------------------------------------------------------- #
# What's the deal with our coarse parameters having a similar Kh?

coarse_pars = ['kminsc1_m', 'kminmc1_m', 'kminvc1_m']
coarse_cov = R.df().loc[coarse_pars,coarse_pars]
