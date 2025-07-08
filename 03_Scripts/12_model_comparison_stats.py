import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pandas.api.types import CategoricalDtype
from pathlib import Path
from sklearn.metrics import r2_score
from hydroeval import evaluator, nse, kge, kgeprime, rmse, pbias

# Import local functions
import sys
sys.path.append('./03_Scripts')
from GAG_weight_processing import cv_stream_weights

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

prev_dir  = Path('./02_Models/SVIHM_MF/')
calb_dir = Path('./02_Models/SVIHM_MF_Calibrated/')
data_dir = Path('./01_Data/')
out_dir = Path('./05_Outputs/')
plt_dir = Path('./04_Plots/')

wells_zero_weighted = ['ST655','ST202','G40','L32','N15','G31','ST186','QV02', 'QV07', 'QV08','J15', 'N17', 'Q21',
                       'QV01', 'QV02', 'SCV_7', 'ST028', 'ST178', 'ST790', 'X7']

fj_file = data_dir / 'FJ (USGS 11519500) Daily Flow, 1990-10-01_2025-02-28.csv'
as_file = data_dir / 'Scott River Above Serpa Lane.txt'
by_file = data_dir / 'Scott River Below Youngs Dam.txt'

# Model Info
origin_date = pd.to_datetime('1990-9-30')
end_date = pd.to_datetime('2024-9-30')

cfs_to_m3day = (0.3048)**3 * (86400)

# -------------------------------------------------------------------------------------------------------------------- #
# Functions/Classes
# -------------------------------------------------------------------------------------------------------------------- #


# -------------------------------------------------------------------------------------------------------------------- #
# HOB comparison
# -------------------------------------------------------------------------------------------------------------------- #

# Read HOB files
hob_cols = ['sim','obs','id']
hob_prev = pd.read_table(prev_dir / 'MODFLOW' / 'HobData_SVIHM.dat', sep='\s+', skiprows=1, names=hob_cols)
hob_calb = pd.read_table(calb_dir / 'MODFLOW' / 'HobData_SVIHM.dat', sep='\s+', skiprows=1, names=hob_cols)

# Combine for ease
hob_comp = pd.merge(hob_prev, hob_calb, on=['id','obs'], suffixes=['_prev','_calb'])

# Get well name
hob_comp['name'] = hob_comp['id'].str.split('.').str[0]

# Remove wells that weren't used in calibration
hob_comp = hob_comp[~hob_comp['name'].isin(wells_zero_weighted)]

# Calculate main stats
hob_summary_rows = {}
for tag in ['prev', 'calb']:
    res = hob_comp['obs'] - hob_comp[f'sim_{tag}']
    hob_summary_rows[tag] = {
        'ME'  : res.mean(),
        'MAE' : res.abs().mean(),
        'RMSE': np.sqrt((res ** 2).mean()),
        'R2'  : r2_score(hob_comp['obs'], hob_comp[f'sim_{tag}'])
    }
hob_summary_df = pd.DataFrame.from_dict(hob_summary_rows, orient='index')
hob_summary_df.to_csv(out_dir / 'HOB_summary_stats.csv')

# -------------------------------------------------------------------------------------------------------------------- #
# FJ comparison
# -------------------------------------------------------------------------------------------------------------------- #

# Read obs
fj_obs = pd.read_csv(fj_file, parse_dates=[2])
fj_obs = fj_obs[fj_obs['Date'] <= end_date]
as_obs = pd.read_csv(as_file, sep='\s+', parse_dates=[0], skiprows=1, names=['Date','Time','cfs','obs'])
by_obs = pd.read_csv(by_file, sep='\s+', parse_dates=[0], skiprows=1, names=['Date','Time','cfs','obs'])

# Read sim
st_cols = ['Time','Stage','Flow','Depth','Width','sim','Precip','ET','Runoff','Conductance','HeadDiff','HydGrad']
fj_prev = pd.read_table(prev_dir / 'MODFLOW/Streamflow_FJ_SVIHM.dat', sep='\s+', skiprows=1, names=st_cols)
fj_calb = pd.read_table(calb_dir / 'MODFLOW/Streamflow_FJ_SVIHM.dat', sep='\s+', skiprows=1, names=st_cols)
as_prev = pd.read_table(prev_dir / 'MODFLOW/Streamflow_AS_SVIHM.dat', sep='\s+', skiprows=1, names=st_cols)
as_calb = pd.read_table(calb_dir / 'MODFLOW/Streamflow_AS_SVIHM.dat', sep='\s+', skiprows=1, names=st_cols)
by_prev = pd.read_table(prev_dir / 'MODFLOW/Streamflow_BY_SVIHM.dat', sep='\s+', skiprows=1, names=st_cols)
by_calb = pd.read_table(calb_dir / 'MODFLOW/Streamflow_BY_SVIHM.dat', sep='\s+', skiprows=1, names=st_cols)

# Add dates, combine
st_cols = ['Time','sim']
fj_comb = pd.merge(fj_prev[st_cols], fj_calb[st_cols], on=['Time'], suffixes=('_prev', '_calb'))
fj_comb['Date'] = origin_date + pd.to_timedelta(fj_comb['Time'],'days')
fj_comb['obs'] = fj_obs['Flow']*cfs_to_m3day
as_comb = pd.merge(as_prev[st_cols], as_calb[st_cols], on='Time', suffixes=('_prev','_calb'))
as_comb['Date'] = origin_date + pd.to_timedelta(as_comb['Time'], 'days')
as_comb = pd.merge(as_comb, as_obs[['Date','obs']], on='Date')
by_comb = pd.merge(by_prev[st_cols], by_calb[st_cols], on='Time', suffixes=('_prev','_calb'))
by_comb['Date'] = origin_date + pd.to_timedelta(by_comb['Time'], 'days')
by_comb = pd.merge(by_comb, by_obs[['Date','obs']], on='Date')

# Calculate flow group using same method used in PEST PST file creation
qts = [0.40, 0.80]
cvs = [0.1, 0.2, 0.5]

fj_comb['group'], _ = cv_stream_weights(fj_comb, qts, cvs, 'fj', obscol='obs')

# Calculate main stats
metrics = {
    'RMSE': lambda obs, sim: evaluator(rmse, sim, obs, axis=0)[0],
    'NSE':   lambda obs, sim: evaluator(nse,   sim, obs, axis=0)[0],
    'KGE':   lambda obs, sim: evaluator(kgeprime, sim, obs, axis=0)[0][0],
    'PBIAS': lambda obs, sim: evaluator(pbias, sim, obs, axis=0)[0]
}

rows = []
for stream, comb, do_groups in [
    ('FJ', fj_comb, True),
    ('AS', as_comb, False),
    ('BY', by_comb, False),
]:
    for model in ['prev','calb']:
        # full‐period first
        obs = comb['obs']
        sim = comb[f'sim_{model}']
        vals = {name: fn(obs, sim) for name, fn in metrics.items()}
        if obs.min() <= 0.0:
            eps = comb.loc[comb['obs'] > 0, 'obs'].min() / 2
            mask = comb['obs'] > 0
            vals['LNSE'] = evaluator(nse, np.log(sim[mask]+eps), np.log(obs[mask]+eps), axis=0)[0]
        else:
            vals['LNSE'] = evaluator(nse, np.log(sim), np.log(obs), axis=0)[0]
        rows.append({
            'stream': stream,
            'model' : model,
            'period': 'All',
            **vals
        })

        # low/med/high only for FJ
        if do_groups:
            for label, grp in [('Low','fj_low'),
                               ('Medium','fj_med'),
                               ('High','fj_high')]:
                mask = comb['group'] == grp
                obs_g = comb.loc[mask, 'obs']
                sim_g = comb.loc[mask, f'sim_{model}']
                vals_g = {name: fn(obs_g, sim_g) for name, fn in metrics.items()}
                vals_g['LNSE'] = evaluator(nse, np.log(sim_g), np.log(obs_g), axis=0)[0]
                rows.append({
                    'stream': stream,
                    'model' : model,
                    'period': label,
                    **vals_g
                })

# 4) Build and save the summary table
summary_df = pd.DataFrame(rows, columns=['stream','model','period','RMSE','NSE','KGE','PBIAS','LNSE'])
# Sort nicely before saving
period_type = CategoricalDtype(categories=['All','Low','Medium','High'], ordered=True)
summary_df['period'] = summary_df['period'].astype(period_type)
summary_df = summary_df.sort_values(['stream','period','period'], ascending=[False,True,True])
summary_df.to_csv(out_dir / 'SFR_summary_stats.csv', index=False)

# -------------------------------------------------------------------------------------------------------------------- #

# Plot!
fig, ax = plt.subplots(figsize=(12, 6))
ax.plot(fj_comb['Date'], fj_comb['obs'], linestyle='-', linewidth=1.75, label='Observed', c='#6ba0a6')
ax.plot(fj_comb['Date'], fj_comb['sim_prev'], linestyle=':', linewidth=1.2, label='Previous Model', c='grey')
ax.plot(fj_comb['Date'], fj_comb['sim_calb'], linestyle='--', linewidth=1.2, label='New Model', c='black')
ax.set_yscale('log')
ax.set_xlim(pd.to_datetime('1990-10-01'), pd.to_datetime('2012-09-30'))
ax.set_xlabel('Date', fontsize=12)
ax.set_ylabel('Flow (m³/day)', fontsize=12)
#ax.set_title('Streamflow Comparison: Previous vs. New Model\n(Oct 1990 – Sep 2012)', fontsize=14)
ax.grid(True, which='both', linestyle='-', linewidth=0.2)
ax.legend(frameon=True, loc='upper right')
fig.autofmt_xdate()
plt.tight_layout()
plt.show()
plt.savefig(plt_dir / 'fj_sim_obs_hydrograph.png', dpi=300)