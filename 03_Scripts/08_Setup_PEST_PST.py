import os

import numpy as np
import pandas as pd
import pyemu
import flopy
from tqdm import tqdm
from pathlib import Path

# Import local functions
import sys
sys.path.append('./03_Scripts')
from HOB_weight_processing import wt_dict, hob_to_df, calculate_hob_weights
from GAG_weight_processing import cv_stream_weights, fj_impossible_dates
from T2P_funcs import t2p_par2par, t2p_par2par_frompar

#----------------------------------------------------------------------------------------------------------------------#
# Setup
#----------------------------------------------------------------------------------------------------------------------#

# Directories
orig_dir = os.getcwd()
data_dir = Path('./01_Data/')
model_dir = Path('./02_Models/SVIHM_MF/')
out_dir = Path('./05_Outputs/PEST/')
svihm_dir = Path('../SVIHM/')  # External to project, local SVIHM Git repo
svihm_ref_dir = svihm_dir / 'SVIHM_Input_Files/reference_data_for_plots/'
out_dir.mkdir(exist_ok=True)

# File Locations
fj_file = data_dir / 'FJ (USGS 11519500) Daily Flow, 1990-10-01_2025-02-28.csv'
as_file = svihm_ref_dir / 'Scott River Above Serpa Lane.txt'
by_file = svihm_ref_dir / 'Scott River Below Youngs Dam.txt'
tex_dists_file = Path('./05_Outputs/lognorm_dist_clustered.par')
tex_scale_range_file = Path('./05_Outputs/lognorm_dist_clustered_scale_ranges.dat')

# Model Info
model_name = 'SVIHM'
xoff = 499977
yoff = 4571330
origin_date = pd.to_datetime('1990-9-30')

# Out
pst_file = 'svihm_t2p11.pst'

vertical_well_pairs = [
    ('ST201', 'ST201_2'),
    ('ST786', 'ST786_2')
]

# Conversions
cfs_to_m3d = 0.3048**3 * 86400

#----------------------------------------------------------------------------------------------------------------------#
# Classes/Functions
#----------------------------------------------------------------------------------------------------------------------#

def write_ts_ins_file(obs_df, origin_date, skip_rows, ins_filename, column_str=None, markers=None, date_col="Date"):
    """
    Writes a PEST instruction (ins) file for streamflow observations with optimized skipping.

    Parameters:
    obs_df (pd.DataFrame): DataFrame containing columns ['Date', 'obsnme']
    origin_date (pd.Timestamp): Model start date
    ins_filename (str or Path): Name of the output instruction file
    """
    obs_df = obs_df.sort_values(by=date_col).reset_index(drop=True)

    # Open file
    with open(ins_filename, 'w') as f:
        f.write("pif @\n")  # PEST instruction file header

        current_date = origin_date + pd.DateOffset(days=1)
        for i, row in obs_df.iterrows():
            days_skipped = (row[date_col] - current_date).days
            if i == 0:
                days_skipped += skip_rows
            if column_str is not None:
                f.write(f"l{days_skipped + 1} [{row['obsnme']}]{column_str}\n")
            elif markers is not None:
                f.write(f"l{days_skipped + 1} {markers} !{row['obsnme']}!\n")
            else:
                raise ValueError("Must pass either markers or column_str")
            current_date = row[date_col] + pd.Timedelta(days=1)

    print(f"Instruction file written: {ins_filename}")

#----------------------------------------------------------------------------------------------------------------------#

def write_static_ins_file(obs_df, ins_filename, markers):
    # Open file
    with open(ins_filename, 'w') as f:
        f.write("pif @\n")  # PEST instruction file header
        for i, row in obs_df.iterrows():
            f.write(f"l1 {markers} !{row['obsnme']}!\n")
    print(f"Instruction file written: {ins_filename}")


#----------------------------------------------------------------------------------------------------------------------#

def write_hobs_ins_file(hobs_df, ins_filename):
    # Open file
    with open(ins_filename, 'w') as f:
        f.write("pif @\n")  # PEST instruction file header

        for i, row in hobs_df.iterrows():
            if i == 0:
                f.write(f"l2 [{row['obsnme']}]1:21\n")
            else:
                f.write(f"l1 [{row['obsnme']}]1:21\n")
    print(f"Instruction file written: {ins_filename}")

#----------------------------------------------------------------------------------------------------------------------#

def zero_weight_dates(df, date_list, date_col="Date", verbose=True):
    for i, (strt, end) in enumerate(date_list):
        df.loc[(df[date_col]>=strt) & (df[date_col]<=end), 'wt'] = 0.0
        print(f'Zeroing dates {i+1}/{len(date_list)}:')
        print(df.loc[(df[date_col]>=strt) & (df[date_col]<=end), [date_col, 'wt']])
    return df

#----------------------------------------------------------------------------------------------------------------------#

def balance_metagroup_weights(obs_df, metagroup_col="metagroup", target_weights=None):
    """
    Adjusts observation weights so that each metagroup contributes equally to total weight
    (or according to user-defined target weights), while preserving intra-group weight ratios.

    Parameters:
    obs_df (pd.DataFrame): Dataframe with 'wt' (weights) and metagroup assignments.
    metagroup_col (str): Column name in obs_df that contains metagroup assignments.
    target_weights (dict, optional): A dictionary {metagroup_name: target_weight_sum}.
                                     If None, metagroups get equal total weight.

    Returns:
    pd.DataFrame: Updated obs_df with adjusted weights.
    """

    # Compute total weight per metagroup
    group_totals = obs_df.groupby(metagroup_col)["wt"].sum()
    total_weight = group_totals.sum()

    # Determine target weights
    if target_weights is None:
        # Default: Distribute total weight equally across metagroups
        num_groups = len(group_totals)
        target_weights = {group: total_weight / num_groups for group in group_totals.index}

    # Validate user-defined target weights
    elif not isinstance(target_weights, dict) or set(target_weights.keys()) != set(group_totals.index):
        raise ValueError("target_weights must be a dictionary with keys matching unique metagroup names.")

    # Step 3: Compute scaling factors for each metagroup
    scaling_factors = {
        group: target_weights[group] / group_totals[group] if group_totals[group] > 0 else 1.0
        for group in group_totals.index
    }

    # Step 4: Apply scaling factors while preserving intra-group weight ratios
    obs_df["wt"] = obs_df.apply(lambda row: row["wt"] * scaling_factors[row[metagroup_col]], axis=1)

    return obs_df

#----------------------------------------------------------------------------------------------------------------------#
# Parameters
#----------------------------------------------------------------------------------------------------------------------#
# Parameter lists are [low, high, default, group]
# unless tied ['tied', tied_parameter, group] or fixed ['fixed', value, group]

# NOTE! Default values are updated using a calibrated PEST par file down below.

# Texture2Par Parameters
t2p_parameters = {
    'KminFF1'   : [1e-7, 10.0, 5.0, 'K'],
    'KminMF1_M' : [1.0, 100.0, 10.0, 'K_M'],
    'KminSC1_M' : [1.0, 100.0, 2.0, 'K_M'],
    'KminMC1_M' : [1.0, 100.0, 1.0, 'K_M'],
    'KminVC1_M' : [1.0, 100.0, 10.0, 'K_M'],
    'AnisoVC1'  : [1.0, 10.0, 5.0, 'Aniso'],
    'AnisoMC1_M': [1.0, 20.0, 5.0, 'Aniso_M'],
    'AnisoSC1_M': [1.0, 20.0,  1.0, 'Aniso_M'],
    'AnisoMF1_M': [1.0, 20.0,  1.0, 'Aniso_M'],
    'AnisoFF1_M': [1.0, 20.0,  1.0, 'Aniso_M'],
    'SsFF1'     : [1e-5, 1e-2, 1e-4, 'Ss'],
    'SsMF1_M'   : [0.1, 1.0, 0.45, 'Ss_M'],
    'SsSC1_M'   : [0.1, 1.0, 0.45, 'Ss_M'],
    'SsMC1_M'   : [0.1, 1.0, 0.45, 'Ss_M'],
    'SsVC1_M'   : [0.1, 1.0, 0.45, 'Ss_M'],
    'SySC1'     : [0.20, 0.40, 0.25, 'Sy'],
    'SyMF1_M'   : [0.1, 1.5, 0.75, 'Sy_M'],
    'SyFF1_M'   : [0.01, 1.0, 0.75, 'Sy_M'],
    'SyMC1_M'   : [0.1, 1.5, 0.75, 'Sy_M'],
    'SyVC1_M'   : [0.1, 1.5, 0.75, 'Sy_M'],
    'KHp1'      : [0.75, 1.0, 0.93, 'PLP'],
    'KVp1'      : [-1.0, -0.5, -0.62, 'PLP'],
}

# Replicates par2par setup
t2p_reality_check = t2p_par2par(t2p_parameters)
print(t2p_reality_check)

# Add SFR2PAR segment multipliers
sfr_parameters = {
    'sbcm01': [0.1, 10, 1.0, 'mSFR'],
    'sbcm02': ['tied','sbcm01', 'mSFR'],
    'sbcm03': ['tied','sbcm01', 'mSFR'],
    'sbcm04': ['tied','sbcm01', 'mSFR'],
    'sbcm05': [0.1, 10, 1.0, 'mSFR'],
    'sbcm06': [0.1, 10, 1.0, 'mSFR'],
    'sbcm07': ['tied','sbcm06', 'mSFR'],
    'sbcm08': ['tied','sbcm06', 'mSFR'],
    'sbcm09': [0.1, 10, 1.0, 'mSFR'],
    'sbcm10': ['tied','sbcm09', 'mSFR'],
    'sbcm11': [0.1, 10, 1.0, 'mSFR'],
    'sbcm12': [0.1, 10, 1.0, 'mSFR'],
    'sbcm13': [0.1, 10, 1.0, 'mSFR'],
    'sbcm14': [0.1, 10, 1.0, 'mSFR'],
    'sbcm15': [0.1, 10, 1.0, 'mSFR'],
    'sbcm16': ['tied','sbcm15', 'mSFR'],
    'sbcm17': [0.1, 10, 1.0, 'mSFR'],
    'sbcm18': [0.1, 10, 1.0, 'mSFR'],
    'sbcm19': [0.1, 10, 1.0, 'mSFR'],
    'sbcm20': [0.1, 10, 1.0, 'mSFR'],
    'sbcm21': [0.1, 10, 1.0, 'mSFR'],
    'sbcm22': [0.1, 10, 1.0, 'mSFR'],
    'sbcm23': [0.1, 10, 1.0, 'mSFR'],
    'sbcm24': [0.1, 10, 1.0, 'mSFR'],
    'sbcm25': [0.1, 10, 1.0, 'mSFR'],
    'sbcm26': [0.1, 10, 1.0, 'mSFR'],
    'sbcm27': [0.1, 10, 1.0, 'mSFR'],
    'sbcm28': ['tied','sbcm27', 'mSFR'],
    'sbcm29': ['tied','sbcm27', 'mSFR'],
    'sbcm30': [0.1, 10, 1.0, 'mSFR'],
}

# Read in texture distribution parameters
tex_dist_params = pd.read_table(tex_dists_file, sep="\\s+", skiprows=1)
tex_scale_ranges = pd.read_table(tex_scale_range_file, sep="\\s+")  # 95% confidence intervals

aem2texture_parameters = {
    'ScaleFF': [tex_scale_ranges.loc[0,'ScaleMin'], tex_scale_ranges.loc[0,'ScaleMax'], tex_dist_params.loc[0,'Scale'], 'aemscale'],
    'ScaleMF': [tex_scale_ranges.loc[1,'ScaleMin'], tex_scale_ranges.loc[1,'ScaleMax'], tex_dist_params.loc[1,'Scale'], 'aemscale'],
    'ScaleSC': [tex_scale_ranges.loc[2,'ScaleMin'], tex_scale_ranges.loc[2,'ScaleMax'], tex_dist_params.loc[2,'Scale'], 'aemscale'],
    'ScaleMC': [tex_scale_ranges.loc[3,'ScaleMin'], tex_scale_ranges.loc[3,'ScaleMax'], tex_dist_params.loc[3,'Scale'], 'aemscale'],
    'ScaleVC': [tex_scale_ranges.loc[4,'ScaleMin'], tex_scale_ranges.loc[4,'ScaleMax'], tex_dist_params.loc[4,'Scale'], 'aemscale'],
}

#-- MODFLOW PVL MFR parameters
pvl_parameters = {
    'MFR5' : [1.0, 1000, 2.00E+00, 'MFR'],
    'MFR6' : [1.0, 1000, 6.00E+01, 'MFR'],
    'MFR7' : [1.0, 1000, 7.00E+01, 'MFR'],
    'MFR8' : [1.0, 1000, 1.50E+02, 'MFR'],
    'MFR9' : [1.0, 1000, 5.00E+00, 'MFR'],
    'MFR10': [1.0, 1000, 2.00E+01, 'MFR'],
    'MFR11': [1.0, 1000, 9.00E+01, 'MFR'],
}

#-- Assemble
pest_parameters = t2p_parameters | sfr_parameters | aem2texture_parameters | pvl_parameters

#----------------------------------------------------------------------------------------------------------------------#
# Observations
#----------------------------------------------------------------------------------------------------------------------#

# Change back to project directory
os.chdir(orig_dir)

# Load model & create spatial reference
gwf = flopy.modflow.Modflow.load((model_name + '.nam'), version='mfnwt', load_only=['dis','bas6'], model_ws=model_dir)
sr = pyemu.helpers.SpatialReference(delr=gwf.dis.delr.array, delc=gwf.dis.delc.array, xll=xoff, yll=yoff, epsg=26910)
end_date = origin_date + pd.DateOffset(months=gwf.nper)

#----------------------------------------------------------------------------------------------------------------------#
# Setup Head Observations

hob_file = model_dir / "svihm.hob"
print('Reading Hobs... (slow)')
hob = flopy.modflow.ModflowHob.load(hob_file, model=gwf)
print('Hobs read.')
hobs_df = hob_to_df(hob, origin_date)
hobs_df = calculate_hob_weights(hobs_df, wt_dict, gwf.get_package('BAS6'))
hobs_df['obsgnme'] = 'SV_HEADS'
hobs_df.loc[hobs_df.wellid.str.startswith('QV'), 'obsgnme'] = 'QV_HEADS'

# Lower contribution for a few wells
hobs_df.loc[hobs_df['obsnme'].str.startswith('SCV_11'),'wt'] = 0.5
hobs_df.loc[hobs_df['obsnme'].str.startswith('28P001M'),'wt'] = 0.5
hobs_df.loc[hobs_df['obsnme'].str.startswith('SCV_5'),'wt'] = 0.5

#----------------------------------------------------------------------------------------------------------------------#
# Setup Head Difference Observations
hobs_diff = hobs_df[['obsnme', 'obsval','wt']].copy()
hobs_diff['diff'] = hobs_df.groupby('wellid')['obsval'].diff()
hobs_diff = hobs_diff.dropna(subset=['diff']).reset_index(drop=True)
hobs_diff['obsnme'] = hobs_diff['obsnme'] + '_D'
hobs_diff['obsval'] = hobs_diff['diff']
hobs_diff['obsgnme'] = 'HEAD_DIFFS'
hobs_diff['wt'] = hobs_diff['wt'] * 1.25

# Keep G31 for head diffs
hobs_diff.loc[hobs_diff['obsnme'].str.startswith('G31'),'wt'] = 1.25

#----------------------------------------------------------------------------------------------------------------------#
# Setup Vertical Head Difference Observations

vhdiff_list = []
for top_well, bottom_well in vertical_well_pairs:
    # Subset each well
    top_df = hobs_df[hobs_df['wellid'] == top_well][['date', 'obsval']].rename(columns={'obsval': 'sim_top'})
    bot_df = hobs_df[hobs_df['wellid'] == bottom_well][['date', 'obsval']].rename(columns={'obsval': 'sim_bot'})

    # Merge on time index
    merged = pd.merge(top_df, bot_df, on='date', how='inner')
    merged['obsval'] = merged['sim_top'] - merged['sim_bot']

    merged['obsnme'] = [f"{top_well}_VD.{i+1}" for i in merged['date'].index]

    # Append
    vhdiff_list.append(merged[['obsnme', 'obsval']])

vhdiff_df = pd.concat(vhdiff_list, ignore_index=True)
vhdiff_df['obsgnme'] = 'VH_DIFFS'
vhdiff_df['wt'] = 25  # Trying to give vdiffs some real representation in the objective function

#----------------------------------------------------------------------------------------------------------------------#
# Setup Streamflow Observations

# Read in streamflow files
str_fj = pd.read_csv(fj_file, parse_dates=['Date'])
str_as = pd.read_table(as_file, sep="\\s+", parse_dates=['Date'])
str_by = pd.read_table(by_file, sep="\\s+", parse_dates=['Date'])

# Convert, combine
str_fj = str_fj[str_fj['Date'] > origin_date]
str_fj = str_fj[str_fj['Date'] <= end_date]
str_fj['obsnme'] = [f"FJ_{i + 1}" for i in str_fj.index]
str_fj['obsval'] = str_fj['Flow'] * cfs_to_m3d
str_as['obsnme'] = [f"AS_{i + 1}" for i in str_as.index]
str_as['obsval'] = str_as['Streamflow_m3/day']
str_by['obsnme'] = [f"BY_{i + 1}" for i in str_by.index]
str_by['obsval'] = str_by['Streamflow_m3/day']

# log transform with a small offset to avoid 0
str_fj['obsval'] = np.log(str_fj['obsval'] + 0.1)
str_as['obsval'] = np.log(str_as['obsval'] + 0.1)
str_by['obsval'] = np.log(str_by['obsval'] + 0.1)

# Set some variables to reuse
qts = [0.40, 0.80]
cvs = [0.1, 0.2, 0.5]

# Get weights, groups
str_fj['obsgnme'], str_fj['wt'] = cv_stream_weights(str_fj, qts, cvs, 'fj')
str_as['obsgnme'], str_as['wt'] = cv_stream_weights(str_as, qts, cvs, 'as')
str_by['obsgnme'], str_by['wt'] = cv_stream_weights(str_by, qts, cvs, 'by')

# Re-weight, log version
# str_fj.loc[str_fj['obsgnme']=='fj_low', 'wt'] = 1/np.sqrt(np.log(1+0.10**2))
# str_as.loc[str_as['obsgnme']=='as_low', 'wt'] = 1/np.sqrt(np.log(1+0.12**2))
# str_by.loc[str_by['obsgnme']=='by_low', 'wt'] = 1/np.sqrt(np.log(1+0.12**2))
# str_fj.loc[str_fj['obsgnme']=='fj_med', 'wt'] = 1/np.sqrt(np.log(1+0.15**2))
# str_as.loc[str_as['obsgnme']=='as_med', 'wt'] = 1/np.sqrt(np.log(1+0.15**2))
# str_by.loc[str_by['obsgnme']=='by_med', 'wt'] = 1/np.sqrt(np.log(1+0.15**2))
# str_fj.loc[str_fj['obsgnme']=='fj_high','wt'] = 1/np.sqrt(np.log(1+0.20**2))
# str_as.loc[str_as['obsgnme']=='as_high','wt'] = 1/np.sqrt(np.log(1+0.20**2))
# str_by.loc[str_by['obsgnme']=='by_high','wt'] = 1/np.sqrt(np.log(1+0.20**2))

# Zero some impossible dates
str_fj = zero_weight_dates(str_fj, fj_impossible_dates)

# Adjust weights where necessary
# Tolley et al. (2019) found some of the smaller stream obs near 0 created Inf weights
# They assigned the weight of low flow of the non-USGS gauges to be the median FJ low flow weight
str_as.loc[str_as['obsgnme']=='as_low','wt'] = str_fj.loc[str_fj['obsgnme']=='fj_low','wt'].median()
str_by.loc[str_by['obsgnme']=='by_low','wt'] = str_fj.loc[str_fj['obsgnme']=='fj_low','wt'].median()

# Create a maximum value for FJ low flows at 10 cfs
str_fj.loc[str_fj.obsval <= 10.0, 'wt'] = 1 / ((10 * cvs[0]) ** 2)

#----------------------------------------------------------------------------------------------------------------------#
# Setup FJ volume observations
str_fj_full = str_fj.set_index('Date')
str_fj_full = str_fj_full.resample('D').asfreq()
str_fj_full['obsval'] = str_fj_full['obsval'].interpolate(method='pchip')

# Note: there were no missing obs through 9/30/2024

fj_wyearly = str_fj_full.resample('YS-OCT').sum()
fj_wyearly['obsnme'] = [f"FJVOL_WY_{d.year+1}" for d in fj_wyearly.index]
fj_monthly = str_fj_full.resample('ME').sum()
fj_monthly['obsnme'] = [f"FJVOL_{d.year}_{d.month:02d}" for d in fj_monthly.index]
fj_wyearly['obsgnme'] = 'FJYRLYVOL'
fj_wyearly['wt'] = 1e-8

thresholds = fj_monthly['obsval'].quantile(qts).values
fj_monthly['obsgnme'] = 'FJMONVOL_H'
fj_monthly.loc[fj_monthly['obsval'] <= thresholds[1], 'obsgnme'] = 'FJMONVOL_M'
fj_monthly.loc[fj_monthly['obsval'] <= thresholds[0], 'obsgnme'] = 'FJMONVOL_L'
fj_monthly.loc[fj_monthly['obsgnme']=='FJMONVOL_H', 'wt'] = 1e-10
fj_monthly.loc[fj_monthly['obsgnme']=='FJMONVOL_M', 'wt'] = 1e-7
fj_monthly.loc[fj_monthly['obsgnme']=='FJMONVOL_L', 'wt'] = 1e-6

#fj_monthly['obsnme'] = [f"FJVOL_{d.year}_{d.month:02d}" for d in fj_monthly.index]
fj_vol = pd.concat([fj_wyearly[['obsnme', 'obsgnme', 'obsval', 'wt']],
                    fj_monthly[['obsnme', 'obsgnme', 'obsval', 'wt']]]).reset_index(drop=True)

#----------------------------------------------------------------------------------------------------------------------#
# Combine observations
pst_obs_cols = ['obsgnme', 'obsnme', 'obsval', 'wt']

obs_df = pd.concat([str_fj   [pst_obs_cols],
                    str_as   [pst_obs_cols],
                    str_by   [pst_obs_cols],
                    fj_vol   [pst_obs_cols],
                    hobs_df  [pst_obs_cols],
                    hobs_diff[pst_obs_cols],
                    vhdiff_df[pst_obs_cols]]).reset_index(drop=True)

#----------------------------------------------------------------------------------------------------------------------#
# Adjust weights

# Define metagroup mappings
metagroup_mapping = {
    # Head observations
    "SV_HEADS": "HEADS",
    "QV_HEADS": "HEADS",
    "HEAD_DIFFS": "HEAD_DIFFS",
    "VH_DIFFS": "VH_DIFFS",
    "fj_low": "STREAMFLOW",
    "fj_med": "STREAMFLOW",
    "fj_high": "STREAMFLOW",
    "as_low": "STREAMFLOW",
    "as_med": "STREAMFLOW",
    "as_high": "STREAMFLOW",
    "by_low": "STREAMFLOW",
    "by_med": "STREAMFLOW",
    "by_high": "STREAMFLOW",
    "FJMONVOL_L": "STREAMVOL",
    "FJMONVOL_M": "STREAMVOL",
    "FJMONVOL_H": "STREAMVOL",
    "FJYRLYVOL":  "STREAMVOL"
}

# Assign metagroup labels
obs_df["metagroup"] = obs_df["obsgnme"].map(metagroup_mapping)

target_weight = obs_df.loc[obs_df['metagroup']=='HEADS','wt'].sum()
obs_df.groupby('obsgnme').wt.sum()
obs_df.groupby('metagroup').wt.sum()

obs_df = balance_metagroup_weights(obs_df,  target_weights={'HEADS':target_weight,
                                                            'HEAD_DIFFS':target_weight,
                                                            'VH_DIFFS':target_weight,
                                                            'STREAMFLOW':target_weight/1.5e5,
                                                           'STREAMVOL':target_weight/1.1e8})

#----------------------------------------------------------------------------------------------------------------------#
# Setup PEST
#----------------------------------------------------------------------------------------------------------------------#

# Switch to folder PEST setup is in
os.chdir('C:/Projects/SVIHM/2025_PEST_t2pcalib/Setup/')

# Write INS files
write_ts_ins_file(str_fj, origin_date, 2, 'Streamflow_FJ_SVIHM.ins', column_str='86:99')
write_ts_ins_file(str_as, origin_date, 2, 'Streamflow_AS_SVIHM.ins', column_str='86:99')
write_ts_ins_file(str_by, origin_date, 2, 'Streamflow_BY_SVIHM.ins', column_str='86:99')
# write_ts_ins_file(str_fj, origin_date,0, 'Streamflow_FJ_SVIHM_MidptFlow_LOG.ins', markers='w')
# write_ts_ins_file(str_as, origin_date,0, 'Streamflow_AS_SVIHM_MidptFlow_LOG.ins', markers='w')
# write_ts_ins_file(str_by, origin_date,0, 'Streamflow_BY_SVIHM_MidptFlow_LOG.ins', markers='w')
write_static_ins_file(fj_vol, 'Streamflow_FJ_SVIHM_VOL.ins', markers='w')
write_static_ins_file(hobs_diff, 'HobData_SVIHM_DIFF.ins', markers='w')
write_static_ins_file(vhdiff_df, 'HobData_SVIHM_VDIFF.ins', markers='w')
write_hobs_ins_file(hobs_df, 'HobData_SVIHM.ins')

#----------------------------------------------------------------------------------------------------------------------#
# Let pyemu detect all observations & parameters, it serves as a check on everything we're doing
# We can then update the groups, weights, values, etc
pst = pyemu.Pst.from_io_files(tpl_files=['t2p_par2par.tpl',
                                         'sfr2par.tpl',
                                         'AEM2Texture.tpl',
                                         'SVIHM_PVAL.tpl'
                                         ],
                              in_files=[Path('./SVIHM/t2p_par2par.in'),
                                        Path('./SVIHM/sfr2par.in'),
                                        Path('./SVIHM/preproc/AEM2Texture.in'),
                                        Path('./SVIHM/MODFLOW/SVIHM.pvl')
                                        ],
                              ins_files=['Streamflow_FJ_SVIHM.ins',
                                         'Streamflow_AS_SVIHM.ins',
                                         'Streamflow_BY_SVIHM.ins',
                                         'Streamflow_FJ_SVIHM_VOL.ins',
                                         'HobData_SVIHM.ins',
                                         'HobData_SVIHM_DIFF.ins',
                                         'HobData_SVIHM_VDIFF.ins',
                                         ],
                              out_files=[Path('./SVIHM/MODFLOW/Streamflow_FJ_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/Streamflow_AS_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/Streamflow_BY_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/Streamflow_FJ_SVIHM_VOL.out'),
                                         Path('./SVIHM/MODFLOW/HobData_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/HobData_SVIHM_DIFF.out'),
                                         Path('./SVIHM/MODFLOW/HobData_SVIHM_VDIFF.out'),
                                         ],
                              pst_filename=pst_file)

# Parameter Check
pest_param_names = {k.lower(): v for k, v in pest_parameters.items()}  # pyemu converts to lowercase
detected_param_names = set(pst.parameter_data.index)
missing_params = set(pest_param_names.keys()) - detected_param_names
extra_params = detected_param_names - set(pest_param_names.keys())

if missing_params:
    print(f"⚠ Warning: {len(missing_params)} parameters from pest_parameters were NOT detected by pyemu!")
    print("Missing parameters:", missing_params)

if extra_params:
    print(f"Note: {len(extra_params)} extra parameters detected by pyemu that were NOT in pest_parameters!")
    print("Extra parameters:", extra_params)

# Update parameter defaults, bounds, groups, connections
for param, value in pest_parameters.items():
    group = value[-1]  # Last item in the list is the group
    pst.parameter_data.loc[param.lower(), 'pargp'] = group

    if value[0] == 'tied':
        pst.parameter_data.loc[param.lower(), 'partrans'] = 'tied'
        pst.parameter_data.loc[param.lower(), 'partied'] = value[1].lower()
        pst.parameter_data.loc[param.lower(), 'parval1'] = pest_parameters[value[1]][2]
        pst.parameter_data.loc[param.lower(), 'parlbnd'] = pest_parameters[value[1]][0]
        pst.parameter_data.loc[param.lower(), 'parubnd'] = pest_parameters[value[1]][1]
    elif value[0] == 'fixed':
        pst.parameter_data.loc[param.lower(), 'parval1'] = value[1]
        pst.parameter_data.loc[param.lower(), 'partrans'] = 'fixed'
        pst.parameter_data.loc[param.lower(), 'parlbnd'] = 0.0
        pst.parameter_data.loc[param.lower(), 'parubnd'] = 1.0
    else:
        pst.parameter_data.loc[param.lower(), 'parval1'] = value[2]
        pst.parameter_data.loc[param.lower(), 'parlbnd'] = value[0]
        pst.parameter_data.loc[param.lower(), 'parubnd'] = value[1]

# Adjust derinc for groups
# pst.rectify_pgroups()
# pst.parameter_groups.loc['K','derinc'] = 0.05
# pst.parameter_groups.loc['K_M','derinc'] = 0.02
# pst.parameter_groups.loc['Sy','derinc'] = 0.02
# pst.parameter_groups.loc['PLP','derinc'] = 0.02
pst.parameter_groups.loc['MFR','derinc'] = 0.02
pst.parameter_groups.loc['mSFR','derinc'] = 0.02

# Adjust scale, transformation of power-law parameters
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"parval1"] *= 100       # parval now read in
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"parlbnd"] *= 100
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"parubnd"] *= 100
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"scale"]    = 1/100
#pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"partrans"] = 'none'
# Make kvp positive ( have to switch low and high!)
pst.parameter_data.loc['kvp1',['parval1','parlbnd','parubnd','scale']] *= -1      # parval now read in
#pst.parameter_data.loc['kvp1',['parlbnd','parubnd','scale']] *= -1
hi_temp = pst.parameter_data.loc['kvp1','parlbnd']
pst.parameter_data.loc['kvp1','parlbnd'] = pst.parameter_data.loc['kvp1','parubnd']
pst.parameter_data.loc['kvp1','parubnd'] = hi_temp

# And Specific Yield
pst.parameter_data.loc[pst.parameter_data['parnme']=='sysc1',"parval1"] *= 100    # parval now read in
pst.parameter_data.loc[pst.parameter_data['parnme']=='sysc1',"parlbnd"] *= 100
pst.parameter_data.loc[pst.parameter_data['parnme']=='sysc1',"parubnd"] *= 100
pst.parameter_data.loc[pst.parameter_data['parnme']=='sysc1',"scale"]    = 1/100

# Observation Check
obs_df['obsnme'] = obs_df['obsnme'].str.lower()   # pyemu converts to lowercase
detected_obs_names = set(pst.observation_data.index)
missing_obs = set(obs_df['obsnme']) - detected_obs_names
extra_obs = detected_obs_names - set(obs_df['obsnme'])

if missing_obs:
    print(f"⚠ Warning: {len(missing_obs)} observations in obs_df were NOT detected by pyemu!")
    print("Missing observations:", missing_obs)

if extra_obs:
    print(f"Note: {len(extra_obs)} extra observations detected by pyemu that were NOT in obs_df!")
    print("Extra observations:", extra_obs)

# Update observation values, weights, and groups
obs_df = obs_df.set_index('obsnme')
pst.observation_data.loc[obs_df.index, ["obsval", "weight", "obgnme"]] = obs_df[["obsval", "wt", "obsgnme"]].to_numpy()

# Add regularization
pyemu.helpers.zero_order_tikhonov(pst, parbounds=True, par_groups=['aemscale', 'Sy', 'PLP', 'mSFR', 'MFR'])  # fancy pyemu helper
pst.prior_information['weight'] *= 750
#pst.prior_information.loc[pst.prior_information.index.str.startswith('mfr'),'weight'] *= 10
pst.prior_information.loc[pst.prior_information.pilbl=='sysc1','weight'] *= 0.75

# Update starting values from parfile
calpar = pd.read_table(Path('../RunRecords/10/svihm_t2p10_iter2.par'), sep="\\s+", skiprows=1, index_col=0, names=['par','parval1','scale','offset'])
#calpar['parval1'] = calpar['parval1'] * calpar['scale'] + calpar['offset']
print(t2p_par2par_frompar(calpar))
pst.parameter_data.loc[calpar.index, "parval1"] = calpar['parval1']

# overwrite a few
# pst.parameter_data.loc[pst.parameter_data.index=='khp1','parval1'] = 93.0
# #pst.parameter_data.loc[pst.parameter_data.index=='kvp1','parval1'] = 62.0
# pst.parameter_data.loc[pst.parameter_data.pargp=='mSFR','parval1'] = 1.0
# for key in aem2texture_parameters.keys():
#     pst.parameter_data.loc[pst.parameter_data.index == key.lower(), 'parval1'] = aem2texture_parameters[key][2]

pst.model_command = [str(Path("forward_run.bat"))]

# Set some final options, save the PST file
pst.control_data.noptmax = 0
pst.svd_data.maxsing = pst.npar_adj
#pst.svd_data.eigthresh
pst.control_data.numcom = 1
pst.control_data.jacfile = np.int32(0)
pst.control_data.messfile = np.int32(0)
pst.control_data.numlam = 10
pst.reg_data.phimlim = pst.nnz_obs - pst.npar_adj    # comment out to check out best fit
pst.reg_data.phimaccept = 1.1 * pst.reg_data.phimlim # same
#pst.reg_data.fracphim
pst.reg_data.wfinit = 1.0
pst.reg_data.wffac = 1.3

# Sort to make pretty
pst.parameter_data = pst.parameter_data.drop('parnme', axis=1)
pst.parameter_data = pst.parameter_data.sort_values(["pargp", "parnme"])
pst.parameter_data['parnme'] = pst.parameter_data.index
pst.observation_data = pst.observation_data.sort_values(["obgnme", "obsnme"])

pst.write(pst_file, version=1)