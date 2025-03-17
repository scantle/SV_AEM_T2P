import os

import numpy as np
import pandas as pd
import pyemu
import flopy
from tqdm import tqdm
from pathlib import Path

# Import weighting tools from 08_1_HOB_weights
import sys
sys.path.append('./03_Scripts')
from HOB_weight_processing import wt_dict, hob_to_df, calculate_hob_weights
from GAG_weight_processing import cv_stream_weights
from T2P_funcs import t2p_par2par

#----------------------------------------------------------------------------------------------------------------------#
# Setup
#----------------------------------------------------------------------------------------------------------------------#

# Directories
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

# Conversions
cfs_to_m3d = 0.3048**3 * 86400

#----------------------------------------------------------------------------------------------------------------------#
# Classes/Functions
#----------------------------------------------------------------------------------------------------------------------#

def write_ts_ins_file(obs_df, origin_date, column_str, skip_rows, ins_filename, date_col="Date"):
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
            f.write(f"l{days_skipped + 1} [{row['obsnme']}]{column_str}\n")
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

# Texture2Par Parameters
t2p_parameters = {
    'KminFF1'   : [1e-7, 10.0, 1.0, 'K'],
    'KminMF1_M' : [0.9, 100.0, 3.0, 'K'],
    'KminSC1_M' : [0.9, 100.0, 3.0, 'K'],
    'KminMC1_M' : [0.9, 100.0, 10.0, 'K'],
    'KminVC1_M' : [0.9, 100.0, 10.0, 'K'],
    'AnisoVC1'  : [1.0, 10.0, 10.0, 'Aniso'],
    'AnisoMC1_M': [1.0, 10.0, 10.0, 'Aniso'],
    'AnisoSC1_M': [1.0, 10.0,  1.0, 'Aniso'],
    'AnisoMF1_M': [1.0, 10.0,  1.0, 'Aniso'],
    'AnisoFF1_M': [1.0, 10.0,  1.0, 'Aniso'],
    'SsFF1'     : [1e-04, 1e-2, 1e-3, 'Ss'],
    'SsMF1_M'   : [0.1, 1.0, 0.45, 'Ss'],
    'SsSC1_M'   : [0.1, 1.0, 0.45, 'Ss'],
    'SsMC1_M'   : [0.1, 1.0, 0.45, 'Ss'],
    'SsVC1_M'   : [0.1, 1.0, 0.45, 'Ss'],
    'SySC1'     : [0.03, 0.30, 0.20, 'Sy'],
    'SyMF1_M'   : [0.5, 1.0, 0.75, 'Sy'],
    'SyFF1_M'   : [0.5, 1.0, 0.75, 'Sy'],
    'SyMC1_M'   : [0.5, 1.0, 0.75, 'Sy'],
    'SyVC1_M'   : [0.5, 1.0, 0.75, 'Sy'],
    'KHp1'      : [0.75, 1.0, 0.93, 'PLP'],
    'KVp1'      : [-1.0, -0.5, -0.62, 'PLP'],
}

# Replicates par2par setup
t2p_reality_check = t2p_par2par(t2p_parameters)
print(t2p_reality_check)

# Add SFR2PAR segment multipliers
sfr_parameters = {
    'sbcm01': [0.01, 100, 0.8, 'mSFR'],
    'sbcm02': ['tied','sbcm01', 'mSFR'],
    'sbcm03': ['tied','sbcm01', 'mSFR'],
    'sbcm04': ['tied','sbcm01', 'mSFR'],
    'sbcm05': [0.01, 100, 0.8, 'mSFR'],
    'sbcm06': [0.01, 100, 0.8, 'mSFR'],
    'sbcm07': ['tied','sbcm06', 'mSFR'],
    'sbcm08': ['tied','sbcm06', 'mSFR'],
    'sbcm09': [0.01, 100, 0.8, 'mSFR'],
    'sbcm10': ['tied','sbcm09', 'mSFR'],
    'sbcm11': [0.01, 100, 0.8, 'mSFR'],
    'sbcm12': [0.01, 100, 0.8, 'mSFR'],
    'sbcm13': [0.01, 100, 0.8, 'mSFR'],
    'sbcm14': [0.01, 100, 0.8, 'mSFR'],
    'sbcm15': [0.01, 100, 0.8, 'mSFR'],
    'sbcm16': ['tied','sbcm15', 'mSFR'],
    'sbcm17': [0.01, 100, 0.1, 'mSFR'],
    'sbcm18': [0.01, 100, 0.1, 'mSFR'],
    'sbcm19': [0.01, 100, 0.1, 'mSFR'],
    'sbcm20': [0.01, 100, 0.1, 'mSFR'],
    'sbcm21': [0.01, 100, 0.1, 'mSFR'],
    'sbcm22': [0.01, 100, 0.1, 'mSFR'],
    'sbcm23': [0.01, 100, 0.1, 'mSFR'],
    'sbcm24': [0.01, 100, 0.1, 'mSFR'],
    'sbcm25': [0.01, 100, 0.1, 'mSFR'],
    'sbcm26': [0.01, 100, 0.1, 'mSFR'],
    'sbcm27': [0.01, 100, 0.1, 'mSFR'],
    'sbcm28': ['tied','sbcm27', 'mSFR'],
    'sbcm29': ['tied','sbcm27', 'mSFR'],
    'sbcm30': [0.01, 100, 0.8, 'mSFR'],
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

#-- Assemble
pest_parameters = t2p_parameters | sfr_parameters | aem2texture_parameters

#----------------------------------------------------------------------------------------------------------------------#
# Observations
#----------------------------------------------------------------------------------------------------------------------#

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

# Set some variables to reuse
qts = [0.40, 0.80]
cvs = [0.1, 0.2, 0.4]

# Get weights, groups
str_fj['obsgnme'], str_fj['wt'] = cv_stream_weights(str_fj, qts, cvs, 'fj')
str_as['obsgnme'], str_as['wt'] = cv_stream_weights(str_as, qts, cvs, 'as')
str_by['obsgnme'], str_by['wt'] = cv_stream_weights(str_by, qts, cvs, 'by')

# Adjust weights where necessary
# Tolley et al. (2019) found some of the smaller stream obs near 0 created Inf weights
# They assigned the weight of low flow of the non-USGS gauges to be the median FJ low flow weight
str_as.loc[str_as['obsgnme']=='as_low','wt'] = str_fj.loc[str_fj['obsgnme']=='fj_low','wt'].median()
str_by.loc[str_by['obsgnme']=='by_low','wt'] = str_fj.loc[str_fj['obsgnme']=='fj_low','wt'].median()

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
fj_wyearly['wt'] = str_fj.loc[str_fj['obsgnme']=='fj_high','wt'].median()

thresholds = fj_monthly['obsval'].quantile(qts).values
fj_monthly['obsgnme'] = 'FJMONVOL_H'
fj_monthly.loc[fj_monthly['obsval'] <= thresholds[1], 'obsgnme'] = 'FJMONVOL_M'
fj_monthly.loc[fj_monthly['obsval'] <= thresholds[0], 'obsgnme'] = 'FJMONVOL_L'
fj_monthly.loc[fj_monthly['obsgnme']=='FJMONVOL_H', 'wt'] = str_fj.loc[str_fj['obsgnme']=='fj_high','wt'].median()
fj_monthly.loc[fj_monthly['obsgnme']=='FJMONVOL_M', 'wt'] = str_fj.loc[str_fj['obsgnme']=='fj_med','wt'].median()
fj_monthly.loc[fj_monthly['obsgnme']=='FJMONVOL_L', 'wt'] = str_fj.loc[str_fj['obsgnme']=='fj_low','wt'].median()

#fj_monthly['obsnme'] = [f"FJVOL_{d.year}_{d.month:02d}" for d in fj_monthly.index]
fj_vol = pd.concat([fj_wyearly[['obsnme', 'obsgnme', 'obsval', 'wt']],
                    fj_monthly[['obsnme', 'obsgnme', 'obsval', 'wt']]]).reset_index(drop=True)

#----------------------------------------------------------------------------------------------------------------------#
# Combine observations
pst_obs_cols = ['obsgnme', 'obsnme', 'obsval', 'wt']

obs_df = pd.concat([str_fj [pst_obs_cols],
                    str_as [pst_obs_cols],
                    str_by [pst_obs_cols],
                    fj_vol [pst_obs_cols],
                    hobs_df[pst_obs_cols]]).reset_index(drop=True)

#----------------------------------------------------------------------------------------------------------------------#
# Adjust weights

# Define metagroup mappings
metagroup_mapping = {
    # Head observations
    "SV_HEADS": "HEADS",
    "QV_HEADS": "HEADS",
    "fj_low": "STREAMFLOW",
    "fj_med": "STREAMFLOW",
    "fj_high": "STREAMFLOW",
    "as_low": "STREAMFLOW",
    "as_med": "STREAMFLOW",
    "as_high": "STREAMFLOW",
    "by_low": "STREAMFLOW",
    "by_med": "STREAMFLOW",
    "by_high": "STREAMFLOW",
    "FJMONVOL_L": "STREAMFLOW",
    "FJMONVOL_M": "STREAMFLOW",
    "FJMONVOL_H": "STREAMFLOW",
    "FJYRLYVOL": "STREAMFLOW"
}

# Assign metagroup labels
obs_df["metagroup"] = obs_df["obsgnme"].map(metagroup_mapping)

obs_df = balance_metagroup_weights(obs_df)

#----------------------------------------------------------------------------------------------------------------------#
# Setup PEST
#----------------------------------------------------------------------------------------------------------------------#

# Switch to folder PEST will run in
os.chdir('C:/Projects/SVIHM/2024_PEST_t2pcalib/')

# Write INS files
write_ts_ins_file(str_fj, origin_date, '86:99', 2, 'Streamflow_FJ_SVIHM.ins')
write_ts_ins_file(str_as, origin_date, '86:99', 2, 'Streamflow_AS_SVIHM.ins')
write_ts_ins_file(str_by, origin_date, '86:99', 2, 'Streamflow_BY_SVIHM.ins')
write_static_ins_file(fj_vol, 'Streamflow_FJ_SVIHM_VOL.ins', markers='w')
write_hobs_ins_file(hobs_df, 'HobData_SVIHM.ins')

#----------------------------------------------------------------------------------------------------------------------#
# Let pyemu detect all observations & parameters, it serves as a check on everything we're doing
# We can then update the groups, weights, values, etc
pst = pyemu.Pst.from_io_files(tpl_files=['t2p_par2par.tpl',
                                         'sfr2par.tpl',
                                         'AEM2Texture.tpl'],
                              in_files=[Path('./SVIHM/t2p_par2par.in'),
                                        Path('./SVIHM/sfr2par.in'),
                                        Path('./SVIHM/preproc/AEM2Texture.in')],
                              ins_files=['Streamflow_FJ_SVIHM.ins',
                                         'Streamflow_AS_SVIHM.ins',
                                         'Streamflow_BY_SVIHM.ins',
                                         'Streamflow_FJ_SVIHM_VOL.ins',
                                         'HobData_SVIHM.ins',],
                              out_files=[Path('./SVIHM/MODFLOW/Streamflow_FJ_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/Streamflow_AS_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/Streamflow_BY_SVIHM.dat'),
                                         Path('./SVIHM/MODFLOW/Streamflow_FJ_SVIHM_VOL.out'),
                                         Path('./SVIHM/MODFLOW/HobData_SVIHM.dat')],
                              pst_filename='svihm_t2p01.pst')

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
        pst.parameter_data.loc[param.lower(), 'parlbnd'] = 0.0
        pst.parameter_data.loc[param.lower(), 'parubnd'] = 1.0
    elif value[0] == 'fixed':
        pst.parameter_data.loc[param.lower(), 'parval1'] = value[1]
        pst.parameter_data.loc[param.lower(), 'partrans'] = 'fixed'
        pst.parameter_data.loc[param.lower(), 'parlbnd'] = 0.0
        pst.parameter_data.loc[param.lower(), 'parubnd'] = 1.0
    else:
        pst.parameter_data.loc[param.lower(), 'parval1'] = value[2]
        pst.parameter_data.loc[param.lower(), 'parlbnd'] = value[0]
        pst.parameter_data.loc[param.lower(), 'parubnd'] = value[1]

# Adjust scale, transformation of power-law parameters
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"parval1"] *= 100
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"parlbnd"] *= 100
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"parubnd"] *= 100
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"scale"]    = 1/100
pst.parameter_data.loc[pst.parameter_data['pargp']=='PLP',"partrans"] = 'none'

# And Specific Yield
pst.parameter_data.loc[pst.parameter_data['parnme']=='sysc1',"parval1"] *= 100
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
pyemu.helpers.zero_order_tikhonov(pst, parbounds=True, par_groups=['aemscale'])  # fancy pyemu helper
pst.prior_information['weight'] *= 10000

pst.model_command = [str(Path("forward_run.bat"))]

# Set some final options, save the PST file
pst.control_data.noptmax = 0
pst.svd_data.maxsing = pst.npar_adj
#pst.svd_data.eigthresh
pst.control_data.numcom = 1
pst.control_data.jacfile = np.int32(0)
pst.control_data.messfile = np.int32(0)
pst.control_data.numlam = 10
#pst.reg_data.phimlim = pst.nnz_obs - pst.npar_adj    # comment out to check out best fit
#pst.reg_data.phimaccept = 1.1 * pst.reg_data.phimlim # same
#pst.reg_data.fracphim
pst.reg_data.wfinit = 1.0
pst.reg_data.wffac = 1.3

pst.write('svihm_t2p01.pst', version=1)