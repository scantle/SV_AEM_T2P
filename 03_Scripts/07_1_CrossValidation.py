import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pathlib import Path
from datetime import datetime
import copy
import shutil
import subprocess
from concurrent.futures import ThreadPoolExecutor
import t2py

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Files
mod_dir = Path('./02_Models/SVIHM_MF/MODFLOW/')
t2p_dir = Path('./02_Models/Texture2Par_onlytexture_2D/')
t2p_inf = t2p_dir / 'svihm.t2p'
t2p_log = t2p_dir / 'logs_and_AEM_5classes.dat'
t2p_path = Path('./00_Tools/T2P_Beta2/bin/Texture2Par.exe')
run_dir = Path('./02_Models/CV_runs/')
run_dir.mkdir(parents=True, exist_ok=True)

classes = ['Fine', 'Mixed_Fine', 'Sand', 'Mixed_Coarse', 'Very_Coarse']
FOLDS = 10
N_LIST = [16] #, 24, 32, 48, 64, 96, 128, 200, 300]

np.random.seed(667)

# -------------------------------------------------------------------------------------------------------------------- #
# Functions/Classes
# -------------------------------------------------------------------------------------------------------------------- #

def update_nnear(t2p_list, nnear_lines, new_nnear):
    new_t2p_list = copy.copy(t2p_list)
    for i in nnear_lines:
        target = new_t2p_list[i].split()[-1]
        new_t2p_list[i] = new_t2p_list[i].replace(target, str(new_nnear))
    return new_t2p_list

# -------------------------------------------------------------------------------------------------------------------- #

def run_t2p(dir, t2p_path, t2p_infile):
    """Run the external program with given argument list."""
    try:
        result = subprocess.run(
            [t2p_path, t2p_infile],
            cwd=dir,
            capture_output=True,
            text=True,
            check=True      # raise exception on failure
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        return f"ERROR: {e}\n{e.stdout}\n{e.stderr}"

# -------------------------------------------------------------------------------------------------------------------- #

def job(run_config):
    run_dir, t2p_infile = run_config
    print(f" - STARTED:  {run_dir.name}")
    result = run_t2p(run_dir, t2p_path.absolute(), t2p_infile)
    print(f" - FINISHED: {run_dir.name}")
    return result

# -------------------------------------------------------------------------------------------------------------------- #

def get_texture_results(dir, classes):
    texture_data = {tex: pd.read_csv(dir / f't2p_{tex.upper()}.csv', na_values=-999) for tex in classes}
    combined = None
    for tex_name, df in texture_data.items():
        layers = sum(df.columns.str.startswith('Layer'))
        df = df.rename(columns={df.columns[0]: 'Row', df.columns[1]: 'Col'})
        for k in range(0,layers):
            if k==0:  laycombined = df['Layer1']
            else:
                laycombined = pd.concat([laycombined, df[f'Layer{k}']])
        if combined is None:
            combined = pd.DataFrame({tex_name : laycombined.values})
        else:
            combined[tex_name] = laycombined.values

    # Normalize & return
    return combined.div(combined.sum(axis=1), axis=0)

# -------------------------------------------------------------------------------------------------------------------- #

def calculate_cv_metrics(full_df, fold_df, classes):
    # Combine prediction and truth
    combined = pd.concat([full_df[classes], fold_df[classes]], axis=1, keys=['true', 'pred'])
    combined = combined.dropna()
    if combined.empty:
        return None

    true_vals = combined['true'].values
    pred_vals = combined['pred'].values

    brier_score = ((true_vals - pred_vals) ** 2).sum(axis=1).mean()
    mae_score = np.abs(true_vals - pred_vals).mean(axis=1).mean()

    true_class = true_vals.argmax(axis=1)
    pred_class = pred_vals.argmax(axis=1)
    misclass_rate = (true_class != pred_class).mean()

    out_of_bounds = ((pred_vals < 0) | (pred_vals > 1)).sum()
    out_of_bounds_prop = out_of_bounds / pred_vals.size

    return {
        'brier_score': brier_score,
        'mae_score': mae_score,
        'misclass_rate': misclass_rate,
        'out_of_bounds_prop': out_of_bounds_prop,
        'n_samples': len(pred_vals)
    }

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

# Setup results DF
all_logs = pd.DataFrame()

# Read in well log file, setup folds
basecase = t2py.Dataset(classes=classes,
                        filename=t2p_log, sep='\s+')
fold_tag = np.random.randint(0, FOLDS, size=basecase.max_id)

# Read in main input file as list
t2p_in = []
nnear_lines = []
in_vario_block = False
with open(t2p_inf, 'r') as f:
    for i, line in enumerate(f):
        t2p_in.append(line)
        if line.startswith('BEGIN VARIOGRAMS'):
            in_vario_block = True
        elif line.startswith('END VARIOGRAMS'):
            in_vario_block = False
        if in_vario_block and line.strip().startswith('CLASS'):
            nnear_lines.append(i+1)
print(f'Found {len(nnear_lines)} variogram nnear specifications in {t2p_inf.name}')


# Setup tests
for n in N_LIST:
    print('\n*----------------------------------------------------')
    print(f'* Starting nnear = {n}')
    print('*----------------------------------------------------\n')

    # Setup T2P file
    t2p_nnear = update_nnear(t2p_in, nnear_lines, n)

    nruns = []

    # Setup full run
    full_dir = run_dir / f'{n}_nnear' / f'full_run'
    full_dir.mkdir(parents=True, exist_ok=True)

    # Write files
    with open(full_dir / t2p_inf.name, "w") as f:
        for line in t2p_nnear:
            f.write(line)
    nruns.append((full_dir.absolute(), t2p_inf.name))
    print(f'Wrote {full_dir / t2p_inf.name}')

    basecase.write_file(filename=full_dir / t2p_log.name)
    print(f'Wrote {full_dir / t2p_log.name}')

    # Setup Folds
    for f in range(0, FOLDS):

        # Create a new folder
        nnear_dir = run_dir / f'{n}_nnear' / f'fold_{f}'
        nnear_dir.mkdir(parents=True, exist_ok=True)

        # Setup dataset
        folddf = basecase.fj_sub.copy()
        loc_ids = [idx for idx, tag in enumerate(fold_tag) if tag != f]
        folddf = folddf[folddf.ID.isin(loc_ids)]
        foldcase = t2py.Dataset(classes)
        foldcase.add_wells_by_df(folddf, name_col='Location', fill_missing=False)

        # Write files
        with open(nnear_dir / t2p_inf.name, "w") as f:
            for line in t2p_nnear:
                f.write(line)
        nruns.append((nnear_dir.absolute(), t2p_inf.name))
        print(f'Wrote {nnear_dir / t2p_inf.name}')

        foldcase.write_file(filename=nnear_dir / t2p_log.name)
        print(f'Wrote {nnear_dir / t2p_log.name}')

    # Copy in model folder
    try:
        shutil.copytree(mod_dir, run_dir / f'{n}_nnear' / mod_dir.name)
    except FileExistsError:
        pass

    # Run for all nnear folders
    with ThreadPoolExecutor(max_workers=12) as pool:
        results = list(pool.map(job, nruns))

    # Read in full results
    full = get_texture_results(full_dir, classes)

    # Loop over folds getting results
    cv_results = []

    for f in range(FOLDS):
        fold_dir = run_dir / f'{n}_nnear' / f'fold_{f}'
        fold_pred = get_texture_results(fold_dir, classes)

        # Remove NAs and calculate metrics
        result = calculate_cv_metrics(full, fold_pred, classes)

        if result is not None:
            result.update({'nnear': n, 'fold': f})
            cv_results.append(result)
        else:
            print(f"Fold {f}: No valid data after dropping NAs.")
    cv_log_df = pd.DataFrame(cv_results)
    all_logs = pd.concat([all_logs, cv_log_df], ignore_index=True)

# Write Out Log
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
log_path = run_dir / f'CV_Results_{timestamp}.csv'
all_logs.to_csv(log_path, index=False)

# --- Analysis ---
print("\nCross-validation complete.")
print(f"Saved results to: {log_path}\n")

summary = []

# Group by nnear and average across folds
grouped = all_logs.groupby('nnear').agg({
    'brier_score': 'mean',
    'mae_score': 'mean',
    'misclass_rate': 'mean',
    'out_of_bounds_prop': 'mean',
    'n_samples': 'sum'
}).reset_index()

# Find the best nnear values for each metric
best_brier = grouped.loc[grouped['brier_score'].idxmin()]
best_mae = grouped.loc[grouped['mae_score'].idxmin()]
best_misclass = grouped.loc[grouped['misclass_rate'].idxmin()]

print("*----------------- Summary -----------------*")
print(grouped.to_string(index=False, float_format="%.4f"))

print("\nBest by metric:")
print(f"- Best Brier Score       : nnear={best_brier['nnear']}, score={best_brier['brier_score']:.4f}")
print(f"- Best MAE               : nnear={best_mae['nnear']}, score={best_mae['mae_score']:.4f}")
print(f"- Best Misclass. Rate    : nnear={best_misclass['nnear']}, rate={best_misclass['misclass_rate']:.4f}")
print("*-------------------------------------------*")

plt.figure()
plt.plot(grouped['nnear'], grouped['brier_score'], marker='o', label='Brier Score')
plt.plot(grouped['nnear'], grouped['mae_score'], marker='o', label='MAE')
plt.xlabel("nnear")
plt.ylabel("Score")
plt.title("CV Error vs nnear")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig(run_dir / f"CV_Summary_{timestamp}.png")