import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyemu
import flopy
from tqdm import tqdm
from pathlib import Path

#----------------------------------------------------------------------------------------------------------------------#
# Setup
#----------------------------------------------------------------------------------------------------------------------#

# Directories
data_dir = Path('./01_Data/')
model_dir = Path('./02_Models/SVIHM_MF/')
out_dir = Path('./04_Plots/Weights/')
out_dir.mkdir(exist_ok=True)

# Model Info
model_name = 'SVIHM'
xoff = 499977
yoff = 4571330
origin_date = pd.to_datetime('1990-9-30')

wt_dict = {'N2': ['after_date', pd.to_datetime('01/01/2019')],
           'Q32': ['after_date', pd.to_datetime('02/10/2020')],
           'ST170': ['rolling_median', 0.5, 90, 0.0],
           'ST192': ['rolling_median', 1.5, 365, 0.0],
           'ST690': ['rolling_median', 1.0, 120, 0.0],
           'ST794': ['rolling_median', 0.7,  90, 0.0],
           'ST888': ['rolling_median', 1.0,  60, 0.0],
           'ST987': ['rolling_median', 1.0, 180, 0.0],
           'ST655': ['after_date', pd.to_datetime('10/01/1990')],
           'ST202': ['after_date', pd.to_datetime('10/01/1990')],
           'G40'  : ['after_date', pd.to_datetime('10/01/1990')],
           'L32'  : ['after_date', pd.to_datetime('10/01/1990')],
           'N15'  : ['after_date', pd.to_datetime('10/01/1990')],
           'G31'  : ['after_date', pd.to_datetime('10/01/1990')],
           'ST186': ['after_date', pd.to_datetime('10/01/1990')],
#           'SCT_969': ['rolling_median', 1.0, 365, 0.0],
          }

#----------------------------------------------------------------------------------------------------------------------#
# Classes/Functions
#----------------------------------------------------------------------------------------------------------------------#

def hob_to_df(hob, origin_date, out_file=None):
    obs_records = []
    for hob_entry in tqdm(hob.obs_data, desc='HOB Entry', total=len(hob.obs_data)):
        for ts_data in hob_entry.time_series_data:
            # Extract individual time series values
            totim = ts_data[0]  # Absolute model time
            sp = ts_data[1]  # Stress period (1-based)
            ts = ts_data[2]  # Time step (not 0-indexed)
            obsval = ts_data[3]
            obsname = ts_data[4].decode("utf-8") if isinstance(ts_data[4], bytes) else ts_data[4]  # Ensure string format

            # Convert totim to actual observation date
            obs_date = origin_date + pd.DateOffset(days=totim)

            # Append all relevant data as a row in the list
            obs_records.append({
                "obsnme": obsname,
                "wellid": obsname.split('.')[0],
                "obsval": obsval,
                "row": hob_entry.row,
                "col": hob_entry.column,
                "lay": hob_entry.layer,
                "multilay": hob_entry.multilayer,
                "roff": hob_entry.roff,
                "coff": hob_entry.coff,
                "sp": sp,
                "ts": ts,
                "date": obs_date
            })

    # Convert list of dictionaries into a DataFrame
    obs_df = pd.DataFrame(obs_records)

    if out_file is not None:
        hob_out = pd.read_csv(out_file, sep='\\s+', skiprows=1, header=None, names=['simval','obsval','obsnme'])
        assert hob_out.shape[0] == obs_df.shape[0]
        obs_df = obs_df.merge(hob_out[['obsnme','simval']], on=['obsnme'])

    return obs_df

#----------------------------------------------------------------------------------------------------------------------#

def calculate_hob_weights(hobs_df, wt_dict, bas, out_dir=None):
    """
    Modify the weights in hobs_df based on predefined rules for selected wells.

    Parameters
    ----------
    hobs_df : pandas.DataFrame
        DataFrame containing HOB observations with at least columns:
            - 'wellid': Well identifier
            - 'obsnme': Observation name (unique ID)
            - 'obsval': Observed value
            - 'date': Timestamp
            - 'wt': Weight (to be modified)
    wt_dict : dict
        Dictionary specifying weighting rules for hand-picked wells.
        Format: {'wellid': ['rule_type', value, weight]} where:
            - 'after_date': Zero weight for observations after `value` (a date).
            - 'rolling_median': Zero weight for deviations from rolling median by more than `value`.
    bas : flopy.modflow.ModflowBas
        MODFLOW Basic Package object, used to check inactive cells.
    out_dir : Path or str, optional
        Directory where hydrograph plots should be saved. (no plots if None)

    Returns
    -------
    hobs_df : pandas.DataFrame
        Updated DataFrame with modified weight values.
    """

    # Create a copy of hobs_df to avoid modifying in place
    hobs_df = hobs_df.copy()
    hobs_df['wt'] = 1.0

    # Track wells that were modified for reporting
    too_few_points = []
    inactive_wells = []

    # Process hand-picked wells with specific rules
    for well in wt_dict.keys():
        well_df = hobs_df[hobs_df['wellid'] == well].copy()
        if well_df.shape[0]==0:
            raise RuntimeError(f"No data for well {well}")
        well_df.sort_values("date", inplace=True)
        deviation_threshold = None
        window=None

        rule_type = wt_dict[well][0]
        if rule_type == 'after_date':
            cutoff_date = wt_dict[well][1]
            well_df.loc[(well_df['date'] > cutoff_date), "wt"] = 0.0

            # Merge changes back into hobs_df
            hobs_df.loc[hobs_df["obsnme"].isin(well_df["obsnme"]), "wt"] = well_df["wt"]

        elif rule_type == 'rolling_median':
            deviation_threshold = wt_dict[well][1]
            window = wt_dict[well][2]  # Window for rolling median
            new_weight = wt_dict[well][3]  # Weight to assign to outliers

            # Compute rolling median
            well_df["rolling_median"] = well_df["obsval"].rolling(window=window, center=True, min_periods=15).median()

            # Identify outliers that deviate too far from the rolling median
            outlier_mask = (well_df["rolling_median"] - well_df["obsval"]) > deviation_threshold
            well_df.loc[outlier_mask, "wt"] = new_weight

            # Merge changes back into hobs_df
            hobs_df.loc[hobs_df["obsnme"].isin(well_df["obsnme"]), "wt"] = well_df["wt"]

        # Plot the hydrograph with weights for visualization
        if out_dir is not None:
            plot_hydrograph_weights(well_df, deviation_threshold, window, out_dir)

    # Process all wells for general conditions
    for wellid, well_df in hobs_df.groupby('wellid'):
        # Zero weights for wells with less than 3 data points
        if well_df.shape[0] < 3:
            hobs_df.loc[hobs_df['wellid'] == wellid, "wt"] = 0.0
            too_few_points.append(wellid)

        # Zero weights for wells in inactive cells using BAS
        row, col = int(well_df.iloc[0]['row']), int(well_df.iloc[0]['col'])  # Assume row & col are consistent per well
        if bas.ibound[0, row, col] == 0:  # Check if cell is inactive (ibound=0)
            hobs_df.loc[hobs_df['wellid'] == wellid, "wt"] = 0.0
            inactive_wells.append(wellid)

    # Print a report of changes
    if too_few_points:
        print(f"Wells with too few points (set to wt=0): {too_few_points}")
    if inactive_wells:
        print(f"Wells in inactive cells (set to wt=0): {inactive_wells}")

    return hobs_df  # Return the updated DataFrame

#----------------------------------------------------------------------------------------------------------------------#

def plot_hydrograph_weights(df, deviation_threshold=None, window=None, out_dir=None):
    """
    Plot a hydrograph for a single well, coloring points by weight,
    with a rolling median and a shaded deviation threshold.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame containing columns:
            - 'wellid': Well identifier
            - 'date': pandas datetime
            - 'obsval': Observed head (float)
            - 'weight': Weight (float) for each observation
    deviation_threshold : float, optional
        The threshold for outlier detection (default is None)
    window : float, optional
        Window used for rolling median (default is None).
    out_dir : Path or str
        Directory where plots will be saved.
    """

    if window is not None:

        # Define upper and lower bounds for the shaded region
        df["upper_bound"] = df["rolling_median"]  # + deviation_threshold
        df["lower_bound"] = df["rolling_median"] - deviation_threshold

        # Create figure
        plt.figure(figsize=(10, 5))

        # Plot rolling median as a solid line
        plt.plot(df["date"], df["rolling_median"], color="grey", linewidth=2, linestyle='--', label=f"Rolling Median, {window} days")

        # Fill between the deviation threshold
        plt.fill_between(df["date"], df["lower_bound"], df["upper_bound"], color="lightgray", alpha=0.5, label="Deviation Threshold")

    # Plot scatter where color is determined by weight (highlighting ignored observations)
    scatter = plt.scatter(df["date"], df["obsval"], c=df["wt"], cmap="coolwarm", edgecolor="k", alpha=0.8)

    # Labels and title
    plt.xlabel("Date")
    plt.ylabel("Observed Head (m)")
    plt.title(f"Hydrograph for well {df['wellid'].iloc[0]}")

    # Rotate x-axis labels
    plt.xticks(rotation=45)

    # Add legend
    if window is not None: plt.legend()

    # Out
    if out_dir is not None:
        if window is not None:
            plot_filename = out_dir / f"{df['wellid'].iloc[0]}_{window}window_weight_hydrograph.png"
        else:
            plot_filename = out_dir / f"{df['wellid'].iloc[0]}_weight_hydrograph.png"
        plt.savefig(plot_filename, dpi=300, bbox_inches="tight")
    plt.clf()

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Load model
    gwf = flopy.modflow.Modflow.load((model_name + '.nam'), version='mfnwt', load_only=['dis','bas6'], model_ws=model_dir)
    bas = gwf.get_package('BAS6')

    hob_file = model_dir / "svihm.hob"
    hob = flopy.modflow.ModflowHob.load(hob_file, model=gwf)
    hobs_df = hob_to_df(hob, origin_date)

    hobs_df = calculate_hob_weights(hobs_df, wt_dict, bas, out_dir)