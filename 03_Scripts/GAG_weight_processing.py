import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import pandas as pd
from pathlib import Path

# Import weighting tools from 08_1_HOB_weights
#import sys
#sys.path.append('./03_Scripts')
#from HOB_weight_processing import

# import os
# os.chdir('../')

#----------------------------------------------------------------------------------------------------------------------#
# Setup
#----------------------------------------------------------------------------------------------------------------------#

# Directories
data_dir = Path('./01_Data/')
model_dir = Path('./02_Models/SVIHM_MF/')
out_dir = Path('./04_Plots/Weights/')
svihm_dir = Path('../SVIHM/')  # External to project, local SVIHM Git repo
svihm_ref_dir = svihm_dir / 'SVIHM_Input_Files/reference_data_for_plots/'
out_dir.mkdir(exist_ok=True)

# File Locations
fj_file = data_dir / 'FJ (USGS 11519500) Daily Flow, 1990-10-01_2025-02-28.csv'
as_file = svihm_ref_dir / 'Scott River Above Serpa Lane.txt'
by_file = svihm_ref_dir / 'Scott River Below Youngs Dam.txt'

# Model Info
origin_date = pd.to_datetime('1990-9-30')
end_date = pd.to_datetime('2024-9-30')

# Removals
fj_impossible_dates = [(pd.to_datetime('11/21/2001'), pd.to_datetime('11/23/2001')),
                       (pd.to_datetime('10/22/2021'), pd.to_datetime('10/23/2021')),
                       (pd.to_datetime(' 9/19/2016'), pd.to_datetime(' 9/22/2016'))]


# Conversions
cfs_to_m3d = 0.3048**3 * 86400
m3d_to_cfs = 1 / cfs_to_m3d


#----------------------------------------------------------------------------------------------------------------------#
# Functions
#----------------------------------------------------------------------------------------------------------------------#

def plot_flow_distributions(stream_df, stream_name, quantiles=[0.30, 0.70], log_transform=True):
    """
    Plots a violin plot and a flow duration curve (exceedance probability) for a given stream dataset,
    ensuring they share the streamflow (y) axis. Optionally applies a log transform before plotting.
    """
    if log_transform:
        stream_df = stream_df.copy()
        stream_df['obsval'] = np.log10(stream_df['obsval'])
        log_label = " (log10)"
    else:
        log_label = ""

    fig, ax = plt.subplots(1, 2, figsize=(10, 8), sharey=True)

    # Compute quantile-based thresholds
    low_q, high_q = stream_df['obsval'].quantile(quantiles)

    # Violin Plot (left)
    sns.violinplot(y=stream_df['obsval'], ax=ax[0], inner=None, color="skyblue")
    ax[0].axhline(low_q, color='red', linestyle='--', label=f'Low Flow ({quantiles[0] * 100:.0f}%)')
    ax[0].axhline(high_q, color='green', linestyle='--', label=f'High Flow ({quantiles[1] * 100:.0f}%)')
    ax[0].set_ylabel(f"Streamflow (m³/day){log_label}")
    ax[0].set_title(f"Violin Plot of {stream_name} Streamflows")
    ax[0].legend()

    # Flow Duration Curve (right)
    sorted_flows = stream_df['obsval'].sort_values(ascending=False).reset_index(drop=True)
    exceedance_prob = np.arange(1, len(sorted_flows) + 1) / len(sorted_flows)
    ax[1].plot(exceedance_prob * 100, sorted_flows, label="Flow Duration Curve", color='blue')
    ax[1].axhline(low_q, color='red', linestyle='--', label=f'Low Flow Threshold')
    ax[1].axhline(high_q, color='green', linestyle='--', label=f'High Flow Threshold')
    ax[1].set_xlabel("Exceedance Probability (%)")
    #ax[1].set_ylabel(f"Streamflow (m³/day){log_label}")
    ax[1].set_title(f"Flow Duration Curve for {stream_name}")
    ax[1].legend()

    plt.tight_layout()
    plt.show()

    return low_q, high_q


#----------------------------------------------------------------------------------------------------------------------#

def cv_stream_weights(stream_df, quantiles, cv_values, prefix, obscol='obsval'):
    """
    Computes streamflow observation weights based on given quantiles and coefficients of variation (CV),
    and assigns flow categories for PEST parameter grouping.

    Parameters:
        stream_df (pd.DataFrame): DataFrame containing streamflow observations.
        quantiles (list of float): List of quantile values (e.g., [0.4, 0.8]) that define flow categories.
        cv_values (list of float): List of CV values corresponding to each flow category. Must have one more element than quantiles.
        prefix (str): Prefix for flow categories (e.g., 'fj' for Fort Jones stream).
        obscol (str): Name of stream_df column containing flows

    Returns:
        tuple: (Array of computed group names, Array of computed weights).
    """
    if len(cv_values) != len(quantiles) + 1:
        raise ValueError("cv_values must have one more element than quantiles.")

    # Compute flow thresholds
    thresholds = stream_df[obscol].quantile(quantiles).values

    # Assign categories based on thresholds
    stream_df = stream_df.copy()
    stream_df['group'] = f"{prefix}_high"  # Default to high flow

    if len(quantiles) == 1:
        stream_df.loc[stream_df[obscol] <= thresholds[0], 'group'] = f"{prefix}_low"
    elif len(quantiles) == 2:
        stream_df.loc[stream_df[obscol] <= thresholds[1], 'group'] = f"{prefix}_med"
        stream_df.loc[stream_df[obscol] <= thresholds[0], 'group'] = f"{prefix}_low"
    else:
        raise NotImplementedError("cv_stream_weights only can handle 3 groups for now")

    # Assign weights using Tolley et al. (2019)'s equation: 1 / ([obsval] * CV)²
    weight_map = {f"{prefix}_low": cv_values[0], f"{prefix}_med": cv_values[1], f"{prefix}_high": cv_values[2]}
    stream_df['weight'] = 1 / ((stream_df[obscol] * stream_df['group'].map(weight_map)) ** 2)

    return stream_df['group'].values, stream_df['weight'].values


#----------------------------------------------------------------------------------------------------------------------#

def calculate_cv_by_group(df, grp_col="obsgnme"):
    """
    Calculates the coefficient of variation (CV) for each group in the dataset.
    Assumes there is a column 'group' that defines different categories.

    Parameters:
        df (pd.DataFrame): Dataframe containing at least 'group' and 'obsval' columns.

    Returns:
        pd.DataFrame: Dataframe with 'group' and corresponding CV values.
    """
    grouped = df.groupby(grp_col)['obsval'].agg(['mean', 'std'])
    grouped['cv'] = grouped['std'] / grouped['mean']
    grouped = grouped.reset_index()
    return grouped

#----------------------------------------------------------------------------------------------------------------------#

def plot_hydrograph(stream_df, stream_name, prefix, start_date=None, end_date=None, color_by_weight=False):
    """
    Plots a hydrograph for a given stream dataset, coloring points based on flow categories or weights.
    """
    # Count occurrences before subsetting
    if not color_by_weight:
        total_counts = stream_df['obsgnme'].value_counts()
        total_low = total_counts.get(f"{prefix.lower()}_low", 0)
        total_med = total_counts.get(f"{prefix.lower()}_med", 0)
        total_high = total_counts.get(f"{prefix.lower()}_high", 0)

    # Subset data based on start and end dates
    stream_df = stream_df[(stream_df['Date'] >= (start_date or stream_df['Date'].min())) &
                          (stream_df['Date'] <= (end_date or stream_df['Date'].max()))]

    plt.figure(figsize=(12, 6))

    if color_by_weight:
        norm = plt.Normalize(stream_df['wt'].min(), stream_df['wt'].max())
        cmap = plt.cm.viridis
        colors = cmap(norm(stream_df['wt']))
    else:
        colors = stream_df['obsgnme'].map({
            f"{prefix.lower()}_low": 'red',
            f"{prefix.lower()}_med": 'blue',
            f"{prefix.lower()}_high": 'green'
        })

    plt.scatter(stream_df['Date'], stream_df['obsval'], c=colors, s=10, alpha=0.9)
    plt.xlabel("Date")
    plt.ylabel("Streamflow (m³/day)")
    plt.title(f"Hydrograph of {stream_name}")

    # Add legend if categorizing by flow levels
    if not color_by_weight:
        handles = [
            plt.Line2D([0], [0], color='red', marker='o', linestyle='None', label=f'Low Flow ({total_low} pts)'),
            plt.Line2D([0], [0], color='blue', marker='o', linestyle='None', label=f'Medium Flow ({total_med} pts)'),
            plt.Line2D([0], [0], color='green', marker='o', linestyle='None', label=f'High Flow ({total_high} pts)')
        ]
        plt.legend(handles=handles, title="Flow Categories")

    plt.savefig(out_dir / f"{prefix}_hydrograph.png")
    plt.show()

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Read in streamflow files
    str_fj = pd.read_csv(fj_file, parse_dates=['Date'])
    str_as = pd.read_table(as_file, sep="\\s+", parse_dates=['Date'])
    str_by = pd.read_table(by_file, sep="\\s+", parse_dates=['Date'])

    # Convert, combine
    str_fj['obsnme'] = [f"FJ_{i+1}" for i in str_fj.index]
    str_fj['obsval']  = str_fj['Flow'] * cfs_to_m3d
    str_as['obsnme'] = [f"SL_{i + 1}" for i in str_as.index]
    str_as['obsval']  = str_as['Streamflow_m3/day']
    str_by['obsnme'] = [f"YD_{i + 1}" for i in str_by.index]
    str_by['obsval']  = str_by['Streamflow_m3/day']

    # Set some variables to reuse
    qts = [0.40, 0.80]
    cvs = [0.1, 0.2, 0.4]

    # Fort Jones
    pstart = pd.to_datetime('2000-10-01')
    pend = pd.to_datetime('2010-9-30')
    nm = "Fort Jones"
    fj_low, fj_high = plot_flow_distributions(str_fj, nm, quantiles=qts)
    str_fj['obsgnme'],str_fj['wt'] = cv_stream_weights(str_fj, qts, cvs, 'fj')
    emp_cvs = calculate_cv_by_group(str_fj)
    plot_hydrograph(str_fj, nm, 'fj', start_date=pstart, end_date=pend)
    plot_hydrograph(str_fj, nm, 'fj', color_by_weight=True, start_date=pstart, end_date=pend)

    # Above Serpa Lane
    nm = nm
    as_low, as_high = plot_flow_distributions(str_as, nm, quantiles=qts)
    str_as['obsgnme'], str_as['wt'] = cv_stream_weights(str_as, qts, cvs, 'as')
    plot_hydrograph(str_as, nm, 'as')
    plot_hydrograph(str_as, nm, 'as', color_by_weight=True)

    # Below Young's Dam
    nm = "Young's Dam"
    by_low, by_high = plot_flow_distributions(str_by, nm, quantiles=qts)
    str_by['obsgnme'], str_by['wt'] = cv_stream_weights(str_by, qts, cvs, 'by')
    plot_hydrograph(str_by, nm, 'by')
    plot_hydrograph(str_by, nm, 'by', color_by_weight=True)