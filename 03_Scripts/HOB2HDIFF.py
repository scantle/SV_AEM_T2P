import pandas as pd
import argparse

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# HOB file header
header = ['simval', 'obval', 'obsnme']

# Setup parser
parser = argparse.ArgumentParser(description="Convert head observation data into head difference data.")
parser.add_argument("hob_file", type=str, help="MODFLOW HOB output filename")

vertical_well_pairs = [
    ('ST201', 'ST201_2'),
    ('ST786', 'ST786_2')
]

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Communicate
    print("HOB2HDIFF.py")

    # parse args
    args = parser.parse_args()

    # process args

    out_file = [f"{args.hob_file.split('.')[0]}_DIFF.out", f"{args.hob_file.split('.')[0]}_VDIFF.out"]

    # Read in
    print(f"Reading {args.hob_file}")
    hob = pd.read_table(args.hob_file, sep="\\s+", skiprows=1, names=header)

    # Drop single observation wells
    hob = hob[hob['obsnme'].str.contains(r'\.')]

    # Calculate Differences by Well
    hob['wellname'] = [name.split('.')[0] for name in hob['obsnme']]
    hob['reltime'] = [int(name.split('.')[1]) for name in hob['obsnme']]
    hob['simval_diff'] = hob.groupby('wellname')['simval'].diff()
    hob_diff = hob.dropna(subset=['simval_diff']).copy()
    hob_diff['obsnme'] = hob_diff['obsnme'] + '_D'

    # Calculate vertical head differences
    vhdiff_list = []

    for top_well, bottom_well in vertical_well_pairs:
        # Subset each well
        top_df = hob[hob['wellname'] == top_well][['reltime', 'simval']].rename(columns={'simval': 'sim_top'})
        bot_df = hob[hob['wellname'] == bottom_well][['reltime', 'simval']].rename(columns={'simval': 'sim_bot'})

        # Merge on time index
        merged = pd.merge(top_df, bot_df, on='reltime', how='inner')
        merged['vhdiff'] = merged['sim_top'] - merged['sim_bot']

        merged['obsnme'] = [f"{top_well}_VD.{i}" for i in merged['reltime']]

        # Append
        vhdiff_list.append(merged[['obsnme', 'vhdiff']])

    vhdiff_df = pd.concat(vhdiff_list, ignore_index=True)

    # Write to file
    print(f"Writing {out_file[0]}")
    hob_diff[['obsnme','simval_diff']].to_csv(out_file[0], sep=" ", header=False, index=False)
    print(f"Writing {out_file[1]}")
    vhdiff_df.to_csv(out_file[1], sep=" ", header=False, index=False)