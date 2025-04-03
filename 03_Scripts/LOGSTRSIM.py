import pandas as pd
import numpy as np
import argparse

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# HOB file header
header = ['Time', 'Stage', 'Flow', 'Depth', 'Width', 'MidptFlow', 'Precip', 'ET', 'Runoff', 'Conductance', 'HeadDiff', 'Grad']

# Setup parser
parser = argparse.ArgumentParser(description="Log Transform SFR Reach Output")
parser.add_argument("streamflow_file", type=str, help="SFR Output Reach file")
parser.add_argument("column", type=str, help="SFR Output Column")

offset = 0.1

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Communicate
    print("LOGSTRSIM.py")

    # parse args
    args = parser.parse_args()

    # process args
    out_file = f"{args.streamflow_file.split('.')[0]}_{args.column}_LOG.out"

    # Check
    assert args.column in header

    # Read in
    print(f"Reading {args.streamflow_file}")
    sfr_out = pd.read_table(args.streamflow_file, sep="\\s+", skiprows=1, names=header)
    sfr_out[args.column] = np.log(sfr_out[args.column] + offset)

    # Write to file
    print(f"Writing {out_file}")
    sfr_out[['Time',args.column]].to_csv(out_file, sep=" ", header=False, index=False)