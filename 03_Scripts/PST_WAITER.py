import subprocess
from pathlib import Path
import pandas as pd
import pyemu
import argparse

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# Setup parser
parser = argparse.ArgumentParser(description="Calculate PEST Objective Function given INSCHEK files.")
parser.add_argument("pst_file", type=str, help="PST File")

#----------------------------------------------------------------------------------------------------------------------#
# Function
#----------------------------------------------------------------------------------------------------------------------#

def calc_pest_obj(
        pst: pyemu.Pst,
        inschek_exe: str = "inschek.exe",
        work_dir: Path | str = ".",
        quiet: bool = False,
        force_run_inschek: bool = False
    ) -> pd.DataFrame:
    """
    Emulates PEST's objective‑function calculation (Φ) for observation data only
    (regularisation terms are ignored).

    Parameters
    ----------
    pst : pyemu.Pst
        Control‑file object that is *already* loaded.
    inschek_exe : str, default "inschek.exe"
        Path (or just the name if on PATH) to the PEST utility **inschek**.
    work_dir : Path or str, default "."
        Directory in which the model was run (where the output files live and
        where *.obf files will be written).
    quiet : bool, default False
        If False (default) prints Φ totals to screen; True suppresses printing.

    Returns
    -------
    pd.DataFrame
        A **copy** of `pst.observation_data` with three extra columns:

        * `sim`  – simulated value parsed from the *.obf files
        * `resid` – residual (sim ‑ obs)
        * `phi`  – contribution to Φ, i.e. `(weight * resid)**2`
    """
    work_dir = Path(work_dir)
    obs_df = pst.observation_data.copy()

    # 1. ------------------------------------------------------------------ #
    # Run inschek on every (instruction, output) pair defined in the pst
    # --------------------------------------------------------------------- #
    obf_paths = []
    for ins_file, out_file in zip(pst.instruction_files, pst.output_files):
        ins_file = work_dir / ins_file
        out_file = work_dir / out_file
        obf_path = ins_file.with_suffix(".obf")

        if not ins_file.exists():
            raise FileNotFoundError(ins_file)
        if not out_file.exists():
            raise FileNotFoundError(out_file)
        if force_run_inschek or not obf_path.exists():
            # inschek creates an .obf file with the same stem as the instruction file
            subprocess.run(
                [inschek_exe, str(ins_file), str(out_file)],
                cwd=work_dir,
                check=True,
                capture_output=True
            )
        else:
            if not quiet:
                print(f'Skipping inschek for: {ins_file} (already exists)')
        obf_paths.append(obf_path)

    # 2. ------------------------------------------------------------------ #
    # Read every *.obf, concatenate, and merge with observation_data
    # --------------------------------------------------------------------- #
    sim_list = []
    for obf in obf_paths:
        # Each line:  <obsnme> <simulated_value>
        df = pd.read_csv(
            obf, sep="\s+", header=None,
            names=["obsnme", "sim"]
        )
        sim_list.append(df)

    sim_df = pd.concat(sim_list, ignore_index=True)
    merged = obs_df.reset_index(drop=True).merge(sim_df, on="obsnme", how="left", validate="one_to_one")

    # sanity check – make sure every obs got a simulated value
    missing_sim = merged["sim"].isna().sum()
    if missing_sim:
        raise ValueError(f"{missing_sim} observations missing simulated values")

    # 3. ------------------------------------------------------------------ #
    # Calculate residuals and Φ contributions
    # --------------------------------------------------------------------- #
    merged["resid"] = merged["sim"] - merged["obsval"]
    merged["phi"]   = (merged["weight"] * merged["resid"]) ** 2

    # 4. ------------------------------------------------------------------ #
    # Summaries
    # --------------------------------------------------------------------- #
    phi_total = merged["phi"].sum()
    phi_by_grp = merged.groupby("obgnme")["phi"].sum().sort_values(ascending=False)

    if not quiet:
        print(f"\nΦ (sum of squared weighted residuals)  : {phi_total:,.4g}\n")
        print("Break‑down by observation group:")
        for grp, val in phi_by_grp.items():
            print(f"  {grp:<20s} : {val:,.4g}")
        print()

    return merged

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Communicate
    print("PST_WAITER.py")

    # parse args
    args = parser.parse_args()

    pst = pyemu.Pst(args.pst_file)
    phi_df = calc_pest_obj(pst, inschek_exe="./inschek.exe", work_dir=".")
    phi_df['aresid'] = phi_df['resid'].abs()
    print('\n-------------- STATS --------------')

    print('\nTop 10 Highest Absolute Residuals (excluding zero-weighted values):')
    print(phi_df.loc[phi_df['weight']>0,['obsnme','weight','aresid','phi','obgnme']].sort_values('aresid', ascending=False).head(10))

    print('\nTop 10 Highest Φ Observations:')
    print(phi_df.loc[phi_df['weight'] > 0, ['obsnme', 'weight', 'resid', 'phi', 'obgnme']].sort_values('phi', ascending=False).head(10))

    print('\nHighest Weighted Observations:')
    print(phi_df.loc[phi_df['weight'] > 0, ['obsnme', 'weight', 'resid', 'phi', 'obgnme']].sort_values('weight',ascending=False).head(10))