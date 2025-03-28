import pandas as pd
import numpy as np

def t2p_par2par(t2p_parameters):
    """
    Replicates the chain multiplications from the par2par file and
    returns a DataFrame of final parameter values for each texture.
    """

    # --- Kmin chain ---
    #  KminFF1 = $ KminFF1
    #  KminMF1 = KminFF1 * $ KminMF1_M
    #  KminSC1 = KminMF1 * $ KminSC1_M
    #  KminMC1 = KminSC1 * $ KminMC1_M
    #  KminVC1 = KminMC1 * $ KminVC1_M
    k_ff = t2p_parameters['KminFF1'][2]
    k_mf = k_ff * t2p_parameters['KminMF1_M'][2]
    k_sc = k_mf * t2p_parameters['KminSC1_M'][2]
    k_mc = k_sc * t2p_parameters['KminMC1_M'][2]
    k_vc = k_mc * t2p_parameters['KminVC1_M'][2]

    # --- Aniso chain ---
    #  AnisoVC1 = $ AnisoVC1
    #  AnisoMC1 = AnisoVC1 * $ AnisoMC1_M
    #  AnisoSC1 = AnisoMC1 * $ AnisoSC1_M
    #  AnisoMF1 = AnisoSC1 * $ AnisoMF1_M
    #  AnisoFF1 = AnisoMF1 * $ AnisoFF1_M
    an_vc = t2p_parameters['AnisoVC1'][2]
    an_mc = an_vc * t2p_parameters['AnisoMC1_M'][2]
    an_sc = an_mc * t2p_parameters['AnisoSC1_M'][2]
    an_mf = an_sc * t2p_parameters['AnisoMF1_M'][2]
    an_ff = an_mf * t2p_parameters['AnisoFF1_M'][2]

    # --- Ss chain ---
    #  SsFF1 = $ SsFF1
    #  SsMF1 = SsFF1 * $ SsMF1_M
    #  SsSC1 = SsMF1 * $ SsSC1_M
    #  SsMC1 = SsSC1 * $ SsMC1_M
    #  SsVC1 = SsMC1 * $ SsVC1_M
    ss_ff = t2p_parameters['SsFF1'][2]
    ss_mf = ss_ff * t2p_parameters['SsMF1_M'][2]
    ss_sc = ss_mf * t2p_parameters['SsSC1_M'][2]
    ss_mc = ss_sc * t2p_parameters['SsMC1_M'][2]
    ss_vc = ss_mc * t2p_parameters['SsVC1_M'][2]

    # --- Sy chain ---
    #  SySC1 = $ SySC1
    #  SyMF1 = SySC1 * $ SyMF1_M
    #  SyFF1 = SyMF1 * $ SyFF1_M
    #  SyMC1 = SySC1 * $ SyMC1_M
    #  SyVC1 = SyMC1 * $ SyVC1_M
    sy_sc = t2p_parameters['SySC1'][2]
    sy_mf = sy_sc * t2p_parameters['SyMF1_M'][2]
    sy_ff = sy_mf * t2p_parameters['SyFF1_M'][2]
    sy_mc = sy_sc * t2p_parameters['SyMC1_M'][2]
    sy_vc = sy_mc * t2p_parameters['SyVC1_M'][2]

    # Build dict of final values keyed by texture;
    # The order is (FF, MF, SC, MC, VC) as rows
    # and the hydraulic properties as columns: (Kmin, Aniso, Ss, Sy)
    final_vals = {
        "FF": {"Kmin": k_ff, "Aniso": an_ff, "Ss": ss_ff, "Sy": sy_ff},
        "MF": {"Kmin": k_mf, "Aniso": an_mf, "Ss": ss_mf, "Sy": sy_mf},
        "SC": {"Kmin": k_sc, "Aniso": an_sc, "Ss": ss_sc, "Sy": sy_sc},
        "MC": {"Kmin": k_mc, "Aniso": an_mc, "Ss": ss_mc, "Sy": sy_mc},
        "VC": {"Kmin": k_vc, "Aniso": an_vc, "Ss": ss_vc, "Sy": sy_vc},
    }

    # Convert to a DataFrame with the desired row and column order, change how numbers show
    df = pd.DataFrame(final_vals).T[["Kmin", "Aniso", "Ss", "Sy"]]
    df['Ss'] = df['Ss'].apply(lambda x: np.format_float_scientific(x, precision=2))
    df['Sy'] = df['Sy'].apply(lambda x: np.format_float_scientific(x, precision=2))

    return df

#----------------------------------------------------------------------------------------------------------------------#

def t2p_par2par_frompar(t2p_parameters):
    """
    Replicates the chain multiplications from the par2par file and
    returns a DataFrame of final parameter values for each texture.

    Parameters
    ----------
    t2p_parameters : pd.DataFrame
        A DataFrame with index = parameter names, and columns:
        ['parval1', 'scale', 'offset'] as written by PEST.

    Returns
    -------
    pd.DataFrame
        Final parameter values by texture class (FF, MF, SC, MC, VC),
        with columns: Kmin, Aniso, Ss, Sy.
    """
    # Ensure index is lowercase
    df = t2p_parameters.copy()
    df.index = df.index.str.lower()

    # Compute the true parameter values: parval1 * scale + offset
    pvals = df['parval1'] * df['scale'] + df['offset']

    # --- Kmin chain ---
    k_ff = pvals['kminff1']
    k_mf = k_ff * pvals['kminmf1_m']
    k_sc = k_mf * pvals['kminsc1_m']
    k_mc = k_sc * pvals['kminmc1_m']
    k_vc = k_mc * pvals['kminvc1_m']

    # --- Aniso chain ---
    an_vc = pvals['anisovc1']
    an_mc = an_vc * pvals['anisomc1_m']
    an_sc = an_mc * pvals['anisosc1_m']
    an_mf = an_sc * pvals['anisomf1_m']
    an_ff = an_mf * pvals['anisoff1_m']

    # --- Ss chain ---
    ss_ff = pvals['ssff1']
    ss_mf = ss_ff * pvals['ssmf1_m']
    ss_sc = ss_mf * pvals['sssc1_m']
    ss_mc = ss_sc * pvals['ssmc1_m']
    ss_vc = ss_mc * pvals['ssvc1_m']

    # --- Sy chain ---
    sy_sc = pvals['sysc1']
    sy_mf = sy_sc * pvals['symf1_m']
    sy_ff = sy_mf * pvals['syff1_m']
    sy_mc = sy_sc * pvals['symc1_m']
    sy_vc = sy_mc * pvals['syvc1_m']

    # Final values by texture
    final_vals = {
        "FF": {"Kmin": k_ff, "Aniso": an_ff, "Ss": ss_ff, "Sy": sy_ff},
        "MF": {"Kmin": k_mf, "Aniso": an_mf, "Ss": ss_mf, "Sy": sy_mf},
        "SC": {"Kmin": k_sc, "Aniso": an_sc, "Ss": ss_sc, "Sy": sy_sc},
        "MC": {"Kmin": k_mc, "Aniso": an_mc, "Ss": ss_mc, "Sy": sy_mc},
        "VC": {"Kmin": k_vc, "Aniso": an_vc, "Ss": ss_vc, "Sy": sy_vc},
    }

    df_out = pd.DataFrame(final_vals).T[["Kmin", "Aniso", "Ss", "Sy"]]
    df_out['Ss'] = df_out['Ss'].apply(lambda x: np.format_float_scientific(x, precision=2))
    df_out['Sy'] = df_out['Sy'].apply(lambda x: np.format_float_scientific(x, precision=2))

    return df_out