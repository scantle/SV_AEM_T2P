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
