ptf $
*==============================================================================
* Texture2Par Main Input File
*==============================================================================

BEGIN OPTIONS
  # Required Options
  MAX_VSTRUCT    1
  USE_MODEL_GSE
END OPTIONS

BEGIN FLOW_MODEL
  TYPE MODFLOW
  NAM_FILE      ../MODFLOW/SVIHM_t2p.nam
  TEMPLATE_FILE SVIHM_TEMPLATE.upw
  XOFFSET       499977.0 
  YOFFSET       4571330.0
  ROTATION      0.0
END FLOW_MODEL

BEGIN CLASSES
  Fine
  Mixed_Fine
  Sand
  Mixed_Coarse
  Very_Coarse
END CLASSES

BEGIN DATASET
  FILE     logs_and_AEM_5classes.dat
END DATASET

BEGIN VARIOGRAMS
  # Structure Vtype  Nugget  Sill  Range_min Range_max ang1  nnear
  CLASS Fine
           1    Exp    0.023   0.077    2.53E+03       2.53E+03  0.0    32
  CLASS Mixed_Fine
           1    Exp    0.041   0.041    9.46E+02       9.46E+02  0.0    32
  CLASS Sand
           1    Exp    0.025   0.030    3.43E+02       3.43E+02  0.0    32
  CLASS Mixed_Coarse
           1    Exp    0.036   0.059    5.33E+02       5.33E+02  0.0    32
  CLASS Very_Coarse
           1    Exp    0.025   0.090    9.29E+02       9.29E+02  0.0    32
  CLASS PilotPoints                                         
           1    Sph    0.00   1.0      1.0E5     1.0E5  0.0  25
END VARIOGRAMS

BEGIN PP_LOCS
# ID          X           Y Zone
  1    511983.0   4599271.0    1
END PP_LOCS

BEGIN PP_PARAMETERS
  TYPE Global
# ID   KHp    KVp   STp
    1    $  KHp1    $  $  KVp1    $   1.0
  TYPE Aquifer
# ID    Class             Kmin             Kmax              Ss              Sy           Aniso     Kd
   1   Fine         $  KminFF1     $   $  KmaxFF1     $  $  SsFF1       $  $  SyFF1       $  $  AnisoFF1    $  0.007
   1   Mixed_Fine   $  KminMF1     $   $  KmaxMF1     $  $  SsMF1       $  $  SyMF1       $  $  AnisoMF1    $  0.007
   1   Sand         $  KminSC1     $   $  KmaxSC1     $  $  SsSC1       $  $  SySC1       $  $  AnisoSC1    $  0.007
   1   Mixed_Coarse $  KminMC1     $   $  KmaxMC1     $  $  SsMC1       $  $  SyMC1       $  $  AnisoMC1    $  0.007
   1   Very_Coarse  $  KminVC1     $   $  KmaxVC1     $  $  SsVC1       $  $  SyVC1       $  $  AnisoVC1    $  0.007
END PP_PARAMETERS

