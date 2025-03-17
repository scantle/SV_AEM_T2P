ptf $
nclasses 5                                         # Number of texture classes
rho_log_file  ..\..\05_Outputs\AEMLog_noUnsat.dat  # Reading this
tex_out_file  logs_and_AEM_5classes.dat            # Writing this
prv_log_file  LithoLog_5classes_nocoloc.dat        # Will copy this over first (NONE if not needed)
        Texture       Shape    Location       Scale
Fine               0.289190    0.000000  $ ScaleFF           $
Mixed_Fine         0.077596    0.000000  $ ScaleMF           $
Sand               0.076751    0.000000  $ ScaleSC           $
Mixed_Coarse       0.075546    0.000000  $ ScaleMC           $
Very_Coarse        0.661872  182.669998  $ ScaleVC           $