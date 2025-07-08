        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:38 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2LPFU1SC__genmod
          INTERFACE 
            SUBROUTINE SGWF2LPFU1SC(SC,ISPST)
              USE GLOBAL, ONLY :                                        &
     &          NCOL,                                                   &
     &          NROW,                                                   &
     &          DELR,                                                   &
     &          DELC,                                                   &
     &          LAYCBD,                                                 &
     &          NODES,                                                  &
     &          TOP,                                                    &
     &          BOT,                                                    &
     &          AREA
              REAL(KIND=4) :: SC(NODES)
              INTEGER(KIND=4) :: ISPST
            END SUBROUTINE SGWF2LPFU1SC
          END INTERFACE 
        END MODULE SGWF2LPFU1SC__genmod
