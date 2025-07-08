        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSVA__genmod
          INTERFACE 
            SUBROUTINE UBDSVA(IBDCHN,NCOL,NROW,J,I,K,Q,IBOUND,NLAY)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: Q
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSVA
          END INTERFACE 
        END MODULE UBDSVA__genmod
