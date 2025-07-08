        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSVB__genmod
          INTERFACE 
            SUBROUTINE UBDSVB(IBDCHN,NCOL,NROW,J,I,K,Q,VAL,NVL,NAUX,LAUX&
     &,IBOUND,NLAY)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NVL
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: K
              REAL(KIND=4) :: Q
              REAL(KIND=4) :: VAL(NVL)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: LAUX
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSVB
          END INTERFACE 
        END MODULE UBDSVB__genmod
