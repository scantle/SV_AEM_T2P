        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSV2__genmod
          INTERFACE 
            SUBROUTINE UBDSV2(KSTP,KPER,TEXT,IBDCHN,NCOL,NROW,NLAY,NLIST&
     &,IOUT,DELT,PERTIM,TOTIM,IBOUND)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: NLIST
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=8) :: DELT
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSV2
          END INTERFACE 
        END MODULE UBDSV2__genmod
