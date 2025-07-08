        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSV4__genmod
          INTERFACE 
            SUBROUTINE UBDSV4(KSTP,KPER,TEXT,NAUX,AUXTXT,IBDCHN,NCOL,   &
     &NROW,NLAY,NLIST,IOUT,DELT,PERTIM,TOTIM,IBOUND)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: NAUX
              CHARACTER(LEN=16) :: AUXTXT(*)
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: NLIST
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=8) :: DELT
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
            END SUBROUTINE UBDSV4
          END INTERFACE 
        END MODULE UBDSV4__genmod
