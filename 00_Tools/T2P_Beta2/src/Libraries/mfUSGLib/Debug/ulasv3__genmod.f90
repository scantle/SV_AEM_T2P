        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULASV3__genmod
          INTERFACE 
            SUBROUTINE ULASV3(IDATA,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,   &
     &NROW,ILAY,ICHN,FMTOUT,LBLSAV)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: IDATA(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
              CHARACTER(LEN=20) :: FMTOUT
              INTEGER(KIND=4) :: LBLSAV
            END SUBROUTINE ULASV3
          END INTERFACE 
        END MODULE ULASV3__genmod
