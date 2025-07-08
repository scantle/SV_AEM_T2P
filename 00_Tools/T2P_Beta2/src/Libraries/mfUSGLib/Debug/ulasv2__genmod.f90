        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULASV2__genmod
          INTERFACE 
            SUBROUTINE ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,NROW&
     &,ILAY,ICHN,FMTOUT,LBLSAV,IBOUND)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: BUFF(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
              CHARACTER(LEN=20) :: FMTOUT
              INTEGER(KIND=4) :: LBLSAV
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW)
            END SUBROUTINE ULASV2
          END INTERFACE 
        END MODULE ULASV2__genmod
