        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULASAVRD__genmod
          INTERFACE 
            SUBROUTINE ULASAVRD(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,   &
     &NROW,ILAY,ICHN)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: BUF(NCOL,NROW)
              CHARACTER(LEN=4) :: TEXT(4)
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
            END SUBROUTINE ULASAVRD
          END INTERFACE 
        END MODULE ULASAVRD__genmod
