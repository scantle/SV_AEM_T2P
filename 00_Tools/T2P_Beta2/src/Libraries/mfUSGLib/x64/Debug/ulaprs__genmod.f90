        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULAPRS__genmod
          INTERFACE 
            SUBROUTINE ULAPRS(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,   &
     &IOUT)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: BUF(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: IPRN
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE ULAPRS
          END INTERFACE 
        END MODULE ULAPRS__genmod
