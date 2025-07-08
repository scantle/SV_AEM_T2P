        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULAPRW8__genmod
          INTERFACE 
            SUBROUTINE ULAPRW8(BUF,TEXT,KSTP,KPER,NCOL,NROW,ILAY,IPRN,  &
     &IOUT)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=8) :: BUF(NCOL,NROW)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: IPRN
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE ULAPRW8
          END INTERFACE 
        END MODULE ULAPRW8__genmod
