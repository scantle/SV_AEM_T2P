        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULAPRWC__genmod
          INTERFACE 
            SUBROUTINE ULAPRWC(A,NCOL,NROW,ILAY,IOUT,IPRN,ANAME)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: A(NCOL,NROW)
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: IPRN
              CHARACTER(*) :: ANAME
            END SUBROUTINE ULAPRWC
          END INTERFACE 
        END MODULE ULAPRWC__genmod
