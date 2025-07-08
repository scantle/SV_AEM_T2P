        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SSMS2BCFU1P__genmod
          INTERFACE 
            SUBROUTINE SSMS2BCFU1P(CNCG,LRCH,ITP,MXITER,IOUT,IUNSTR)
              INTEGER(KIND=4) :: MXITER
              REAL(KIND=8) :: CNCG(MXITER)
              INTEGER(KIND=4) :: LRCH(3,MXITER)
              INTEGER(KIND=4) :: ITP
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: IUNSTR
            END SUBROUTINE SSMS2BCFU1P
          END INTERFACE 
        END MODULE SSMS2BCFU1P__genmod
