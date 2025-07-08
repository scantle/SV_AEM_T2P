        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:37 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE USUB2D__genmod
          INTERFACE 
            SUBROUTINE USUB2D(ZZ,NCOL,NROW,IP,ILAY,INIT,NSUB)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: ZZ(NCOL,NROW)
              INTEGER(KIND=4) :: IP
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: INIT
              INTEGER(KIND=4) :: NSUB
            END SUBROUTINE USUB2D
          END INTERFACE 
        END MODULE USUB2D__genmod
