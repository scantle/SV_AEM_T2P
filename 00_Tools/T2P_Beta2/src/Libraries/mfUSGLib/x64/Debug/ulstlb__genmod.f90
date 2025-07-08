        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULSTLB__genmod
          INTERFACE 
            SUBROUTINE ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: LABEL
              CHARACTER(LEN=16) :: CAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
            END SUBROUTINE ULSTLB
          END INTERFACE 
        END MODULE ULSTLB__genmod
