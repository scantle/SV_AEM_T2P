        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:04 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2BAS7V__genmod
          INTERFACE 
            SUBROUTINE SGWF2BAS7V(MSUM,VBNM,VBVL,KSTP,KPER,IOUT)
              INTEGER(KIND=4) :: MSUM
              CHARACTER(LEN=16) :: VBNM(MSUM)
              REAL(KIND=8) :: VBVL(4,MSUM)
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE SGWF2BAS7V
          END INTERFACE 
        END MODULE SGWF2BAS7V__genmod
