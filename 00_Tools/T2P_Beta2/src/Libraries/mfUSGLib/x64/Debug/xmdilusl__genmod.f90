        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDILUSL__genmod
          INTERFACE 
            SUBROUTINE XMDILUSL(TEMP,B,AF,IAF,JAF,IDIAGF,NJAF,NBLACK)
              INTEGER(KIND=4) :: NBLACK
              INTEGER(KIND=4) :: NJAF
              REAL(KIND=8) :: TEMP(NBLACK)
              REAL(KIND=8) :: B(NBLACK)
              REAL(KIND=8) :: AF(NJAF)
              INTEGER(KIND=4) :: IAF(NBLACK+1)
              INTEGER(KIND=4) :: JAF(NJAF)
              INTEGER(KIND=4) :: IDIAGF(NBLACK)
            END SUBROUTINE XMDILUSL
          END INTERFACE 
        END MODULE XMDILUSL__genmod
