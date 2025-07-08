        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:11 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDMRGL__genmod
          INTERFACE 
            SUBROUTINE XMDMRGL(I,IAF,JAF,IDIAGF,LIST,NJAF,N,FIRST,LEVEL,&
     &LEVPTR,LROWPTR,NLEVPTR)
              INTEGER(KIND=4) :: NLEVPTR
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NJAF
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: IAF(N+1)
              INTEGER(KIND=4) :: JAF(NJAF)
              INTEGER(KIND=4) :: IDIAGF(N)
              INTEGER(KIND=4) :: LIST(N)
              INTEGER(KIND=4) :: FIRST
              INTEGER(KIND=4) :: LEVEL
              INTEGER(KIND=4) :: LEVPTR(NLEVPTR)
              INTEGER(KIND=4) :: LROWPTR(N)
            END SUBROUTINE XMDMRGL
          END INTERFACE 
        END MODULE XMDMRGL__genmod
