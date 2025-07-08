        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:11 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDMRGD__genmod
          INTERFACE 
            SUBROUTINE XMDMRGD(A,AF,ROW,EPSRN,I,NBLACK,IA,IAF,JAF,IDIAGF&
     &,LIST,RBORDER,NJA,NJAF,N,FIRST,LEVEL,LEVPTR,LROWPTR,NLEVPTR)
              INTEGER(KIND=4) :: NLEVPTR
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NJAF
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NBLACK
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: AF(NJAF)
              REAL(KIND=8) :: ROW(NBLACK)
              REAL(KIND=8) :: EPSRN
              INTEGER(KIND=4) :: I
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: IAF(NBLACK+1)
              INTEGER(KIND=4) :: JAF(NJAF)
              INTEGER(KIND=4) :: IDIAGF(NBLACK)
              INTEGER(KIND=4) :: LIST(NBLACK)
              INTEGER(KIND=4) :: RBORDER(N)
              INTEGER(KIND=4) :: FIRST
              INTEGER(KIND=4) :: LEVEL
              INTEGER(KIND=4) :: LEVPTR(NLEVPTR)
              INTEGER(KIND=4) :: LROWPTR(NBLACK)
            END SUBROUTINE XMDMRGD
          END INTERFACE 
        END MODULE XMDMRGD__genmod
