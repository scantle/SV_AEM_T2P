        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDCGSTB__genmod
          INTERFACE 
            SUBROUTINE XMDCGSTB(A,B,X,AF,SOLN,CTOL,RRCTOL,IA,JA,IAF,JAF,&
     &IDIAGF,RBORDER,NBLACK,NRED,N,NJA,NJAF,NITMAX,NORTH,IERR)
              INTEGER(KIND=4) :: NJAF
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NBLACK
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: B(N)
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: AF(NJAF)
              REAL(KIND=8) :: SOLN(NBLACK)
              REAL(KIND=8) :: CTOL
              REAL(KIND=8) :: RRCTOL
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: IAF(NBLACK+1)
              INTEGER(KIND=4) :: JAF(NJAF)
              INTEGER(KIND=4) :: IDIAGF(NBLACK)
              INTEGER(KIND=4) :: RBORDER(N)
              INTEGER(KIND=4) :: NRED
              INTEGER(KIND=4) :: NITMAX
              INTEGER(KIND=4) :: NORTH
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE XMDCGSTB
          END INTERFACE 
        END MODULE XMDCGSTB__genmod
