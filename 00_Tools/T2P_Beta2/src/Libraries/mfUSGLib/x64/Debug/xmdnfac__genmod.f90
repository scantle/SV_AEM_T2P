        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDNFAC__genmod
          INTERFACE 
            SUBROUTINE XMDNFAC(IA,JA,AF,N,NJA,NJAF,A,B,IDIAGF,IAF,JAF,  &
     &NBLACK,ICOLOUR,RBORDER,IBLACKEND)
              INTEGER(KIND=4) :: NBLACK
              INTEGER(KIND=4) :: NJAF
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              REAL(KIND=8) :: AF(NJAF)
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: B(N)
              INTEGER(KIND=4) :: IDIAGF(NBLACK)
              INTEGER(KIND=4) :: IAF(NBLACK+1)
              INTEGER(KIND=4) :: JAF(NJAF)
              INTEGER(KIND=4) :: ICOLOUR(N)
              INTEGER(KIND=4) :: RBORDER(N)
              INTEGER(KIND=4) :: IBLACKEND(N)
            END SUBROUTINE XMDNFAC
          END INTERFACE 
        END MODULE XMDNFAC__genmod
