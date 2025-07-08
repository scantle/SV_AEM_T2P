        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDGTRED__genmod
          INTERFACE 
            SUBROUTINE XMDGTRED(A,XX,B,IA,JA,REDORDER,NJA,N,NRED)
              INTEGER(KIND=4) :: NRED
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NJA
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: XX(N)
              REAL(KIND=8) :: B(N)
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: REDORDER(NRED)
            END SUBROUTINE XMDGTRED
          END INTERFACE 
        END MODULE XMDGTRED__genmod
