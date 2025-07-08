        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:12 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDNFCTR__genmod
          INTERFACE 
            SUBROUTINE XMDNFCTR(A,B,IA,JA,NJA,N,IERR)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NJA
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: B(N)
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE XMDNFCTR
          END INTERFACE 
        END MODULE XMDNFCTR__genmod
