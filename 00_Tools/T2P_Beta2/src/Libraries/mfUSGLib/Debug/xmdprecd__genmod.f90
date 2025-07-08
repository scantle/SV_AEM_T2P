        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:11 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDPRECD__genmod
          INTERFACE 
            SUBROUTINE XMDPRECD(A,B,EPSRN,IA,JA,NJA,NN,LEVEL,IERR)
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: NJA
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: B(NN)
              REAL(KIND=8) :: EPSRN
              INTEGER(KIND=4) :: IA(NN+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: LEVEL
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE XMDPRECD
          END INTERFACE 
        END MODULE XMDPRECD__genmod
