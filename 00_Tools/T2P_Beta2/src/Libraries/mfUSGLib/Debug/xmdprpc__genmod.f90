        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:12 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDPRPC__genmod
          INTERFACE 
            SUBROUTINE XMDPRPC(IA,JA,NJA,NN,NORDER,IERR,REDSYS)
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: IA(NN+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: NORDER
              INTEGER(KIND=4) :: IERR
              LOGICAL(KIND=4) :: REDSYS
            END SUBROUTINE XMDPRPC
          END INTERFACE 
        END MODULE XMDPRPC__genmod
