        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:12 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDORDNG__genmod
          INTERFACE 
            SUBROUTINE XMDORDNG(IA,JA,LORDER,NEQ,NJA,NORDER,IERR)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQ
              INTEGER(KIND=4) :: IA(NEQ+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: LORDER(NEQ)
              INTEGER(KIND=4) :: NORDER
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE XMDORDNG
          END INTERFACE 
        END MODULE XMDORDNG__genmod
