        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
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
