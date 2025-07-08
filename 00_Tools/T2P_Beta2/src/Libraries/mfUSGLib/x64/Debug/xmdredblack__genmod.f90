        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDREDBLACK__genmod
          INTERFACE 
            SUBROUTINE XMDREDBLACK(IA,JA,LORDER,ICOLOUR,RBORDER,        &
     &IBLACKEND,NEQ,NJA,NBLACK,IERR,REDCDSYS)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQ
              INTEGER(KIND=4) :: IA(NEQ+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: LORDER(NEQ)
              INTEGER(KIND=4) :: ICOLOUR(NEQ)
              INTEGER(KIND=4) :: RBORDER(NEQ)
              INTEGER(KIND=4) :: IBLACKEND(NEQ)
              INTEGER(KIND=4) :: NBLACK
              INTEGER(KIND=4) :: IERR
              LOGICAL(KIND=4) :: REDCDSYS
            END SUBROUTINE XMDREDBLACK
          END INTERFACE 
        END MODULE XMDREDBLACK__genmod
