        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GENRCM__genmod
          INTERFACE 
            SUBROUTINE GENRCM(NEQNS,NJA,XADJ,ADJNCY,PERM,MASK,XLS)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQNS
              INTEGER(KIND=4) :: XADJ(NEQNS+1)
              INTEGER(KIND=4) :: ADJNCY(NJA)
              INTEGER(KIND=4) :: PERM(NEQNS)
              INTEGER(KIND=4) :: MASK(NEQNS)
              INTEGER(KIND=4) :: XLS(NEQNS+1)
            END SUBROUTINE GENRCM
          END INTERFACE 
        END MODULE GENRCM__genmod
