        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:11 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ROOTLS__genmod
          INTERFACE 
            SUBROUTINE ROOTLS(LLS,NEQNS,NJA,ROOT,XADJ,ADJNCY,MASK,NLVL, &
     &XLS,LS)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQNS
              INTEGER(KIND=4) :: LLS
              INTEGER(KIND=4) :: ROOT
              INTEGER(KIND=4) :: XADJ(NEQNS+1)
              INTEGER(KIND=4) :: ADJNCY(NJA)
              INTEGER(KIND=4) :: MASK(NEQNS)
              INTEGER(KIND=4) :: NLVL
              INTEGER(KIND=4) :: XLS(NEQNS+1)
              INTEGER(KIND=4) :: LS(LLS)
            END SUBROUTINE ROOTLS
          END INTERFACE 
        END MODULE ROOTLS__genmod
