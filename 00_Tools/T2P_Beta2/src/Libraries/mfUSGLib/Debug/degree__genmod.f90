        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:11 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DEGREE__genmod
          INTERFACE 
            SUBROUTINE DEGREE(LLS,NEQNS,NJA,ROOT,XADJ,ADJNCY,MASK,DEG,  &
     &CCSIZE,LS)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQNS
              INTEGER(KIND=4) :: LLS
              INTEGER(KIND=4) :: ROOT
              INTEGER(KIND=4) :: XADJ(NEQNS+1)
              INTEGER(KIND=4) :: ADJNCY(NJA)
              INTEGER(KIND=4) :: MASK(NEQNS)
              INTEGER(KIND=4) :: DEG(NEQNS)
              INTEGER(KIND=4) :: CCSIZE
              INTEGER(KIND=4) :: LS(LLS)
            END SUBROUTINE DEGREE
          END INTERFACE 
        END MODULE DEGREE__genmod
