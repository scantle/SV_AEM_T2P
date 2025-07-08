        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:12 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RCM__genmod
          INTERFACE 
            SUBROUTINE RCM(LLPERM,NEQNS,NJA,ROOT,XADJ,ADJNCY,MASK,PERM, &
     &CCSIZE,DEG)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQNS
              INTEGER(KIND=4) :: LLPERM
              INTEGER(KIND=4) :: ROOT
              INTEGER(KIND=4) :: XADJ(NEQNS+1)
              INTEGER(KIND=4) :: ADJNCY(NJA)
              INTEGER(KIND=4) :: MASK(NEQNS)
              INTEGER(KIND=4) :: PERM(LLPERM)
              INTEGER(KIND=4) :: CCSIZE
              INTEGER(KIND=4) :: DEG(NEQNS)
            END SUBROUTINE RCM
          END INTERFACE 
        END MODULE RCM__genmod
