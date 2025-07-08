        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:05 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SOLVERS__genmod
          INTERFACE 
            SUBROUTINE SOLVERS(IOUT,KITER,ICNVG,KSTP,KPER,AMAT,SOLN,RHS,&
     &IBND,STOL,VNOFLO,ITP,NEQS,NJA,ILUFLAG,IN_ITER)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQS
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: KITER
              INTEGER(KIND=4) :: ICNVG
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=8) :: AMAT(NJA)
              REAL(KIND=8) :: SOLN(NEQS)
              REAL(KIND=8) :: RHS(NEQS)
              INTEGER(KIND=4) :: IBND(NEQS)
              REAL(KIND=8) :: STOL
              REAL(KIND=4) :: VNOFLO
              INTEGER(KIND=4) :: ITP
              INTEGER(KIND=4) :: ILUFLAG
              INTEGER(KIND=4) :: IN_ITER
            END SUBROUTINE SOLVERS
          END INTERFACE 
        END MODULE SOLVERS__genmod
