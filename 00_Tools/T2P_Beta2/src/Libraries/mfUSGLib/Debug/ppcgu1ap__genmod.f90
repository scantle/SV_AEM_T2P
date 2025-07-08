        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:15 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PPCGU1AP__genmod
          INTERFACE 
            SUBROUTINE PPCGU1AP(AC,RHS,HNEW,IAC,JAC,ICNVG,KSTP,KPER,    &
     &MXITER,KITER,IN_ITER,IOUT)
              USE PPCGUMODULE
              REAL(KIND=8) ,TARGET, INTENT(INOUT) :: AC(NNZC)
              REAL(KIND=8), INTENT(INOUT) :: RHS(NIAC)
              REAL(KIND=8), INTENT(INOUT) :: HNEW(NIAC)
              INTEGER(KIND=4) ,TARGET, INTENT(IN) :: IAC(NIAC+1)
              INTEGER(KIND=4) ,TARGET, INTENT(IN) :: JAC(NNZC)
              INTEGER(KIND=4), INTENT(INOUT) :: ICNVG
              INTEGER(KIND=4), INTENT(IN) :: KSTP
              INTEGER(KIND=4), INTENT(IN) :: KPER
              INTEGER(KIND=4), INTENT(IN) :: MXITER
              INTEGER(KIND=4), INTENT(IN) :: KITER
              INTEGER(KIND=4), INTENT(INOUT) :: IN_ITER
              INTEGER(KIND=4), INTENT(IN) :: IOUT
            END SUBROUTINE PPCGU1AP
          END INTERFACE 
        END MODULE PPCGU1AP__genmod
