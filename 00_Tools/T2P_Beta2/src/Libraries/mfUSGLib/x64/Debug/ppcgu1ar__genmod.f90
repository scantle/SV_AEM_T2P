        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:47 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PPCGU1AR__genmod
          INTERFACE 
            SUBROUTINE PPCGU1AR(IN,NJA,NEQS,MXITER,HICLOSE,ITER1,IPRSMS,&
     &IFDPARAM,IPCGUM)
              INTEGER(KIND=4), INTENT(IN) :: IN
              INTEGER(KIND=4), INTENT(IN) :: NJA
              INTEGER(KIND=4), INTENT(IN) :: NEQS
              INTEGER(KIND=4), INTENT(IN) :: MXITER
              REAL(KIND=8), INTENT(IN) :: HICLOSE
              INTEGER(KIND=4), INTENT(IN) :: ITER1
              INTEGER(KIND=4) :: IPRSMS
              INTEGER(KIND=4), INTENT(IN) :: IFDPARAM
              INTEGER(KIND=4), INTENT(INOUT) :: IPCGUM
            END SUBROUTINE PPCGU1AR
          END INTERFACE 
        END MODULE PPCGU1AR__genmod
