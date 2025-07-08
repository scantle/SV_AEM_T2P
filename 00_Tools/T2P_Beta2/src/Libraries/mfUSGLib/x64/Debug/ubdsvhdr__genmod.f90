        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSVHDR__genmod
          INTERFACE 
            SUBROUTINE UBDSVHDR(IUNSTR,KSTP,KPER,IOUT,IBNDCB,ICLNCB,    &
     &NODES,NCLNNDS,NCOL,NROW,NLAY,NBND,NBNDVL,NAUX,IBOUND,TEXT,BNDAUX, &
     &DELT,PERTIM,TOTIM,BND)
              INTEGER(KIND=4), INTENT(IN) :: NAUX
              INTEGER(KIND=4), INTENT(IN) :: NBNDVL
              INTEGER(KIND=4), INTENT(IN) :: NBND
              INTEGER(KIND=4), INTENT(IN) :: NCLNNDS
              INTEGER(KIND=4), INTENT(IN) :: NODES
              INTEGER(KIND=4), INTENT(IN) :: IUNSTR
              INTEGER(KIND=4), INTENT(IN) :: KSTP
              INTEGER(KIND=4), INTENT(IN) :: KPER
              INTEGER(KIND=4), INTENT(IN) :: IOUT
              INTEGER(KIND=4), INTENT(IN) :: IBNDCB
              INTEGER(KIND=4), INTENT(IN) :: ICLNCB
              INTEGER(KIND=4), INTENT(IN) :: NCOL
              INTEGER(KIND=4), INTENT(IN) :: NROW
              INTEGER(KIND=4), INTENT(IN) :: NLAY
              INTEGER(KIND=4), INTENT(IN) :: IBOUND(NODES+NCLNNDS)
              CHARACTER(LEN=16), INTENT(IN) :: TEXT
              CHARACTER(LEN=16), INTENT(IN) :: BNDAUX(NAUX)
              REAL(KIND=8), INTENT(IN) :: DELT
              REAL(KIND=8), INTENT(IN) :: PERTIM
              REAL(KIND=8), INTENT(IN) :: TOTIM
              REAL(KIND=4), INTENT(IN) :: BND(NBNDVL,NBND)
            END SUBROUTINE UBDSVHDR
          END INTERFACE 
        END MODULE UBDSVHDR__genmod
