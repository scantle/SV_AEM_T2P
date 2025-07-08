        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSVREC__genmod
          INTERFACE 
            SUBROUTINE UBDSVREC(IUNSTR,N,NODES,NCLNNDS,IBNDCB,ICLNCB,   &
     &NBNDVL,LAUX,NAUX,Q,VAL,IBOUND,NCOL,NROW,NLAY)
              INTEGER(KIND=4), INTENT(IN) :: NBNDVL
              INTEGER(KIND=4), INTENT(IN) :: NCLNNDS
              INTEGER(KIND=4), INTENT(IN) :: NODES
              INTEGER(KIND=4), INTENT(IN) :: IUNSTR
              INTEGER(KIND=4), INTENT(IN) :: N
              INTEGER(KIND=4), INTENT(IN) :: IBNDCB
              INTEGER(KIND=4), INTENT(IN) :: ICLNCB
              INTEGER(KIND=4), INTENT(IN) :: LAUX
              INTEGER(KIND=4), INTENT(IN) :: NAUX
              REAL(KIND=4), INTENT(IN) :: Q
              REAL(KIND=4), INTENT(IN) :: VAL(NBNDVL)
              INTEGER(KIND=4), INTENT(IN) :: IBOUND(NODES+NCLNNDS)
              INTEGER(KIND=4), INTENT(IN) :: NCOL
              INTEGER(KIND=4), INTENT(IN) :: NROW
              INTEGER(KIND=4), INTENT(IN) :: NLAY
            END SUBROUTINE UBDSVREC
          END INTERFACE 
        END MODULE UBDSVREC__genmod
