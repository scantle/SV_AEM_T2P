        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSVBU__genmod
          INTERFACE 
            SUBROUTINE UBDSVBU(IBDCHN,NODES,N,Q,VAL,NVL,NAUX,LAUX,IBOUND&
     &)
              INTEGER(KIND=4) :: NVL
              INTEGER(KIND=4) :: NODES
              INTEGER(KIND=4) :: IBDCHN
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: Q
              REAL(KIND=4) :: VAL(NVL)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: LAUX
              INTEGER(KIND=4) :: IBOUND(NODES)
            END SUBROUTINE UBDSVBU
          END INTERFACE 
        END MODULE UBDSVBU__genmod
