        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
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
