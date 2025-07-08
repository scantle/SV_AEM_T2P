        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSV3U__genmod
          INTERFACE 
            SUBROUTINE UBDSV3U(KSTP,KPER,TEXT,IBDCHN,BUFF,IBUFF,IDIM,   &
     &NOPT,NODES,IOUT,DELT,PERTIM,TOTIM,IBOUND)
              INTEGER(KIND=4) :: NODES
              INTEGER(KIND=4) :: IDIM
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: IBDCHN
              REAL(KIND=4) :: BUFF(NODES)
              INTEGER(KIND=4) :: IBUFF(IDIM)
              INTEGER(KIND=4) :: NOPT
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=8) :: DELT
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: IBOUND(NODES)
            END SUBROUTINE UBDSV3U
          END INTERFACE 
        END MODULE UBDSV3U__genmod
