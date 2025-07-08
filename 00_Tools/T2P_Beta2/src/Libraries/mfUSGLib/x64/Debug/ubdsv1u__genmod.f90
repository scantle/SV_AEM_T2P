        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBDSV1U__genmod
          INTERFACE 
            SUBROUTINE UBDSV1U(KSTP,KPER,TEXT,IBDCHN,BUFF,NJA,IOUT,DELT,&
     &PERTIM,TOTIM,IBOUND,NODES)
              INTEGER(KIND=4) :: NODES
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: IBDCHN
              REAL(KIND=4) :: BUFF(NJA)
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=8) :: DELT
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: IBOUND(NODES)
            END SUBROUTINE UBDSV1U
          END INTERFACE 
        END MODULE UBDSV1U__genmod
