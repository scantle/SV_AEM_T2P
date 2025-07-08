        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBUDSVU__genmod
          INTERFACE 
            SUBROUTINE UBUDSVU(KSTP,KPER,TEXT,IBDCHN,BUFF,NJA,IOUT,     &
     &PERTIM,TOTIM)
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: IBDCHN
              REAL(KIND=4) :: BUFF(NJA)
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
            END SUBROUTINE UBUDSVU
          END INTERFACE 
        END MODULE UBUDSVU__genmod
