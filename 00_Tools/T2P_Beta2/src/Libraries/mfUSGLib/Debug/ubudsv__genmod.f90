        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UBUDSV__genmod
          INTERFACE 
            SUBROUTINE UBUDSV(KSTP,KPER,TEXT,IBDCHN,BUFF,NCOL,NROW,NLAY,&
     &IOUT)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: IBDCHN
              REAL(KIND=4) :: BUFF(NCOL,NROW,NLAY)
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE UBUDSV
          END INTERFACE 
        END MODULE UBUDSV__genmod
