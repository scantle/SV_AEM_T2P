        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULASAVURD__genmod
          INTERFACE 
            SUBROUTINE ULASAVURD(BUF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT, &
     &NNDLAY,ILAY,ICHN,NODES)
              INTEGER(KIND=4) :: NODES
              REAL(KIND=4) :: BUF(NODES)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: NSTRT
              INTEGER(KIND=4) :: NNDLAY
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
            END SUBROUTINE ULASAVURD
          END INTERFACE 
        END MODULE ULASAVURD__genmod
