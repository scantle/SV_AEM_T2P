        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULAPRU__genmod
          INTERFACE 
            SUBROUTINE ULAPRU(BUF,TEXT,KSTP,KPER,NSTRT,NNDLAY,ILAY,IPRN,&
     &IOUT,PERTIM,TOTIM,NODES)
              INTEGER(KIND=4) :: NODES
              REAL(KIND=4) :: BUF(NODES)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              INTEGER(KIND=4) :: NSTRT
              INTEGER(KIND=4) :: NNDLAY
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: IPRN
              INTEGER(KIND=4) :: IOUT
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
            END SUBROUTINE ULAPRU
          END INTERFACE 
        END MODULE ULAPRU__genmod
