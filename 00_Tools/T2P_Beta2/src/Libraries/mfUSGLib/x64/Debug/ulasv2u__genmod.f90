        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULASV2U__genmod
          INTERFACE 
            SUBROUTINE ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,  &
     &NNDLAY,ILAY,ICHN,FMTOUT,LBLSAV,IBOUND,NODES)
              INTEGER(KIND=4) :: NODES
              REAL(KIND=4) :: BUFF(NODES)
              CHARACTER(LEN=16) :: TEXT
              INTEGER(KIND=4) :: KSTP
              INTEGER(KIND=4) :: KPER
              REAL(KIND=8) :: PERTIM
              REAL(KIND=8) :: TOTIM
              INTEGER(KIND=4) :: NSTRT
              INTEGER(KIND=4) :: NNDLAY
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: ICHN
              CHARACTER(LEN=20) :: FMTOUT
              INTEGER(KIND=4) :: LBLSAV
              INTEGER(KIND=4) :: IBOUND(NODES)
            END SUBROUTINE ULASV2U
          END INTERFACE 
        END MODULE ULASV2U__genmod
