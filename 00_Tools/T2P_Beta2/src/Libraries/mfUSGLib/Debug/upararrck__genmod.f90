        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:37 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UPARARRCK__genmod
          INTERFACE 
            SUBROUTINE UPARARRCK(BUFF,IBOUND,IOUT,LAY,NCOL,NLAY,NROW,   &
     &IUNSTR,PTYP)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: BUFF(NCOL,NROW)
              INTEGER(KIND=4) :: IBOUND(NCOL,NROW,NLAY)
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: LAY
              INTEGER(KIND=4) :: IUNSTR
              CHARACTER(LEN=4) :: PTYP
            END SUBROUTINE UPARARRCK
          END INTERFACE 
        END MODULE UPARARRCK__genmod
