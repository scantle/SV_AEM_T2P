        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:13 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QREAD__genmod
          INTERFACE 
            SUBROUTINE QREAD(R,NI,AIN,IERR)
              REAL(KIND=8), INTENT(OUT) :: R(25)
              INTEGER(KIND=4), INTENT(IN) :: NI
              CHARACTER(LEN=256), INTENT(IN) :: AIN
              INTEGER(KIND=4), INTENT(OUT) :: IERR
            END SUBROUTINE QREAD
          END INTERFACE 
        END MODULE QREAD__genmod
