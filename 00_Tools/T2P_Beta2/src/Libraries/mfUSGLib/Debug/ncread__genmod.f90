        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:13 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE NCREAD__genmod
          INTERFACE 
            SUBROUTINE NCREAD(IO,TXT,IERR)
              INTEGER(KIND=4), INTENT(INOUT) :: IO
              CHARACTER(LEN=256), INTENT(OUT) :: TXT
              INTEGER(KIND=4), INTENT(OUT) :: IERR
            END SUBROUTINE NCREAD
          END INTERFACE 
        END MODULE NCREAD__genmod
