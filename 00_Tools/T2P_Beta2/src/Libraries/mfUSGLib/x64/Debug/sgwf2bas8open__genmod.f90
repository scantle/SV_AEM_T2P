        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:04 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2BAS8OPEN__genmod
          INTERFACE 
            SUBROUTINE SGWF2BAS8OPEN(INUNIT,IOUT,IUNIT,CUNIT,NIUNIT,    &
     &VERSION,INBAS,MAXUNIT,MFVNAM)
              INTEGER(KIND=4) :: NIUNIT
              INTEGER(KIND=4) :: INUNIT
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: IUNIT(NIUNIT)
              CHARACTER(LEN=4) :: CUNIT(NIUNIT)
              CHARACTER(*) :: VERSION
              INTEGER(KIND=4) :: INBAS
              INTEGER(KIND=4) :: MAXUNIT
              CHARACTER(*) :: MFVNAM
            END SUBROUTINE SGWF2BAS8OPEN
          END INTERFACE 
        END MODULE SGWF2BAS8OPEN__genmod
