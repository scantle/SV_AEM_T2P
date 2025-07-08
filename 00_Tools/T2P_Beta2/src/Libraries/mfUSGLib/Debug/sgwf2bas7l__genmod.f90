        !COMPILER-GENERATED INTERFACE MODULE: Sun May 18 20:00:23 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2BAS7L__genmod
          INTERFACE 
            SUBROUTINE SGWF2BAS7L(IPOS,LINE,LLOC,IOFLG,NLAY,IOUT,LABEL, &
     &INOC)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: IPOS
              CHARACTER(LEN=200) :: LINE
              INTEGER(KIND=4) :: LLOC
              INTEGER(KIND=4) :: IOFLG(NLAY,7)
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: LABEL
              INTEGER(KIND=4) :: INOC
            END SUBROUTINE SGWF2BAS7L
          END INTERFACE 
        END MODULE SGWF2BAS7L__genmod
