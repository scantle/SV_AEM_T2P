        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:45 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2HFB7RL__genmod
          INTERFACE 
            SUBROUTINE SGWF2HFB7RL(NLIST,HFB,LSTBEG,MXHFB,INPACK,IOUT,  &
     &LABEL,NCOL,NROW,NLAY,IPRFLG)
              INTEGER(KIND=4) :: MXHFB
              INTEGER(KIND=4) :: NLIST
              REAL(KIND=4) :: HFB(7,MXHFB)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: INPACK
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: LABEL
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: IPRFLG
            END SUBROUTINE SGWF2HFB7RL
          END INTERFACE 
        END MODULE SGWF2HFB7RL__genmod
