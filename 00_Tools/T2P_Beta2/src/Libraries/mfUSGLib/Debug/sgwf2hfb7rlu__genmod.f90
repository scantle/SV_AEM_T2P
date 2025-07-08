        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:20 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2HFB7RLU__genmod
          INTERFACE 
            SUBROUTINE SGWF2HFB7RLU(NLIST,HFB,LSTBEG,MXHFB,INPACK,IOUT, &
     &LABEL,NODES,NODLAY,NLAY,IPRFLG)
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: MXHFB
              INTEGER(KIND=4) :: NLIST
              REAL(KIND=4) :: HFB(7,MXHFB)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: INPACK
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: LABEL
              INTEGER(KIND=4) :: NODES
              INTEGER(KIND=4) :: NODLAY(0:NLAY)
              INTEGER(KIND=4) :: IPRFLG
            END SUBROUTINE SGWF2HFB7RLU
          END INTERFACE 
        END MODULE SGWF2HFB7RLU__genmod
