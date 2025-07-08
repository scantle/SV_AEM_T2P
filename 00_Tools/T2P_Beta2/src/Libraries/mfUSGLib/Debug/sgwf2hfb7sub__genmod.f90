        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:20 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2HFB7SUB__genmod
          INTERFACE 
            SUBROUTINE SGWF2HFB7SUB(IN,PACK,IOUTU,PTYP,HFB,LSTVL,MXHFB, &
     &MXACTFB,NHFB,IUNSTR,LABEL,LABEL2)
              INTEGER(KIND=4) :: MXHFB
              INTEGER(KIND=4) :: LSTVL
              INTEGER(KIND=4) :: IN
              CHARACTER(*) :: PACK
              INTEGER(KIND=4) :: IOUTU
              CHARACTER(*) :: PTYP
              REAL(KIND=4) :: HFB(LSTVL,MXHFB)
              INTEGER(KIND=4) :: MXACTFB
              INTEGER(KIND=4) :: NHFB
              INTEGER(KIND=4) :: IUNSTR
              CHARACTER(*) :: LABEL
              CHARACTER(*) :: LABEL2
            END SUBROUTINE SGWF2HFB7SUB
          END INTERFACE 
        END MODULE SGWF2HFB7SUB__genmod
