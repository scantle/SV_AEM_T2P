        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:15 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGNCN2DISU1SUB__genmod
          INTERFACE 
            SUBROUTINE SGNCN2DISU1SUB(IN,PACK,IOUTU,PTYP,GNCN,MXGNCN,   &
     &MXADJN,MXACTGNN,NGNCN,IFLALPHAN,LABEL)
              INTEGER(KIND=4) :: MXADJN
              INTEGER(KIND=4) :: MXGNCN
              INTEGER(KIND=4) :: IN
              CHARACTER(*) :: PACK
              INTEGER(KIND=4) :: IOUTU
              CHARACTER(*) :: PTYP
              REAL(KIND=4) :: GNCN(2*MXADJN+3,MXGNCN)
              INTEGER(KIND=4) :: MXACTGNN
              INTEGER(KIND=4) :: NGNCN
              INTEGER(KIND=4) :: IFLALPHAN
              CHARACTER(*) :: LABEL
            END SUBROUTINE SGNCN2DISU1SUB
          END INTERFACE 
        END MODULE SGNCN2DISU1SUB__genmod
