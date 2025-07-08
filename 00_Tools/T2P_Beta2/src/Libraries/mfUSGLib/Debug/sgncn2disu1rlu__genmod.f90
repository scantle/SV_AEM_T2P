        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:15 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGNCN2DISU1RLU__genmod
          INTERFACE 
            SUBROUTINE SGNCN2DISU1RLU(NLIST,GNCN,LSTBEG,MXGNCN,INPACK,  &
     &IOUT,LABEL,NEQS,IPRFLG,I2KN,LGNCN,IRGNCN,ISYMGNCN,MXADJN,IFLALPHAN&
     &)
              INTEGER(KIND=4) :: MXADJN
              INTEGER(KIND=4) :: MXGNCN
              INTEGER(KIND=4) :: NLIST
              REAL(KIND=4) :: GNCN(2*MXADJN+3,MXGNCN)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: INPACK
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: LABEL
              INTEGER(KIND=4) :: NEQS
              INTEGER(KIND=4) :: IPRFLG
              INTEGER(KIND=4) :: I2KN
              INTEGER(KIND=4) :: LGNCN(MXGNCN)
              INTEGER(KIND=4) :: IRGNCN(2,MXADJN,MXGNCN)
              INTEGER(KIND=4) :: ISYMGNCN
              INTEGER(KIND=4) :: IFLALPHAN
            END SUBROUTINE SGNCN2DISU1RLU
          END INTERFACE 
        END MODULE SGNCN2DISU1RLU__genmod
