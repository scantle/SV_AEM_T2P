        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:01 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GNCB2DISU1AR__genmod
          INTERFACE 
            SUBROUTINE GNCB2DISU1AR(IN,TEXT,MXGNCB,GNCB,IRGNCB,ISYMGNCB,&
     &MXADJB,IADJMATB,IPRGNCB)
              INTEGER(KIND=4) :: MXADJB
              INTEGER(KIND=4) :: MXGNCB
              INTEGER(KIND=4) :: IN
              CHARACTER(LEN=3) :: TEXT
              REAL(KIND=4) :: GNCB(4+2*MXADJB,MXGNCB)
              INTEGER(KIND=4) :: IRGNCB(MXADJB,MXGNCB)
              INTEGER(KIND=4) :: ISYMGNCB
              INTEGER(KIND=4) :: IADJMATB
              INTEGER(KIND=4) :: IPRGNCB
            END SUBROUTINE GNCB2DISU1AR
          END INTERFACE 
        END MODULE GNCB2DISU1AR__genmod
