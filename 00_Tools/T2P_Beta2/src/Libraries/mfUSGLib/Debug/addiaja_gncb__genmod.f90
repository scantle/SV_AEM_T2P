        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:37 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ADDIAJA_GNCB__genmod
          INTERFACE 
            SUBROUTINE ADDIAJA_GNCB(NGNCB,ISYMGNCB,MXADJB,MXGNCB,GNCB,  &
     &IRGNCB,IADJMATB)
              INTEGER(KIND=4) :: MXGNCB
              INTEGER(KIND=4) :: MXADJB
              INTEGER(KIND=4) :: NGNCB
              INTEGER(KIND=4) :: ISYMGNCB
              REAL(KIND=4) :: GNCB(4+2*MXADJB,MXGNCB)
              INTEGER(KIND=4) :: IRGNCB(MXADJB,MXGNCB)
              INTEGER(KIND=4) :: IADJMATB
            END SUBROUTINE ADDIAJA_GNCB
          END INTERFACE 
        END MODULE ADDIAJA_GNCB__genmod
