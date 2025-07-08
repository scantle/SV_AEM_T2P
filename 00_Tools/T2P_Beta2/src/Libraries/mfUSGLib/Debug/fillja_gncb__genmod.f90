        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:37 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FILLJA_GNCB__genmod
          INTERFACE 
            SUBROUTINE FILLJA_GNCB(IGFOUNDB,NGNCB,GNCB,MXADJB,MXGNCB)
              INTEGER(KIND=4) :: MXGNCB
              INTEGER(KIND=4) :: MXADJB
              INTEGER(KIND=4) :: NGNCB
              INTEGER(KIND=4) :: IGFOUNDB(NGNCB,MXADJB)
              REAL(KIND=4) :: GNCB(4+2*MXADJB,MXGNCB)
            END SUBROUTINE FILLJA_GNCB
          END INTERFACE 
        END MODULE FILLJA_GNCB__genmod
