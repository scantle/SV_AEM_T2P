        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:01 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGNCB2BCFU1BDADJ1__genmod
          INTERFACE 
            SUBROUTINE SGNCB2BCFU1BDADJ1(NGNCB,GNCB,IRGNCB,ISYMGNCB,    &
     &MXADJB,MXGNCB,BOTB,ICONSTRAINTB)
              INTEGER(KIND=4) :: MXGNCB
              INTEGER(KIND=4) :: MXADJB
              INTEGER(KIND=4) :: NGNCB
              REAL(KIND=4) :: GNCB(4+2*MXADJB,MXGNCB)
              INTEGER(KIND=4) :: IRGNCB(MXADJB,MXGNCB)
              INTEGER(KIND=4) :: ISYMGNCB
              REAL(KIND=4) :: BOTB(MXGNCB)
              INTEGER(KIND=4) :: ICONSTRAINTB
            END SUBROUTINE SGNCB2BCFU1BDADJ1
          END INTERFACE 
        END MODULE SGNCB2BCFU1BDADJ1__genmod
