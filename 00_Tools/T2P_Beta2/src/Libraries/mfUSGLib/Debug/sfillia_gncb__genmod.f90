        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:37 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SFILLIA_GNCB__genmod
          INTERFACE 
            SUBROUTINE SFILLIA_GNCB(IAT,JAT,NEQS,NJA,IGFOUNDB,NGNCB,GNCB&
     &,MXADJB,MXGNCB)
              INTEGER(KIND=4) :: MXGNCB
              INTEGER(KIND=4) :: MXADJB
              INTEGER(KIND=4) :: NGNCB
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: NEQS
              INTEGER(KIND=4) :: IAT(NEQS+1)
              INTEGER(KIND=4) :: JAT(NJA)
              INTEGER(KIND=4) :: IGFOUNDB(NGNCB,MXADJB)
              REAL(KIND=4) :: GNCB(4+2*MXADJB,MXGNCB)
            END SUBROUTINE SFILLIA_GNCB
          END INTERFACE 
        END MODULE SFILLIA_GNCB__genmod
