        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE U1DINTNJA__genmod
          INTERFACE 
            SUBROUTINE U1DINTNJA(IARRAY,IAG,ANAME,NJAG,IN,IOUT,IDSYMRD)
              USE GLOBAL, ONLY :                                        &
     &          IA,                                                     &
     &          JA,                                                     &
     &          JAS,                                                    &
     &          NJAS,                                                   &
     &          NODES,                                                  &
     &          NEQS,                                                   &
     &          NJA,                                                    &
     &          ISYM
              INTEGER(KIND=4) :: IARRAY(NJAS)
              INTEGER(KIND=4) :: IAG(NODES+1)
              CHARACTER(LEN=24) :: ANAME
              INTEGER(KIND=4) :: NJAG
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: IDSYMRD
            END SUBROUTINE U1DINTNJA
          END INTERFACE 
        END MODULE U1DINTNJA__genmod
