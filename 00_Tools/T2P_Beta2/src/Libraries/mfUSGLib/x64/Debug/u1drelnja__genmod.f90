        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:03 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE U1DRELNJA__genmod
          INTERFACE 
            SUBROUTINE U1DRELNJA(ARRAY,IAG,ANAME,NJAG,IN,IOUT,IDSYMRD)
              USE GLOBAL, ONLY :                                        &
     &          IA,                                                     &
     &          JA,                                                     &
     &          JAS,                                                    &
     &          NJAS,                                                   &
     &          NODES,                                                  &
     &          NEQS,                                                   &
     &          NJA,                                                    &
     &          ISYM
              REAL(KIND=4) :: ARRAY(NJAS)
              INTEGER(KIND=4) :: IAG(NODES+1)
              CHARACTER(LEN=24) :: ANAME
              INTEGER(KIND=4) :: NJAG
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: IDSYMRD
            END SUBROUTINE U1DRELNJA
          END INTERFACE 
        END MODULE U1DRELNJA__genmod
