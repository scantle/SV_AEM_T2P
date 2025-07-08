        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE U1DRELNJAU__genmod
          INTERFACE 
            SUBROUTINE U1DRELNJAU(ARRAY1,ARRAY2,ANAME,IAG,NJAG,IN,IOUT)
              USE GLOBAL, ONLY :                                        &
     &          IA,                                                     &
     &          JA,                                                     &
     &          JAS,                                                    &
     &          NJAS,                                                   &
     &          NODES,                                                  &
     &          NEQS,                                                   &
     &          NJA,                                                    &
     &          ISYM
              REAL(KIND=4) :: ARRAY1(NJAS)
              REAL(KIND=4) :: ARRAY2(NJAS)
              CHARACTER(LEN=24) :: ANAME
              INTEGER(KIND=4) :: IAG(NODES+1)
              INTEGER(KIND=4) :: NJAG
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE U1DRELNJAU
          END INTERFACE 
        END MODULE U1DRELNJAU__genmod
