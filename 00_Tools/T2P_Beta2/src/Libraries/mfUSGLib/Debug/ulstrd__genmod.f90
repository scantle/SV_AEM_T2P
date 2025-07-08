        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:29 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ULSTRD__genmod
          INTERFACE 
            SUBROUTINE ULSTRD(NLIST,RLIST,LSTBEG,LDIM,MXLIST,IAL,INPACK,&
     &IOUT,LABEL,CAUX,NCAUX,NAUX,IFREFM,NCOL,NROW,NLAY,ISCLOC1,ISCLOC2, &
     &IPRFLG)
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: MXLIST
              INTEGER(KIND=4) :: LDIM
              INTEGER(KIND=4) :: NLIST
              REAL(KIND=4) :: RLIST(LDIM,MXLIST)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: IAL
              INTEGER(KIND=4) :: INPACK
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: LABEL
              CHARACTER(LEN=16) :: CAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: IFREFM
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: ISCLOC1
              INTEGER(KIND=4) :: ISCLOC2
              INTEGER(KIND=4) :: IPRFLG
            END SUBROUTINE ULSTRD
          END INTERFACE 
        END MODULE ULSTRD__genmod
