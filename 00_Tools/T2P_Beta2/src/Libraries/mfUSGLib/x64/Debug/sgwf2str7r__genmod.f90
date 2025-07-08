        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:42 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2STR7R__genmod
          INTERFACE 
            SUBROUTINE SGWF2STR7R(NLST,MXSTRM,STRM,ISTRM,LSTBEG,IN,IOUT,&
     &NCOL,NROW,NLAY,IRDFLG,NCAUX,NSTRVL,STRAUX,NAUX,IFREFM,IUNSTR,NODES&
     &)
              INTEGER(KIND=4) :: NSTRVL
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: MXSTRM
              INTEGER(KIND=4) :: NLST
              REAL(KIND=4) :: STRM(NSTRVL,MXSTRM)
              INTEGER(KIND=4) :: ISTRM(5,MXSTRM)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUT
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: IRDFLG
              CHARACTER(LEN=16) :: STRAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: IFREFM
              INTEGER(KIND=4) :: IUNSTR
              INTEGER(KIND=4) :: NODES
            END SUBROUTINE SGWF2STR7R
          END INTERFACE 
        END MODULE SGWF2STR7R__genmod
