        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:48 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2QRT8LR__genmod
          INTERFACE 
            SUBROUTINE SGWF2QRT8LR(NLIST,QRTF,LSTBEG,NQRTVL,MXQRT,INPACK&
     &,IOUT,QRTAUX,NCAUX,NAUX,IFREFM,NCOL,NROW,NLAY,ITERP,IQRTFL,IUNSTR,&
     &NEQS,NODQRT,MXRTCELLS)
              INTEGER(KIND=4) :: MXRTCELLS
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: MXQRT
              INTEGER(KIND=4) :: NQRTVL
              INTEGER(KIND=4) :: NLIST
              REAL(KIND=4) :: QRTF(NQRTVL,MXQRT)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: INPACK
              INTEGER(KIND=4) :: IOUT
              CHARACTER(LEN=16) :: QRTAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: IFREFM
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: ITERP
              INTEGER(KIND=4) :: IQRTFL
              INTEGER(KIND=4) :: IUNSTR
              INTEGER(KIND=4) :: NEQS
              INTEGER(KIND=4) :: NODQRT(MXRTCELLS)
            END SUBROUTINE SGWF2QRT8LR
          END INTERFACE 
        END MODULE SGWF2QRT8LR__genmod
