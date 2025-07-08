        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:49 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2DRT8LR__genmod
          INTERFACE 
            SUBROUTINE SGWF2DRT8LR(NLIST,DRTF,LSTBEG,NDRTVL,MXDRT,INPACK&
     &,IOUT,DRTAUX,NCAUX,NAUX,IFREFM,NCOL,NROW,NLAY,ITERP,IDRTFL,IUNSTR,&
     &NEQS)
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: MXDRT
              INTEGER(KIND=4) :: NDRTVL
              INTEGER(KIND=4) :: NLIST
              REAL(KIND=4) :: DRTF(NDRTVL,MXDRT)
              INTEGER(KIND=4) :: LSTBEG
              INTEGER(KIND=4) :: INPACK
              INTEGER(KIND=4) :: IOUT
              CHARACTER(LEN=16) :: DRTAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: IFREFM
              INTEGER(KIND=4) :: NCOL
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NLAY
              INTEGER(KIND=4) :: ITERP
              INTEGER(KIND=4) :: IDRTFL
              INTEGER(KIND=4) :: IUNSTR
              INTEGER(KIND=4) :: NEQS
            END SUBROUTINE SGWF2DRT8LR
          END INTERFACE 
        END MODULE SGWF2DRT8LR__genmod
