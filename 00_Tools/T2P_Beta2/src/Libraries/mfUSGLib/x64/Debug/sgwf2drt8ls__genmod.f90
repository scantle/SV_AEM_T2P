        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:49 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2DRT8LS__genmod
          INTERFACE 
            SUBROUTINE SGWF2DRT8LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,    &
     &MXADRT,NDRTCL,DRTAUX,NCAUX,NAUX,IDRTFL,IUNSTR)
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: MXDRT
              INTEGER(KIND=4) :: NDRTVL
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUTU
              REAL(KIND=4) :: DRTF(NDRTVL,MXDRT)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: MXADRT
              INTEGER(KIND=4) :: NDRTCL
              CHARACTER(LEN=16) :: DRTAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: IDRTFL
              INTEGER(KIND=4) :: IUNSTR
            END SUBROUTINE SGWF2DRT8LS
          END INTERFACE 
        END MODULE SGWF2DRT8LS__genmod
