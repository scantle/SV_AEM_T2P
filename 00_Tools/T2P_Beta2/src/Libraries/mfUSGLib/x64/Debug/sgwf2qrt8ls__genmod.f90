        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:48 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2QRT8LS__genmod
          INTERFACE 
            SUBROUTINE SGWF2QRT8LS(IN,IOUTU,QRTF,NQRTVL,MXQRT,NREAD,    &
     &MXAQRT,NQRTCL,QRTAUX,NCAUX,NAUX,IQRTFL,IUNSTR,NODQRT,MXRTCELLS)
              INTEGER(KIND=4) :: MXRTCELLS
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: MXQRT
              INTEGER(KIND=4) :: NQRTVL
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUTU
              REAL(KIND=4) :: QRTF(NQRTVL,MXQRT)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: MXAQRT
              INTEGER(KIND=4) :: NQRTCL
              CHARACTER(LEN=16) :: QRTAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
              INTEGER(KIND=4) :: IQRTFL
              INTEGER(KIND=4) :: IUNSTR
              INTEGER(KIND=4) :: NODQRT(MXRTCELLS)
            END SUBROUTINE SGWF2QRT8LS
          END INTERFACE 
        END MODULE SGWF2QRT8LS__genmod
