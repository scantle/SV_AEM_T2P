        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:08 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UPARARRSUB2__genmod
          INTERFACE 
            SUBROUTINE UPARARRSUB2(ZZ,NCOL,NROW,ILAY,NP,IN,IOUT,PTYP,   &
     &ANAME,PACK,IPF)
              INTEGER(KIND=4) :: NROW
              INTEGER(KIND=4) :: NCOL
              REAL(KIND=4) :: ZZ(NCOL,NROW)
              INTEGER(KIND=4) :: ILAY
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: IN
              INTEGER(KIND=4) :: IOUT
              CHARACTER(*) :: PTYP
              CHARACTER(LEN=24) :: ANAME
              CHARACTER(*) :: PACK
              INTEGER(KIND=4) :: IPF
            END SUBROUTINE UPARARRSUB2
          END INTERFACE 
        END MODULE UPARARRSUB2__genmod
