        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:09 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UPARLSTSUB__genmod
          INTERFACE 
            SUBROUTINE UPARLSTSUB(IN,PACK,IOUTU,PTYP,RLIST,LSTVL,LSTDIM,&
     &NREAD,MXLST,NTOT,IPVL1,IPVL2,LABEL,CAUX,NCAUX,NAUX)
              INTEGER(KIND=4) :: NCAUX
              INTEGER(KIND=4) :: LSTDIM
              INTEGER(KIND=4) :: LSTVL
              INTEGER(KIND=4) :: IN
              CHARACTER(*) :: PACK
              INTEGER(KIND=4) :: IOUTU
              CHARACTER(*) :: PTYP
              REAL(KIND=4) :: RLIST(LSTVL,LSTDIM)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: MXLST
              INTEGER(KIND=4) :: NTOT
              INTEGER(KIND=4) :: IPVL1
              INTEGER(KIND=4) :: IPVL2
              CHARACTER(*) :: LABEL
              CHARACTER(LEN=16) :: CAUX(NCAUX)
              INTEGER(KIND=4) :: NAUX
            END SUBROUTINE UPARLSTSUB
          END INTERFACE 
        END MODULE UPARLSTSUB__genmod
