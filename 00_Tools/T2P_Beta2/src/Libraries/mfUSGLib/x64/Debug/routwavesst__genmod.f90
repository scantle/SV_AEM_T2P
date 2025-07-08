        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:07 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ROUTWAVESST__genmod
          INTERFACE 
            SUBROUTINE ROUTWAVESST(L,SEEP,H,HLD,THR,THETAS,FKS,EPSILON, &
     &IWIDTHCHECK,SBOT,ICALC,NWAVST,UZWDTH,UZFLWT,UZOLSFLX,UZSEEP,ITRLST&
     &,LTRLST,UZSPST,UZFLST,UZDPST,UZTHST,DELTINC)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTOTRL,                                                &
     &          ISUZN,                                                  &
     &          STRM,                                                   &
     &          NEARZEROSFR
              INTEGER(KIND=4) :: L
              REAL(KIND=4) :: SEEP
              REAL(KIND=8) :: H
              REAL(KIND=8) :: HLD
              REAL(KIND=8) :: THR
              REAL(KIND=8) :: THETAS
              REAL(KIND=4) :: FKS
              REAL(KIND=8) :: EPSILON
              INTEGER(KIND=4) :: IWIDTHCHECK
              REAL(KIND=8) :: SBOT
              INTEGER(KIND=4) :: ICALC
              INTEGER(KIND=4) :: NWAVST(ISUZN)
              REAL(KIND=8) :: UZWDTH(ISUZN)
              REAL(KIND=8) :: UZFLWT(ISUZN)
              REAL(KIND=8) :: UZOLSFLX(ISUZN)
              REAL(KIND=8) :: UZSEEP(ISUZN)
              INTEGER(KIND=4) :: ITRLST(NSTOTRL)
              INTEGER(KIND=4) :: LTRLST(NSTOTRL)
              REAL(KIND=8) :: UZSPST(NSTOTRL)
              REAL(KIND=8) :: UZFLST(NSTOTRL)
              REAL(KIND=8) :: UZDPST(NSTOTRL)
              REAL(KIND=8) :: UZTHST(NSTOTRL)
              REAL(KIND=8) :: DELTINC
            END SUBROUTINE ROUTWAVESST
          END INTERFACE 
        END MODULE ROUTWAVESST__genmod
