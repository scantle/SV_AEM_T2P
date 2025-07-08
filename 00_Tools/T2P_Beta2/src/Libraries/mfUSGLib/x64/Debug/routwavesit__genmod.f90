        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:07 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ROUTWAVESIT__genmod
          INTERFACE 
            SUBROUTINE ROUTWAVESIT(L,SEEP,H,HLD,THR,THETAS,FKS,EPSILON, &
     &ICALC,NWAVST,UZWDTH,UZFLWT,UZOLSFLX,UZSEEP,ITRLST,LTRLST,UZSPST,  &
     &UZFLST,UZDPST,UZTHST,ITRLIT,LTRLIT,UZSPIT,UZFLIT,UZDPIT,UZTHIT,   &
     &DELTINC,SBOT)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTOTRL,                                                &
     &          ISUZN,                                                  &
     &          STRM,                                                   &
     &          CLOSEZEROSFR,                                           &
     &          NEARZEROSFR
              INTEGER(KIND=4) :: L
              REAL(KIND=4) :: SEEP
              REAL(KIND=8) :: H
              REAL(KIND=8) :: HLD
              REAL(KIND=8) :: THR
              REAL(KIND=8) :: THETAS
              REAL(KIND=4) :: FKS
              REAL(KIND=8) :: EPSILON
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
              INTEGER(KIND=4) :: ITRLIT(NSTOTRL)
              INTEGER(KIND=4) :: LTRLIT(NSTOTRL)
              REAL(KIND=8) :: UZSPIT(NSTOTRL)
              REAL(KIND=8) :: UZFLIT(NSTOTRL)
              REAL(KIND=8) :: UZDPIT(NSTOTRL)
              REAL(KIND=8) :: UZTHIT(NSTOTRL)
              REAL(KIND=8) :: DELTINC
              REAL(KIND=8) :: SBOT
            END SUBROUTINE ROUTWAVESIT
          END INTERFACE 
        END MODULE ROUTWAVESIT__genmod
