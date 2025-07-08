        !COMPILER-GENERATED INTERFACE MODULE: Sun May 18 20:00:25 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UZMASSBAL__genmod
          INTERFACE 
            SUBROUTINE UZMASSBAL(L,H,HLD,THR,THETAS,EPSILON,FKS,UZDPST, &
     &UZTHST,UZSPST,UZFLST,LTRLST,ITRLST,UZFLWT,UZSTOR,DELSTOR,NWAVST,  &
     &UZOLSFLX,UZWDTH,WETPER,UZSEEP,RATIN,RATOUT,IN,FLOBOT,SBOT,STRLEN, &
     &TOTFLWT,TOTUZSTOR,TOTDELSTOR,IWIDTHCHECK,AVDPT,AVWAT,WAT1,IBD,    &
     &ICALC,DELTINC,IMASSROUTE,IUNITGAGE,GWFLOW)
              USE GWFSFRMODULE, ONLY :                                  &
     &          ISUZN,                                                  &
     &          NSTOTRL,                                                &
     &          NUMAVE,                                                 &
     &          STRM,                                                   &
     &          ITRLSTH,                                                &
     &          SFRUZBD,                                                &
     &          SUMLEAK,                                                &
     &          SUMRCH,                                                 &
     &          NEARZEROSFR,                                            &
     &          CLOSEZEROSFR
              INTEGER(KIND=4) :: L
              REAL(KIND=8) :: H
              REAL(KIND=8) :: HLD
              REAL(KIND=8) :: THR
              REAL(KIND=8) :: THETAS
              REAL(KIND=8) :: EPSILON
              REAL(KIND=4) :: FKS
              REAL(KIND=8) :: UZDPST(NSTOTRL)
              REAL(KIND=8) :: UZTHST(NSTOTRL)
              REAL(KIND=8) :: UZSPST(NSTOTRL)
              REAL(KIND=8) :: UZFLST(NSTOTRL)
              INTEGER(KIND=4) :: LTRLST(NSTOTRL)
              INTEGER(KIND=4) :: ITRLST(NSTOTRL)
              REAL(KIND=8) :: UZFLWT(ISUZN)
              REAL(KIND=8) :: UZSTOR(ISUZN)
              REAL(KIND=8) :: DELSTOR(ISUZN)
              INTEGER(KIND=4) :: NWAVST(ISUZN)
              REAL(KIND=8) :: UZOLSFLX(ISUZN)
              REAL(KIND=8) :: UZWDTH(ISUZN)
              REAL(KIND=8) :: WETPER(ISUZN)
              REAL(KIND=8) :: UZSEEP(ISUZN)
              REAL(KIND=8) :: RATIN
              REAL(KIND=8) :: RATOUT
              INTEGER(KIND=4) :: IN
              REAL(KIND=8) :: FLOBOT
              REAL(KIND=8) :: SBOT
              REAL(KIND=4) :: STRLEN
              REAL(KIND=8) :: TOTFLWT
              REAL(KIND=8) :: TOTUZSTOR
              REAL(KIND=8) :: TOTDELSTOR
              INTEGER(KIND=4) :: IWIDTHCHECK
              REAL(KIND=4) :: AVDPT(NUMAVE)
              REAL(KIND=4) :: AVWAT(NUMAVE)
              REAL(KIND=4) :: WAT1(NUMAVE)
              INTEGER(KIND=4) :: IBD
              INTEGER(KIND=4) :: ICALC
              REAL(KIND=8) :: DELTINC
              INTEGER(KIND=4) :: IMASSROUTE
              INTEGER(KIND=4) :: IUNITGAGE
              REAL(KIND=8) :: GWFLOW
            END SUBROUTINE UZMASSBAL
          END INTERFACE 
        END MODULE UZMASSBAL__genmod
