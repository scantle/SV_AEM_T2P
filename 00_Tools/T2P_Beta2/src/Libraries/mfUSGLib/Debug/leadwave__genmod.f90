        !COMPILER-GENERATED INTERFACE MODULE: Sun May 18 20:00:24 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LEADWAVE__genmod
          INTERFACE 
            SUBROUTINE LEADWAVE(NUMWAVES,TIME,TOTALFLUX,ITESTER,FLUX,   &
     &THETA,SPEED,DEPTH,ITRWAVE,LTRAIL,FKSAT,EPS,THETAS,THETAR,SURFLUX, &
     &OLDSFLX,JPNT,FEPS2,ITRAILFLG,DELTINC)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTOTRL,                                                &
     &          NEARZEROSFR,                                            &
     &          CLOSEZEROSFR,                                           &
     &          THETAB,                                                 &
     &          FLUXB,                                                  &
     &          FLUXHLD2
              INTEGER(KIND=4) :: NUMWAVES
              REAL(KIND=8) :: TIME
              REAL(KIND=8) :: TOTALFLUX
              INTEGER(KIND=4) :: ITESTER
              REAL(KIND=8) :: FLUX(NSTOTRL)
              REAL(KIND=8) :: THETA(NSTOTRL)
              REAL(KIND=8) :: SPEED(NSTOTRL)
              REAL(KIND=8) :: DEPTH(NSTOTRL)
              INTEGER(KIND=4) :: ITRWAVE(NSTOTRL)
              INTEGER(KIND=4) :: LTRAIL(NSTOTRL)
              REAL(KIND=4) :: FKSAT
              REAL(KIND=8) :: EPS
              REAL(KIND=8) :: THETAS
              REAL(KIND=8) :: THETAR
              REAL(KIND=8) :: SURFLUX
              REAL(KIND=8) :: OLDSFLX
              INTEGER(KIND=4) :: JPNT
              REAL(KIND=8) :: FEPS2
              INTEGER(KIND=4) :: ITRAILFLG
              REAL(KIND=8) :: DELTINC
            END SUBROUTINE LEADWAVE
          END INTERFACE 
        END MODULE LEADWAVE__genmod
