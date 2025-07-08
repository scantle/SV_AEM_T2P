        !COMPILER-GENERATED INTERFACE MODULE: Sun May 18 20:00:24 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE UZFLOW__genmod
          INTERFACE 
            SUBROUTINE UZFLOW(I,SURFLUX,DLENGTH,ZOLDIST,DEPTH,THETA,FLUX&
     &,SPEED,ITRWAVE,LTRAIL,TOTALFLUX,NUMWAVES,THETAR,THETAS,FKSAT,EPS, &
     &OLDSFLX,JPNT,DELTINC)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTOTRL,                                                &
     &          NSFRSETS,                                               &
     &          NSTRAIL,                                                &
     &          THETAB,                                                 &
     &          FLUXB
              INTEGER(KIND=4) :: I
              REAL(KIND=8) :: SURFLUX
              REAL(KIND=8) :: DLENGTH
              REAL(KIND=8) :: ZOLDIST
              REAL(KIND=8) :: DEPTH(NSTOTRL)
              REAL(KIND=8) :: THETA(NSTOTRL)
              REAL(KIND=8) :: FLUX(NSTOTRL)
              REAL(KIND=8) :: SPEED(NSTOTRL)
              INTEGER(KIND=4) :: ITRWAVE(NSTOTRL)
              INTEGER(KIND=4) :: LTRAIL(NSTOTRL)
              REAL(KIND=8) :: TOTALFLUX
              INTEGER(KIND=4) :: NUMWAVES
              REAL(KIND=8) :: THETAR
              REAL(KIND=8) :: THETAS
              REAL(KIND=4) :: FKSAT
              REAL(KIND=8) :: EPS
              REAL(KIND=8) :: OLDSFLX
              INTEGER(KIND=4) :: JPNT
              REAL(KIND=8) :: DELTINC
            END SUBROUTINE UZFLOW
          END INTERFACE 
        END MODULE UZFLOW__genmod
