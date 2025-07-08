        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:06 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TRAILWAVE__genmod
          INTERFACE 
            SUBROUTINE TRAILWAVE(NUMWAVES,I,FLUX,THETA,SPEED,DEPTH,     &
     &ITRWAVE,LTRAIL,FKSAT,EPS,THETAS,THETAR,SURFLUX,JPNT)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTOTRL,                                                &
     &          NSTRAIL,                                                &
     &          NSFRSETS,                                               &
     &          NEARZEROSFR,                                            &
     &          FLUXHLD2,                                               &
     &          FLUXB,                                                  &
     &          THETAB
              INTEGER(KIND=4) :: NUMWAVES
              INTEGER(KIND=4) :: I
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
              INTEGER(KIND=4) :: JPNT
            END SUBROUTINE TRAILWAVE
          END INTERFACE 
        END MODULE TRAILWAVE__genmod
