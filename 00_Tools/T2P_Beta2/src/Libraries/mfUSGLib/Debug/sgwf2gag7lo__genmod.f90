        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:32 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2GAG7LO__genmod
          INTERFACE 
            SUBROUTINE SGWF2GAG7LO(IUNITGWT,IUNITUZF,CLAKE,GAGETM,GWIN, &
     &GWOUT,SEEP,FLXINL,VOLOLD,CLKOLD,CLAKINIT,NSOL)
              USE GWFLAKMODULE, ONLY :                                  &
     &          NLAKES,                                                 &
     &          RNF,                                                    &
     &          VOL,                                                    &
     &          STGNEW,                                                 &
     &          PRECIP,                                                 &
     &          EVAP,                                                   &
     &          SURFIN,                                                 &
     &          SURFOT,                                                 &
     &          WITHDRW,                                                &
     &          SUMCNN,                                                 &
     &          DELH,                                                   &
     &          TDELH,                                                  &
     &          VOLINIT,                                                &
     &          OVRLNDRNF,                                              &
     &          TSLAKERR,                                               &
     &          CMLAKERR,                                               &
     &          DELVOL,                                                 &
     &          SEEPUZ,                                                 &
     &          SURFA
              INTEGER(KIND=4) :: NSOL
              INTEGER(KIND=4) :: IUNITGWT
              INTEGER(KIND=4) :: IUNITUZF
              REAL(KIND=4) :: CLAKE(NLAKES,NSOL)
              REAL(KIND=4) :: GAGETM
              REAL(KIND=4) :: GWIN(NLAKES)
              REAL(KIND=4) :: GWOUT(NLAKES)
              REAL(KIND=8) :: SEEP(NLAKES)
              REAL(KIND=4) :: FLXINL(NLAKES)
              REAL(KIND=4) :: VOLOLD(NLAKES)
              REAL(KIND=4) :: CLKOLD(1,1)
              REAL(KIND=4) :: CLAKINIT(1,1)
            END SUBROUTINE SGWF2GAG7LO
          END INTERFACE 
        END MODULE SGWF2GAG7LO__genmod
