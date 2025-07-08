        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:04:00 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SGWF2GAG7SO__genmod
          INTERFACE 
            SUBROUTINE SGWF2GAG7SO(IUNITGWT,IUNITUZF,GAGETM,COUT,SFRQ,  &
     &IBD,NSOL)
              USE GWFSFRMODULE, ONLY :                                  &
     &          NSTRM,                                                  &
     &          NUMAVE,                                                 &
     &          IDIVAR,                                                 &
     &          STRM,                                                   &
     &          ISEG,                                                   &
     &          SEG,                                                    &
     &          SGOTFLW,                                                &
     &          AVWAT,                                                  &
     &          WAT1,                                                   &
     &          AVDPT
              INTEGER(KIND=4) :: NSOL
              INTEGER(KIND=4) :: IUNITGWT
              INTEGER(KIND=4) :: IUNITUZF
              REAL(KIND=4) :: GAGETM
              REAL(KIND=4) :: COUT(NSTRM,NSOL)
              REAL(KIND=4) :: SFRQ(5,NSTRM)
              INTEGER(KIND=4) :: IBD
            END SUBROUTINE SGWF2GAG7SO
          END INTERFACE 
        END MODULE SGWF2GAG7SO__genmod
