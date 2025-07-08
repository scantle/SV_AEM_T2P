        !COMPILER-GENERATED INTERFACE MODULE: Sun May 18 20:00:23 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INIT_MODFLOW_USG__genmod
          INTERFACE 
            SUBROUTINE INIT_MODFLOW_USG(NAMFNAME,IERR,IOMSG,CMAXUNIT)
              CHARACTER(*) :: NAMFNAME
              INTEGER(KIND=4), INTENT(INOUT) :: IERR
              CHARACTER(LEN=256), INTENT(INOUT) :: IOMSG
              INTEGER(KIND=4), INTENT(INOUT) :: CMAXUNIT
            END SUBROUTINE INIT_MODFLOW_USG
          END INTERFACE 
        END MODULE INIT_MODFLOW_USG__genmod
