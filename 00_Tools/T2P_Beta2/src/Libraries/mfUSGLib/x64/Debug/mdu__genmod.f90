        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MDU__genmod
          INTERFACE 
            SUBROUTINE MDU(N,MMAX,EK,DMIN,V,L,HEAD,LAST,NEXT,MARK)
              INTEGER(KIND=4) :: MMAX
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: EK
              INTEGER(KIND=4) :: DMIN
              INTEGER(KIND=4) :: V(MMAX)
              INTEGER(KIND=4) :: L(MMAX)
              INTEGER(KIND=4) :: HEAD(N)
              INTEGER(KIND=4) :: LAST(N)
              INTEGER(KIND=4) :: NEXT(N)
              INTEGER(KIND=4) :: MARK(N)
            END SUBROUTINE MDU
          END INTERFACE 
        END MODULE MDU__genmod
