        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MDM__genmod
          INTERFACE 
            SUBROUTINE MDM(N,MMAX,VK,TAIL,V,L,LAST,NEXT,MARK)
              INTEGER(KIND=4) :: MMAX
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: VK
              INTEGER(KIND=4) :: TAIL
              INTEGER(KIND=4) :: V(MMAX)
              INTEGER(KIND=4) :: L(MMAX)
              INTEGER(KIND=4) :: LAST(N)
              INTEGER(KIND=4) :: NEXT(N)
              INTEGER(KIND=4) :: MARK(N)
            END SUBROUTINE MDM
          END INTERFACE 
        END MODULE MDM__genmod
