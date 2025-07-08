        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MDI__genmod
          INTERFACE 
            SUBROUTINE MDI(N,NJA,IA,JA,MMAX,V,L,HEAD,LAST,NEXT,MARK,TAG,&
     &FLAG)
              INTEGER(KIND=4) :: MMAX
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: V(MMAX)
              INTEGER(KIND=4) :: L(MMAX)
              INTEGER(KIND=4) :: HEAD(N)
              INTEGER(KIND=4) :: LAST(N)
              INTEGER(KIND=4) :: NEXT(N)
              INTEGER(KIND=4) :: MARK(N)
              INTEGER(KIND=4) :: TAG
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE MDI
          END INTERFACE 
        END MODULE MDI__genmod
