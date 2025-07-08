        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:12 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MD__genmod
          INTERFACE 
            SUBROUTINE MD(N,NJA,IA,JA,MMAX,V,L,HEAD,LAST,NEXT,MARK,FLAG)
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
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE MD
          END INTERFACE 
        END MODULE MD__genmod
