        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:12 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDSOLV__genmod
          INTERFACE 
            SUBROUTINE XMDSOLV(A,B,X,CTOL,RRCTOL,IA,JA,NJA,N,NORTH,     &
     &NITMAX,IACL,IERR)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: NJA
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: B(N)
              REAL(KIND=8) :: X(N)
              REAL(KIND=8) :: CTOL
              REAL(KIND=8) :: RRCTOL
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: NORTH
              INTEGER(KIND=4) :: NITMAX
              INTEGER(KIND=4) :: IACL
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE XMDSOLV
          END INTERFACE 
        END MODULE XMDSOLV__genmod
