        !COMPILER-GENERATED INTERFACE MODULE: Mon Oct  7 11:36:11 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE XMDRBMTV__genmod
          INTERFACE 
            SUBROUTINE XMDRBMTV(A,XX,AMLTX,IA,JA,RBORDER,REDORDER,N,NJA,&
     &NBLACK,NRED)
              INTEGER(KIND=4) :: NRED
              INTEGER(KIND=4) :: NBLACK
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(NJA)
              REAL(KIND=8) :: XX(N)
              REAL(KIND=8) :: AMLTX(NBLACK)
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: RBORDER(NBLACK)
              INTEGER(KIND=4) :: REDORDER(NRED)
            END SUBROUTINE XMDRBMTV
          END INTERFACE 
        END MODULE XMDRBMTV__genmod
