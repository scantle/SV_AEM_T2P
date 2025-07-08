        !COMPILER-GENERATED INTERFACE MODULE: Tue Jun 17 11:03:40 2025
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ODRV__genmod
          INTERFACE 
            SUBROUTINE ODRV(IA,JA,P,IP,ISP,N,NJA,NSP,FLAG)
              INTEGER(KIND=4) :: NSP
              INTEGER(KIND=4) :: NJA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: IA(N+1)
              INTEGER(KIND=4) :: JA(NJA)
              INTEGER(KIND=4) :: P(N)
              INTEGER(KIND=4) :: IP(N)
              INTEGER(KIND=4) :: ISP(NSP)
              INTEGER(KIND=4) :: FLAG
            END SUBROUTINE ODRV
          END INTERFACE 
        END MODULE ODRV__genmod
