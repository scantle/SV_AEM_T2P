module str_module
  implicit none
  
  contains

!-------------------------------------------------------------------------------------------------!
    SUBROUTINE multisplit(IFAIL,NUM,LW,RW,CLINE)

! -- Subroutine multisplit splits a string into blank-delimited fragments.

! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       num:      input parameter representing the number of elements or fragments into which the string will be split
!       lw:       output parameter that stores the left positions or indices of the fragments after splitting the string. The array LW has a size of NUM.
!       rw:       output parameter that stores the right positions or indices of the fragments after splitting the string. The array RW also has a size of NUM.
!       cline:    input parameter of type character that represents the input string to be split into fragments

! -- Author:-
!       John Doherty

       INTEGER IFAIL,NW,NBLC,J,I
       INTEGER NUM,NBLNK
       INTEGER LW(NUM),RW(NUM)
       CHARACTER*(*) CLINE
       IFAIL=0
       NW=0
       NBLC=LEN_TRIM(CLINE)
       IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
         CALL TABREM(CLINE)
         NBLC=LEN_TRIM(CLINE)
       ENDIF
       IF(NBLC.EQ.0) THEN
         IFAIL=-1
         RETURN
       END IF
       J=0
5      IF(NW.EQ.NUM) RETURN
       DO 10 I=J+1,NBLC
         IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.&
         (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10     CONTINUE
       IFAIL=1
       RETURN
20     NW=NW+1
       LW(NW)=I
       DO 30 I=LW(NW)+1,NBLC
         IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.&
         (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30     CONTINUE
       RW(NW)=NBLC
       IF(NW.LT.NUM) IFAIL=1
       RETURN
40     RW(NW)=I-1
       J=RW(NW)
       GO TO 5

    END subroutine multisplit
    
!-------------------------------------------------------------------------------------------------!

function count_fragments(line) result(count)
  implicit none
  ! Counts the number of string "fragments". Intended to be a preprocessor to multisplit.
  ! Spaces and tabs can separate values but does not consider commas (commented out)
  !
  ! Author: Leland Scantlebury
  !
  ! Arguments:
  ! - line: input string to be split into fragments
  CHARACTER(*), INTENT(IN)  :: line
  INTEGER                   :: count
  INTEGER                   :: i, length

  length = LEN_TRIM(line)
  count = 0
  i = 1
  DO WHILE (i<= length)
    IF (line(i:i) == ' ' .OR. ICHAR(line(i:i)) == 9) THEN  ! .OR. line(i:i) == ','
      i = i + 1
      CYCLE
    END IF
    count = count + 1

    DO WHILE (i <= length .AND. &
              (line(i:i) /= ' '  .AND. ICHAR(line(i:i)) /= 9))  ! .AND. line(i:i) /= ','
      i = i + 1
    END DO
  END DO
  return
end function count_fragments

!-------------------------------------------------------------------------------------------------!

  subroutine TABREM(CLINE)

! -- Subroutine TABREM removes tabs from a string.

! -- Subroutine arguments are as follows:-
!       cline:    character string


       INTEGER I
       CHARACTER*(*) CLINE

       DO 10 I=1,LEN(CLINE)
10     IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

       RETURN
  end subroutine tabrem
  
!-------------------------------------------------------------------------------------------------!
  
function to_upper(strIn) result(strOut)
! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
! Original author: Clive Page

  implicit none

  character(len=*), intent(in) :: strIn
  character(len=len(strIn)) :: strOut
  integer :: i,j

  do i = 1, len(strIn)
      j = iachar(strIn(i:i))
      if (j>= iachar("a") .and. j<=iachar("z") ) then
            strOut(i:i) = achar(iachar(strIn(i:i))-32)
      else
            strOut(i:i) = strIn(i:i)
      end if
  end do

end function to_upper

!-------------------------------------------------------------------------------------------------!
end module str_module