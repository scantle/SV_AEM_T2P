module m_file_io
  use m_vstring
  use m_vstringlist
  use list_module, only: stack, list_node
  implicit none

!-------------------------------------------------------------------------------------------------!
  type t_file_handler  ! Abstract Base Class
    
    character(100)           :: file
    integer                  :: unit
    integer                  :: ioerr
    
    contains
      procedure,private      :: iomsg_handler
      procedure,public       :: close_file
    
  end type t_file_handler
!-------------------------------------------------------------------------------------------------!
  type, extends(t_file_handler) :: t_file_reader
  
    type(stack)              :: backstack
  
    contains
      procedure,private      :: jump_to_external
      procedure,private      :: handle_eof
      procedure,public       :: read_to_next_line
      procedure,public       :: next_item
      procedure,public       :: skip
      procedure,public       :: next_block_id
      procedure,public       :: next_block_item
      procedure,public       :: get_block_len
      procedure,public       :: get_block_dim
      procedure,public       :: get_data_len
      procedure,public       :: back
    
  end type t_file_reader
!-------------------------------------------------------------------------------------------------!
    type, extends(t_file_handler) :: t_file_writer
  
    contains
      procedure,private     :: write_line_character, write_line_t_vstring
      generic,public        :: write_line => write_line_character, write_line_t_vstring
      procedure,private     :: write_valueline_int, write_valueline_real, write_valueline_char
      generic,public        :: write_valueline => write_valueline_int, write_valueline_real, write_valueline_char
    
  end type t_file_writer

!-------------------------------------------------------------------------------------------------!

  integer                     :: max_unit   ! Maximum unit currently open (starts at 9)
                                            ! (notably, this is not the maximum units allowed open)
  integer                     :: log_unit   ! Unit the log file is open on
                                            ! (so IO errors can be written to it)
  character(500)              :: line       ! For reading in lines from files
  INTEGER, PARAMETER          :: dp = SELECTED_REAL_KIND(15,307)  ! 64-bit real
  integer, parameter          :: log_width=38   
  
  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!
  
  subroutine io_initialize()
    implicit none
    
    ! Sets the initial unit number used for opening a file
    
    max_unit = 9
    log_unit = 0
    
  end subroutine io_initialize
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine get_command_args(main_fname)
    implicit none
    
    character(*),intent(inout)    :: main_fname
    
    ! For now, no flags or anything fancy - just if they pass a new main input file
    if (COMMAND_ARGUMENT_COUNT() > 0) then
      call GET_COMMAND_ARGUMENT(1, main_fname)
    else
      write (*, "(A)") "Must pass name of SWBM input file."
      stop
    end if
  
  end subroutine get_command_args

!-------------------------------------------------------------------------------------------------!
  
  integer function get_next_iunit() result(unit_no)
    implicit none
    logical      :: isopen
    isopen = .true.
    unit_no = max_unit
    do while (isopen)
      unit_no = unit_no + 1
      inquire(unit=unit_no, opened=isopen)
    end do
    return
    max_unit = unit_no
  end function get_next_iunit

!-------------------------------------------------------------------------------------------------!
  
  subroutine return_iunit(unit_no)  ! should re-name, maybe "de-register" ?
    implicit none
    integer, intent(in) :: unit_no
    
    ! TODO - more robust handling
    max_unit = unit_no - 1
    
  end subroutine return_iunit
  
!-------------------------------------------------------------------------------------------------!
  
  function open_file_reader(file) result(f)
    implicit none
    
    type(t_file_reader),pointer  :: f
    character(*), intent(in)   :: file
    character(256)             :: iomsg
    
    allocate(f)
    
    f%unit = get_next_iunit()
    f%file = trim(file)
    
    open(f%unit, file=f%file, action='READ', STATUS='OLD', iostat=f%ioerr, iomsg=iomsg)
    !write(log_unit,'(3a)') 'Opening file ', trim(f%file), ' for reading.'
    call f%iomsg_handler(iomsg)
    
    
  end function open_file_reader

!-------------------------------------------------------------------------------------------------!
  
  function open_file_writer(file) result(f)
    implicit none
    
    type(t_file_writer),pointer  :: f
    character(*), intent(in)   :: file
    character(256)             :: iomsg
    
    allocate(f)
    
    f%unit = get_next_iunit()
    f%file = trim(file)
    
    open(f%unit, file=f%file, action='WRITE', STATUS='REPLACE', iostat=f%ioerr, iomsg=iomsg)
    !if (log_unit > 0) write(log_unit,'(3a)') 'Opening file', trim(f%file), ' for writing.'
    call f%iomsg_handler(iomsg)
    
  end function open_file_writer

!-------------------------------------------------------------------------------------------------!
  
    ! Should these be here or in a different module??
  function item2int(strings, i) result(out_int)
    implicit none
    class(t_vstringlist),intent(in)   :: strings
    integer, intent(in)               :: i
    integer                           :: out_int
    
    call vstring_cast(vstrlist_index(strings, i), out_int)
  end function item2int
  
!-------------------------------------------------------------------------------------------------!
  
    ! Should these be here or in a different module??
  function item2real(strings, i) result(out_real)
    implicit none
    class(t_vstringlist),intent(in)   :: strings
    integer, intent(in)               :: i
    real                              :: out_real
    
    call vstring_cast(vstrlist_index(strings, i), out_real)
  end function item2real
  
!-------------------------------------------------------------------------------------------------!
  
    ! Should these be here or in a different module??
  function item2dp(strings, i) result(out_dp)
    implicit none
    class(t_vstringlist),intent(in)   :: strings
    integer, intent(in)               :: i
    real(dp)                          :: out_dp
    
    call vstring_cast(vstrlist_index(strings, i), out_dp)
  end function item2dp
  
!-------------------------------------------------------------------------------------------------!
  
    ! Should these be here or in a different module??
  subroutine item2char(strings, i, out_char, toupper)
    implicit none
    class(t_vstringlist),intent(in)   :: strings
    integer, intent(in)               :: i
    logical, optional                 :: toupper
    character(*),intent(out)          :: out_char
    
    if (present(toupper)) then
      if (toupper) then
        call vstring_cast(vstring_toupper(vstrlist_index(strings, i)), out_char)
      end if
    else
      call vstring_cast(vstrlist_index(strings, i), out_char)
    end if
  end subroutine item2char
  
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
  
    ! Should these be here or in a different module??
  function find_string_index_in_list(strings, i, list, toupper) result(index)
    implicit none
    ! Given input (strings(i)), find the index of it in the comma delimited character list "list"
    ! Useful when you have an input (e.g., TYPE IWFM) and you want to know which valid entry in
    ! a character list ("GRID,IWFM,MODFLOW") it is (in this case, IWFM is 2). Then a variable
    ! can be set using this value. If the input is not in the character list, it returns zero.
    class(t_vstringlist),intent(in)   :: strings
    integer, intent(in)               :: i
    character(*), intent(in)          :: list
    logical, optional                 :: toupper
    type(t_vstring)                   :: temp_str
    integer                           :: index
    
    call vstring_new(temp_str, list)
    if (present(toupper)) then
      if (toupper) then
        index = vstrlist_search(vstrlist_split(temp_str, ","), vstring_toupper(vstrlist_index(strings, i)) )
      end if
    else
      index = vstrlist_search(vstrlist_split(temp_str, ","), vstrlist_index(strings, i) )
    end if
    
  end function find_string_index_in_list
  
!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! BASE CLASS FILE TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!
  
  subroutine iomsg_handler(this,iomsg)
    implicit none
    class(t_file_handler)        :: this
    character(256),intent(in)    :: iomsg
    
    if (this%ioerr /= 0) then
      !write(*,'(a)') trim(iomsg)
      write(log_unit,'(3a)') 'IO Error - ', trim(iomsg)
      close(this%unit)
      error stop
    end if
    
  end subroutine iomsg_handler

!-------------------------------------------------------------------------------------------------!

  subroutine close_file(this)
    implicit none
    class(t_file_handler)        :: this
    close(this%unit)
    
    call return_iunit(this%unit)
    
    !deallocate(f)
    !nullify(f)
  end subroutine close_file
  
!-------------------------------------------------------------------------------------------------!
  
!-------------------------------------------------------------------------------------------------!
! READER TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!
  
  subroutine read_to_next_line(this, eof, skipped)
    implicit none
    class(t_file_reader)              :: this
    type(t_file_reader),pointer       :: next_file
    integer                           :: i, ierr
    integer,intent(out)               :: eof
    integer,intent(inout),optional    :: skipped
    
    ! For line splitting
    integer             :: left(2), right(2)

    eof = 0
    i = 0
    do while (eof == 0)
      read(this%unit, '(a500)', iostat=ierr) line
      line = adjustl(line)
      ! Handle EOF
      if (IS_IOSTAT_END(ierr)) then
        call this%handle_eof(eof)
      else if ((line(1:1) == '#').or.(line(1:1) == '*').or.(line(1:1) == '')) then
        ! Comment line, blank line
        i = i + 1
        continue
      else if (index(line, 'EXTERNAL')>0) then
        ! External command, continue reading in new file
        call multisplit(ierr,2,left,right,line)
        call this%jump_to_external(line(left(2):right(2)))
        i = 0  ! No lines skipped if it's a new file
        !call this%read_to_next_line(eof)
      else
        ! Data
        backspace(this%unit)
        exit
      end if
    end do
    
    if (present(skipped)) skipped = i
  
  end subroutine
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine jump_to_external(this, file)
    implicit none
    class(t_file_reader)  :: this
    character(*), intent(in) :: file
    character(256)           :: iomsg
    
    ! Store old iunit
    call this%backstack%push(this%unit)
    
    ! Update where we're reading from
    this%unit = get_next_iunit()
  
    open(this%unit, file=file, action='READ', STATUS='OLD', iostat=this%ioerr)
    write(log_unit,'(3a)') 'Opening external file', trim(file), ' for reading.'
    call this%iomsg_handler(iomsg)
    
  end subroutine jump_to_external
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine handle_eof(this, eof)
    implicit none
    class(t_file_reader)     :: this
    type(list_node)        :: prev
    integer, intent(inout) :: eof
    
    if (this%backstack%size > 0) then
      close(this%unit)
      prev = this%backstack%pop()
      this%unit  = prev%value
      eof = 0
    else
      eof = -1
    end if
    
  end subroutine handle_eof
  
!-------------------------------------------------------------------------------------------------!
  
    subroutine next_item(this, eof, strings)
    implicit none
    class(t_file_reader)              :: this
    integer,intent(out)               :: eof
    type(t_vstringlist)               :: temp
    type(t_vstringlist), intent(out)  :: strings
    type(t_vstring)                   :: line_string
    integer                           :: ierr, i

    ! Get to the next line
    call this%read_to_next_line(eof)
    if (eof == 0) then
    
      ! Read string
      read(this%unit, '(a500)', iostat=ierr) line
      ! TODO Error handling
      call vstring_new(line_string, trim(adjustl(line)))
      temp = vstrlist_split(line_string)
      ! Add all strings of non-zero length to strings object
      ! TODO: fix whatever causes vstringlist_split not to catch tabs followed by spaces
      call vstrlist_new ( strings )
      do i=1, vstrlist_length(temp)
        if (vstring_length(vstrlist_index(temp, i))>0) call vstrlist_append(strings, vstrlist_index(temp, i))
      end do
      call vstrlist_free(temp)
    else
      ! End of file - just return
    end if
  
  end subroutine next_item
  
!-------------------------------------------------------------------------------------------------!
  
    subroutine skip(this, nlines)
    implicit none
    class(t_file_reader)              :: this
    integer,intent(in)                :: nlines
    integer                           :: ierr, i, eof

    ! Get to the next line
    call this%read_to_next_line(eof)
    if (eof == 0) then
      do i=1, nlines    
        ! Read empty
        read(this%unit, '(a500)', iostat=ierr)
        ! TODO Error handling
      end do
    else
      ! End of file - just return
    end if
  
  end subroutine skip
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine next_block_id(this, eof, id)  
    implicit none
    class(t_file_reader)        :: this
    integer,intent(out)       :: eof
    character(*),intent(out)  :: id
    type(t_vstringlist)       :: slist

    ! Get next item in file
    call next_item(this, eof, slist)
    if (eof == 0) then
      ! Make sure there's enough there...
      if (vstrlist_length(slist) < 2) then
        ! ERROR
        stop
      end if
      !! Get block name
      if (vstring_equals(vstring_toupper(vstrlist_index(slist, 1)), "BEGIN")) then
        call vstring_cast(vstring_toupper(vstrlist_index(slist, 2)), id)
      end if
    else
      ! End of file - just return
    end if
  
  end subroutine next_block_id

!-------------------------------------------------------------------------------------------------!
  
  subroutine next_block_item(this, status, id, strings, length)
    implicit none
    class(t_file_reader)      :: this
    integer,intent(out)       :: status   ! 0 = Fine. -1 = EOF. +1 = End of Block
    integer,intent(out)       :: length
    character(*),intent(out)  :: id
    type(t_vstringlist)       :: strings

    ! Get next item in file
    call next_item(this, status, strings)
    if (status == 0) then
      
      !! Check for end
      call vstring_cast(vstring_toupper(vstrlist_index(strings, 1)), id)
      if (trim(id)=="END") then
        status = 1
        length = 1
      else
        length = vstrlist_length(strings)
      end if
    else
      status = -1
    end if
  
  end subroutine next_block_item

!-------------------------------------------------------------------------------------------------!
  
  function get_block_len(this) result(block_len)
    implicit none
    class(t_file_reader), intent(in) :: this
    integer                         :: i, block_len, eof, ierr
    integer                         :: status, skipped, total_skipped, length
    character(30)                   :: id
    type(t_vstringlist)             :: strings
    
    ! TODO what if file changes in block? (backspace may fail)

    status = 0
    total_skipped = 0
    block_len = -1  ! To account for reading END line
          
    ! Loop through the block items and count non-comment lines
    call this%read_to_next_line(eof, skipped=total_skipped)
    !read(this%unit, '(a500)', iostat=ierr) line
    if (index(line, "END") > 0.or.eof < 0) status = 1
    do while (status == 0)
      block_len = block_len + 1
      call this%read_to_next_line(eof, skipped=skipped)
      read(this%unit, '(a500)', iostat=ierr) line
      total_skipped = total_skipped + skipped + 1
      if (index(line, "END") > 0.or.eof < 0) status = 1
    end do
    
    ! Rewind!
    do i=1, total_skipped
      backspace(this%unit)
    end do
          
  end function get_block_len
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine get_block_dim(this, block_len, entries)
    ! Returns the number of non-comment, non-empty entries in a block, as well as block length
    implicit none
    class(t_file_reader), intent(in) :: this
    integer                         :: i, block_len, eof, ierr
    integer                         :: status, skipped, total_skipped, entries
    character(30)                   :: id
    type(t_vstringlist)             :: strings
    
    ! TODO what if file changes in block? (backspace may fail)

    status = 0
    total_skipped = 0
    block_len = -1  ! To account for reading END line
    entries = 0
          
    ! Loop through the block items and count non-comment lines
    call this%read_to_next_line(eof, skipped=total_skipped)
    !read(this%unit, '(a500)', iostat=ierr) line
    if (index(line, "END") > 0.or.eof < 0) status = 1
    do while (status == 0)
      block_len = block_len + 1
      call this%read_to_next_line(eof, skipped=skipped)
      call this%next_item(eof, strings)
      entries = entries + vstrlist_length(strings)
      total_skipped = total_skipped + skipped + 1
      if (vstring_equals(vstrlist_index(strings,1),"END").or.eof < 0) status = 1
    end do
    
    entries = entries - vstrlist_length(strings)  ! Subtract out END
    
    ! Rewind!
    do i=1, total_skipped
      backspace(this%unit)
    end do
          
  end subroutine get_block_dim

!-------------------------------------------------------------------------------------------------!
  
  function get_data_len(this,rewind_to_last_line) result(data_len)
    implicit none
    class(t_file_reader), intent(in) :: this
    integer                          :: i, data_len, eof, ierr
    character(200)                   :: line
    logical, optional                :: rewind_to_last_line

    data_len = 0
          
    ! Loop through and count non-comment lines
    call this%read_to_next_line(eof)
    do while (eof == 0)
      !call this%read_to_next_line(eof)  ! Inconsistent with how data files are usually read
      read(this%unit, '(a)', iostat=ierr) line
      if (IS_IOSTAT_END(ierr)) then
        eof = 1
      else if (trim(adjustl(line)) /= '') then
        ! not blank
        data_len = data_len + 1
      else
        ! blank - easiest implementation is to stop here
        exit
      end if
    end do
    
    if (present(rewind_to_last_line)) then
      if (rewind_to_last_line) then
        call this%back(2)
      end if
    end if
          
  end function get_data_len

!-------------------------------------------------------------------------------------------------!
  
  subroutine back(this, nlines)
    implicit none
    class(t_file_reader), intent(in) :: this
    integer,intent(in)               :: nlines
    integer                          :: i
    
    do i=1, nlines
      backspace(this%unit)
    end do
    
  end subroutine back
  
!-------------------------------------------------------------------------------------------------!
! WRITER TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!
  
  subroutine write_line_character(this, line, blank_start, blank_end)
    implicit none
    class(t_file_writer), intent(inout) :: this
    character(len=*), intent(in)        :: line
    integer,intent(in),optional         :: blank_start, blank_end
    integer                             :: i
    
    if (present(blank_start)) then
      do i=1, blank_start
        write(this%unit,*)
      end do
    end if
    
    write(this%unit, '(A)') trim(line)
    
    if (present(blank_end)) then
      do i=1, blank_end
        write(this%unit,*)
      end do
    end if
    
  end subroutine write_line_character

!-------------------------------------------------------------------------------------------------!
  
  subroutine write_line_t_vstring(this, line, blank_start, blank_end)
    implicit none
    class(t_file_writer), intent(inout) :: this
    type(t_vstring), intent(in)         :: line
    character(300)                      :: out_char
    integer,intent(in),optional         :: blank_start, blank_end
    integer                             :: i
    
    if (present(blank_start)) then
      do i=1, blank_start
        write(this%unit,*)
      end do
    end if
    
    call vstring_cast(line, out_char)
    
    write(this%unit, '(A)') trim(out_char)
    
    if (present(blank_end)) then
      do i=1, blank_end
        write(this%unit,*)
      end do
    end if
    
  end subroutine write_line_t_vstring

!-------------------------------------------------------------------------------------------------!
  
  subroutine write_valueline_int(this, line, value, fmt)
    implicit none
    class(t_file_writer), intent(inout) :: this
    character(*), intent(in)            :: line
    integer, intent(in)                 :: value
    character(*), optional              :: fmt
    character(24)                       :: fmt_use,full_fmt
    integer                             :: line_length, spaces
    
    fmt_use = 'i8'
    if (present(fmt)) fmt_use = trim(fmt)
    
    line_length = len_trim(line)
    spaces = log_width - line_length
    
    write(full_fmt, '(2(a,i2),7a)') '(1x,a', line_length, ',', spaces, 'x,', trim(fmt_use), ')'
    write(this%unit, full_fmt) adjustl(line), value
    
  end subroutine write_valueline_int

!-------------------------------------------------------------------------------------------------!
  
  subroutine write_valueline_real(this, line, value, fmt)
    implicit none
    class(t_file_writer), intent(inout) :: this
    character(*), intent(in)            :: line
    real(dp)                            :: value
    character(*), optional              :: fmt
    character(24)                       :: fmt_use,full_fmt
    integer                             :: line_length, spaces
    
    fmt_use = 'f8.2'
    if (present(fmt)) fmt_use = trim(fmt)
    
    line_length = len_trim(line)
    spaces = log_width - line_length
    
    write(full_fmt, '(2(a,i2),7a)') '(1x,a', line_length, ',', spaces, 'x,', trim(fmt_use), ')'
    write(this%unit, full_fmt) adjustl(line), value
    
  end subroutine write_valueline_real

!-------------------------------------------------------------------------------------------------!
  
  subroutine write_valueline_char(this, line, value, fmt)
    implicit none
    class(t_file_writer), intent(inout) :: this
    character(*), intent(in)            :: line
    character(*)                        :: value
    character(*), optional              :: fmt
    character(24)                       :: fmt_use,full_fmt
    integer                             :: line_length, spaces
    
    fmt_use = 'a'
    if (present(fmt)) fmt_use = trim(fmt)
    
    line_length = len_trim(line)
    spaces = log_width - line_length
    
    write(full_fmt, '(2(a,i2),7a)') '(1x,a', line_length, ',', spaces, 'x,', trim(fmt_use), ')'
    write(this%unit, full_fmt) adjustl(line), trim(value)
    
  end subroutine write_valueline_char

!-------------------------------------------------------------------------------------------------!

end module m_file_io