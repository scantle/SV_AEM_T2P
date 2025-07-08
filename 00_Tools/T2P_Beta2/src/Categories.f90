module m_categories
  use m_vstring, only: t_vstring, vstring_toupper, vstring_new
  use m_vstringlist
  implicit none
  
  integer,parameter   :: DEFAULT_CATLIST_SIZE=10
!-------------------------------------------------------------------------------------------------!

  type :: t_category
    integer                         :: dtype          ! Data type, 0-int, 1-str
    integer                         :: narr           ! Number of unique arrays
    integer                         :: ncat           ! Number of categories
    integer,allocatable             :: catlist(:,:)   ! List of category values
    integer,allocatable             :: catlist_glo(:) ! List of category values
    type(t_vstringlist),allocatable :: strlist(:)     ! Category values, but when string
    type(t_vstringlist)             :: strlist_glo    ! Category strings for ALL arrays
    integer,allocatable             :: lay2arr(:)     ! Translation from layer to array
    integer,allocatable             :: array(:,:)     ! Arrays of category values (nvalues, narr)
    integer,allocatable             :: arrncat(:)     ! Number of categories per array
    logical                         :: from_file      ! Categories were read from file
    
    contains
      procedure, public        :: read_file
      procedure, public        :: initialize
      procedure, private       :: no_category_setup
      procedure, pass          :: add_category_int
      procedure, pass          :: add_category_str
      procedure, pass          :: add_category_char
      generic, private         :: add_category => add_category_int,add_category_str,add_category_char
      procedure, pass          :: get_id_char
      procedure,private        :: reallocate_catlist
      procedure,private        :: reallocate_catlist_glo
      generic, public          :: get_id => get_id_char
      procedure,private        :: finalize
  end type
!-------------------------------------------------------------------------------------------------!
  contains
  
!-------------------------------------------------------------------------------------------------!
! CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!
  subroutine initialize(this, narr, nlayers, nnodes, datatype)
    ! Called by this%read_file()
    implicit none
    class(t_category)     :: this
    integer,intent(in)    :: narr, datatype, nlayers, nnodes
    integer               :: i
    
    this%dtype = datatype
    this%narr = narr
    allocate(this%lay2arr (nlayers     ), &
             this%array   (nnodes, narr), &  ! Assumes uniform size layers
             this%arrncat (narr        ), &
             this%catlist_glo(DEFAULT_CATLIST_SIZE), &
             this%catlist(DEFAULT_CATLIST_SIZE, narr)) 
    if (this%dtype==1) then
      allocate(this%strlist(narr))
      call vstrlist_new(this%strlist_glo)
      do i=1, narr
        call vstrlist_new(this%strlist(i))
      end do
    end if
    
    ! By Default
    this%ncat        =  0
    this%lay2arr     =  1
    this%array       =  1
    this%catlist     = -1
    this%catlist_glo = -1
    this%arrncat     =  0
  
  end subroutine initialize

!-------------------------------------------------------------------------------------------------!
  
  subroutine no_category_setup(this)
    class(t_category), intent(inout) :: this
    
    this%ncat = 1
    this%catlist(1,:) = 1
    this%catlist_glo(1) = 1
    this%arrncat(:) = 1
    if (this%dtype==1) then
      call vstrlist_append(this%strlist(1), '')  ! Necessary?
      call vstrlist_append(this%strlist_glo, '')  ! Necessary?
    end if
  
  end subroutine no_category_setup
  
!-------------------------------------------------------------------------------------------------!

  subroutine add_category_int(this, value, arr, index)
    class(t_category), intent(inout) :: this
    integer, intent(in)              :: value, arr
    logical                          :: is_new, is_new_glo
    integer, intent(out)             :: index
    integer                          :: i, k

    is_new = .true.
    do k = 1, this%arrncat(arr)
      if (this%catlist(k,arr) == value) then
        is_new = .false.
        index      = value
        exit
      end if
    end do
    
    if (is_new) then
      ! Add to array categories
      if (this%arrncat(arr) >= size(this%catlist,1)) call this%reallocate_catlist()
      this%arrncat(arr)                   = this%arrncat(arr) + 1
      this%catlist(this%arrncat(arr),arr) = value
      index = value
      
      ! Is it globally new too?
      is_new_glo = .true.
      do k = 1, this%ncat
        if (this%catlist_glo(k) == value) then
        is_new_glo = .false.
        exit
        end if
      end do
      if (is_new_glo) then
        if (this%ncat >= size(this%catlist_glo)) call this%reallocate_catlist_glo()
        this%ncat                   = this%ncat + 1
        this%catlist_glo(this%ncat) = value
      end if
    end if
  end subroutine add_category_int
  
!-------------------------------------------------------------------------------------------------!

  subroutine add_category_str(this, value, arr, index)
    class(t_category), intent(inout) :: this
    type(t_vstring), intent(in)      :: value
    integer, intent(in)              :: arr
    type(t_vstring)                  :: valupp
    integer, intent(out)             :: index
    integer                          :: loc_id, glo_id

    valupp = vstring_toupper(value)
    
    ! Get global id (if exists) for index
    glo_id = vstrlist_search(this%strlist_glo, valupp)
    if (glo_id == 0) then
      call vstrlist_append(this%strlist_glo, valupp)
      this%ncat = vstrlist_length(this%strlist_glo)
      glo_id    = this%ncat
    end if
    
    loc_id = vstrlist_search(this%strlist(arr), valupp)
    if (loc_id == 0) then
      call vstrlist_append(this%strlist(arr), valupp)
      loc_id = vstrlist_length(this%strlist(arr))
      this%arrncat(arr) = loc_id
      if (loc_id > size(this%catlist,1)) call reallocate_catlist(this)
      this%catlist(loc_id,arr) = glo_id
    end if
    
    index = glo_id
  end subroutine add_category_str
  
!-------------------------------------------------------------------------------------------------!

  subroutine add_category_char(this, value, arr, index)
    class(t_category), intent(inout) :: this
    character(*), intent(in)         :: value
    integer, intent(in)              :: arr
    type(t_vstring)                  :: valupp
    integer, intent(out)             :: index

    call vstring_new (valupp, trim(value))
    call this%add_category_str(valupp, arr, index)
  end subroutine add_category_char
  
!-------------------------------------------------------------------------------------------------!

  subroutine reallocate_catlist(this)
    class(t_category), intent(inout) :: this
    integer, allocatable :: temp_catlist(:,:)
    integer :: new_size

    new_size = size(this%catlist, 1) + DEFAULT_CATLIST_SIZE
    allocate(temp_catlist(new_size, size(this%catlist, 2)))
    temp_catlist(1:size(this%catlist, 1), :) = this%catlist
    call move_alloc(temp_catlist, this%catlist)
  end subroutine reallocate_catlist

!-------------------------------------------------------------------------------------------------!
  
  subroutine reallocate_catlist_glo(this)
    class(t_category), intent(inout) :: this
    integer, allocatable :: tmp(:)
    integer :: new_size

    new_size = size(this%catlist_glo) + DEFAULT_CATLIST_SIZE
    allocate(tmp(new_size))
    tmp(1:size(this%catlist_glo)) = this%catlist_glo
    tmp(size(this%catlist_glo)+1:) = -huge(1)
    call move_alloc(tmp, this%catlist_glo)
  end subroutine reallocate_catlist_glo
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine read_file(this, file, nlayers, nnodes, datatype, lay_aquitard_flag, arr_aquitard_flag)
    use m_file_io, only: t_file_reader, open_file_reader, item2int, item2char
    use m_error_handler, only: error_handler
    implicit none
    class(t_category), intent(inout) :: this
    character(*), intent(in)         :: file
    integer,intent(in)               :: datatype, nlayers, nnodes
    logical,intent(in)               :: lay_aquitard_flag(nlayers)
    logical,allocatable,intent(out)  :: arr_aquitard_flag(:)
    type(t_file_reader), pointer     :: reader
    type(t_vstringlist)              :: strings
    !type(t_vstring)                  :: temp
    integer                          :: lay2arr(nlayers)
    integer                          :: eof, ierr, narr, cidx, i, j, inode, itemp
    character(30),allocatable        :: chartemp(:)
    character(256)                   :: iomsg
    logical                          :: has_aquitard
    
    lay2arr = 0
    
    ! Check if a file was actually passed. If not, we'll just initialize and be done
    if (len(trim(file))>0) then
      this%from_file = .true.
      reader => open_file_reader(file)
      call reader%read_to_next_line(eof)
      !call reader%next_item(eof, strings)
      
      ! Free read version, allows for one "layer" per line (i.e., can list aquitard & aquifer for IWFM)
      read(reader%unit, *, iomsg=iomsg, iostat=ierr) lay2arr(:)
      if (ierr/=0) then
        if (ierr==59) then
          call error_handler(2,filename=file,opt_msg="Not enough array indices assigned: need one index for each layer")
        else
          call reader%iomsg_handler(iomsg)  ! Something weirder happened
        end if
      end if
      
      narr = maxval(lay2arr)
      call this%initialize(narr, nlayers, nnodes, datatype)
      ! Move to new lay2array
      this%lay2arr = lay2arr
    else
      this%from_file = .false.
      has_aquitard = any(lay_aquitard_flag)
      narr = 1 + merge(1, 0, has_aquitard)  ! narr = 2 if there's any aquitard, otherwise 1
      call this%initialize(narr, nlayers, nnodes, datatype)
      call this%no_category_setup()
      
      ! Initialize arr_aquitard_flag
      allocate(arr_aquitard_flag(narr))
      arr_aquitard_flag = .false.
      if (has_aquitard) arr_aquitard_flag(2) = .true.
      
      return                   ! Early return if no file
    end if
    
    ! Initialize arr_aquitard_flag
    allocate(arr_aquitard_flag(narr))
    allocate(chartemp(nnodes))
    arr_aquitard_flag = .false.  ! Default to aquifer
    
    ! Check for re-use of arrays between aquifers and aquitards
    do i = 1, nlayers
      if (lay_aquitard_flag(i) .and. .not. arr_aquitard_flag(lay2arr(i))) then
        arr_aquitard_flag(lay2arr(i)) = .true.
      elseif (.not. lay_aquitard_flag(i) .and. arr_aquitard_flag(lay2arr(i))) then
        call error_handler(1, filename=file, opt_msg="Array re-use between aquifer and aquitard layers is not allowed")
      end if
    end do
    
    ! Continue if file passed - Loop over arrays, skipping header
    do i=1, narr
      call reader%read_to_next_line(eof)  ! Skip header
      if (datatype==0) then ! INT
        ! Check for constant
        read(reader%unit,*) chartemp(1), itemp
        if (trim(chartemp(1))=="CONSTANT") then
          this%array(:,i) = itemp
          call this%add_category(itemp, i, cidx)
        else
          ! array
          backspace(reader%unit)
          read(reader%unit,*) this%array(:,i)
          do j=1, nnodes
            call this%add_category(this%array(j,i), i, cidx)
            !this%array(j,i) = cidx  ! Excessively confusing, e.g. cat 4 is 3...
          end do
        end if
      else if (datatype==1) then  ! STR/CHAR
        ! Check for constant
        read(reader%unit,*) chartemp(1), chartemp(2)
        if (trim(chartemp(1))=="CONSTANT") then
          call this%add_category(chartemp(2), i, cidx)
          this%array(:,i) = cidx
        else
          ! array
          backspace(reader%unit)
          read(reader%unit,*) chartemp
          do j=1, nnodes
            call this%add_category(chartemp(j), i, cidx)
            this%array(j,i) = cidx
          end do
        end if
      end if
    end do
    deallocate(chartemp)
    
  end subroutine read_file
  
!-------------------------------------------------------------------------------------------------!
  
  function get_id_char(this, value) result(index)
    ! Gets strlist index of pass character category
    implicit none
    class(t_category), intent(inout) :: this
    character(*),intent(in)          :: value
    type(t_vstring)                  :: valupp
    integer                          :: index

    call vstring_new (valupp, trim(value))
    valupp = vstring_toupper(valupp)
    index = vstrlist_search(this%strlist_glo, valupp)
  
  end function get_id_char

!-------------------------------------------------------------------------------------------------!
  
  subroutine finalize(this)
    implicit none
    class(t_category), intent(inout) :: this
  
    if (allocated(this%catlist)) deallocate(this%catlist)
    if (allocated(this%lay2arr)) deallocate(this%lay2arr)
    if (allocated(this%array))   deallocate(this%array)
    if (allocated(this%arrncat)) deallocate(this%arrncat)
    if (allocated(this%strlist))  deallocate(this%strlist)
    if (allocated(this%catlist_glo)) deallocate(this%catlist_glo)
    call vstrlist_free(this%strlist_glo)
  
  end subroutine finalize
  
!-------------------------------------------------------------------------------------------------!
end module m_categories