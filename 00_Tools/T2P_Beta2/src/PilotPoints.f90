module m_pilotpoints
  use m_datasets, only: t_dataset
  use m_global, only: NODATA
  use m_grid, only: t_grid, create_grid_object
  implicit none

  ! Module Variables                              !    1     2   3   4      5   6    7    8    9
  integer,parameter           :: nproperties = 9  ! Kmin, Kmax, Ss, Sy, Aniso, Kd, KHp, KVp, STp
  integer,parameter           :: global_props(3) = (/7,8,9/)
  integer,parameter           :: nglobal_props = size(global_props)
  integer,parameter           :: aquitard_props(6) = (/1,2,5,6,7,8/)
  integer,parameter           :: naquitard_props = size(aquitard_props)

!-------------------------------------------------------------------------------------------------!
  type, extends(t_dataset) :: t_pilotpoints
    ! For reference: inherited from t_dataset
    !integer,allocatable           :: category(:)      ! Zones, Hydrostratigraphic Units (HSUs)
    !type(t_grid),pointer          :: grid             ! Grid

    integer,pointer                :: pp_id(:)         ! ID value of each pilot point (index matches pilot_points)
    integer,allocatable            :: zonelist(:)      ! List of "categories" assigned to each pilot point (pilot point zone)
    real    ,allocatable           :: values(:,:,:)    ! Data values by parameter (point, parameter, class_id)

    ! Counters
    integer                        :: npp              ! Number of pilot points
    integer                        :: nppzones         ! Number of pilot point zones

    contains

      procedure,private            :: get_pp_index
      procedure,public             :: init_pilotpoints
      generic                      :: initialize => init_pilotpoints
      procedure,public             :: read_pp_line
      procedure,public             :: write_pp_table
      procedure,public             :: finalize => finalize_pilotpoints

  end type t_pilotpoints
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  function create_pilotpoint_object(npoints, nclasses) result(object)
    implicit none

    integer,intent(inout)            :: npoints,nclasses
    class(t_pilotpoints),pointer     :: object

    allocate(object)

    call object%initialize(npoints)

    ! Have to allocate this seperately, since init cannot take more than npoints (inheritance)
    ! TODO that didn't have to be true
    allocate(object%values(npoints, nproperties, nclasses))
    object%values   = NODATA

  end function

!-------------------------------------------------------------------------------------------------!
! LAYER CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine init_pilotpoints(this, npoints, grid)
    implicit none
    class(t_pilotpoints),intent(inout) :: this
    integer,intent(in)                 :: npoints
    class(t_grid),pointer,optional      :: grid

    allocate(this%category   (npoints))
    allocate(this%grid)

    this%npp            = npoints

    ! Intialize with default values
    this%nppzones        = 1
    this%category        = 1

    if (present(grid)) then
      this%grid => grid
    else
      this%grid => create_grid_object(2, npoints)  ! X,Y coords by n
    end if

  end subroutine init_pilotpoints

!-------------------------------------------------------------------------------------------------!

  function get_pp_index(this, pp_id) result(index)
    use m_error_handler, only: error_handler
    implicit none
    class(t_pilotpoints), intent(inout)   :: this
    integer,intent(in)                    :: pp_id
    integer                               :: index
    character(4)                          :: temp

    if (pp_id > this%npp) then
      write(temp, '(i2)') pp_id
      call error_handler(1,"Pilot Point ID greater than number of pilot points locations: " // trim(temp))
    end if

    do index=1, this%npp
      if (this%pp_id(index)==pp_id) exit
    end do

  end function get_pp_index

!-------------------------------------------------------------------------------------------------!

  subroutine read_pp_line(this, id, pp_type, strings, length, class_id)
    use m_vstringlist, only: t_vstringlist, vstrlist_search, vstrlist_index
    use m_file_io, only: item2int, item2dp
    use m_error_handler, only: error_handler

    implicit none
    class(t_pilotpoints),intent(inout) :: this
    character(*),intent(in)            :: id, pp_type
    type(t_vstringlist),intent(in)     :: strings
    integer,intent(in)                 :: length, class_id
    integer                            :: pp_index

    ! Read line for pp_id and get internal pilot point index
    pp_index = this%get_pp_index(item2int(strings, 1))
    ! Now we react to type
    select case(pp_type)
      case("AQUIFER")
        this%values(pp_index,1,class_id) = item2dp(strings, 3)  ! Kmin
        this%values(pp_index,2,class_id) = item2dp(strings, 4)  ! Kmax
        this%values(pp_index,3,class_id) = item2dp(strings, 5)  ! Ss
        this%values(pp_index,4,class_id) = item2dp(strings, 6)  ! Sy
        this%values(pp_index,5,class_id) = item2dp(strings, 7)  ! Aniso
        this%values(pp_index,6,class_id) = item2dp(strings, 8)  ! Kd
        ! K Processing & Checking
        if (this%values(pp_index,2,class_id) < this%values(pp_index,1,class_id)) then
          call error_handler(1,"KMax < Kmin for pilot point " // trim(id))
        else
          ! We krige the *difference* to ensure Kmax > Kmin everywhere
          ! Store this value as a difference
          this%values(pp_index,2,class_id) = this%values(pp_index,2,class_id) - this%values(pp_index,1,class_id)
        end if
      case("AQUITARD")
        this%values(pp_index,1,class_id) = item2dp(strings, 3)  ! Kmin
        this%values(pp_index,2,class_id) = item2dp(strings, 4)  ! Kmax
        this%values(pp_index,5,class_id) = item2dp(strings, 5)  ! Aniso
        this%values(pp_index,6,class_id) = item2dp(strings, 6)  ! Kd
        ! K Processing & Checking
        if (this%values(pp_index,2,class_id) < this%values(pp_index,1,class_id)) then
          call error_handler(1,"KMax < Kmin for pilot point " // trim(id))
        else
          ! We krige the *difference* to ensure Kmax > Kmin everywhere
          ! Store this value as a difference
          this%values(pp_index,2,class_id) = this%values(pp_index,2,class_id) - this%values(pp_index,1,class_id)
        end if
      case("GLOBAL")
        ! Set for ALL classes
        this%values(pp_index,7, :) = item2dp(strings, 2)  ! KHp
        this%values(pp_index,8, :) = item2dp(strings, 3)  ! KVp
        this%values(pp_index,9, :) = item2dp(strings, 4)  ! STp
      case("GLOBAL_AQUITARD")
        ! Set for ALL classes
        this%values(pp_index,7, :) = item2dp(strings, 2)  ! KHp
        this%values(pp_index,8, :) = item2dp(strings, 3)  ! KVp
      case DEFAULT
        ! Outer routine prevents us from getting here
        call error_handler(1,"Unknown Pilot Point Type: " // trim(pp_type))
    end select

  end subroutine read_pp_line

!-------------------------------------------------------------------------------------------------!

  subroutine write_pp_table(this, is_aquitard)
    use m_global, only: log
    use m_file_io, only: t_file_writer
    implicit none
    class(t_pilotpoints), intent(in)     :: this
    logical, intent(in)                  :: is_aquitard

    integer, allocatable :: props(:)
    character(len=5), allocatable :: prop_names(:)
    character(len=256) :: header, line
    integer :: i, j, k, nprops
    real :: val

    ! Set up properties and names
    if (is_aquitard) then
      props = aquitard_props
      allocate(prop_names(naquitard_props))
      prop_names = [" Kmin", " Kmax", "Aniso", "   Kd", "  KHp", "  KVp"]
    else
      props = [(k, k=1,nproperties)]
      allocate(prop_names(nproperties))
      prop_names = [" Kmin", " Kmax", "   Ss", "   Sy", "Aniso", "   Kd", "  KHp", "  KVp", "  STp"]
    end if

    nprops = size(props)

    ! Header line
    header = "   ID  Class             X            Y  Zone "
    do k = 1, nprops
      write(header(len_trim(header)+1:), '(A13)') trim(prop_names(k))
    end do
    call log%write_line(trim(header))

    ! Loop over classes, then pilot points
    do j = 1, size(this%values, 3)  ! class_id
      do i = 1, this%npp
        write(line, '(I5,2X,I5,2X,F12.2,1X,F12.2,1X,I5)') this%pp_id(i), j, this%grid%coords(1,i), this%grid%coords(2,i), this%category(i)
        do k = 1, nprops
          write(line(len_trim(line)+2:), '(ES12.4)') this%values(i, props(k), j)
        end do
        call log%write_line(trim(line))
      end do
    end do

  end subroutine write_pp_table

!-------------------------------------------------------------------------------------------------!

  subroutine finalize_pilotpoints(this)
    implicit none
    class(t_pilotpoints),intent(inout) :: this

    if (allocated(this%category)) deallocate(this%category)
    if (allocated(this%values  )) deallocate(this%values  )
    if (allocated(this%zonelist)) deallocate(this%zonelist)
    if (associated(this%pp_id  )) nullify(this%pp_id)
    if (associated(this%grid   )) nullify(this%grid)

  end subroutine finalize_pilotpoints

!-------------------------------------------------------------------------------------------------!
end module m_pilotpoints