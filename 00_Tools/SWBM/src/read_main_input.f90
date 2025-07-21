module m_read_main_input
  use m_file_io
  use m_error_handler, only: error_handler
  use m_options, only: t_options
!-------------------------------------------------------------------------------------------------!
! Module variables
  type(t_file_reader), pointer :: reader
  logical                      :: has_necessary_blocks(4) ! Discretization, Opt, Parameters, Inputs

  ! Interfaces
  interface read_array_file
    module procedure read_int_array_file
    module procedure read_real_array_file
  end interface

!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!
  subroutine read_main_input(file, opt)
    use m_global, only: specwell_locs_file, specwell_vol_file
    use define_fields, only: nSpecWells
    implicit none
    character(100), intent(in)  :: file
    type(t_options), intent(in) :: opt
    integer                     :: eof_check
    character(100)              :: id

    reader => open_file_reader(file)

    eof_check = 0
    has_necessary_blocks = .false.
    do
      call reader%next_block_id(eof_check, id)
      if (eof_check == -1) exit
      call block_redirect(opt, id)
    end do
    call close_file(reader)

    if (has_necessary_blocks(1) .eqv. .false.) call error_handler(1,reader%file,"Missing Discretization Block!")
    if (has_necessary_blocks(2) .eqv. .false.) call error_handler(1,reader%file,"Missing Options Block!")
    !if (has_necessary_blocks(3) .eqv. .false.) call error_handler(1,reader%file,"Missing Parameters Block!")  ! Currently no parameters are specified here!
    if (has_necessary_blocks(4) .eqv. .false.) call error_handler(1,reader%file,"Missing Input_Files Block!")
    
    if ((trim(specwell_locs_file) == "".and.nSpecWells>0).or.(trim(specwell_vol_file) == "".and.nSpecWells>0)) then
      call error_handler(1,reader%file,"Must specify SPECWELL_LOCS and SPECWELL_RATES input files if NSPECWELLS > 0")
    end if

  end subroutine read_main_input
!-------------------------------------------------------------------------------------------------!
  subroutine block_redirect(opt, id)
    implicit none
    type(t_options), intent(in) :: opt
    character(*), intent(in)        :: id

    ! Regardless of block
    call pre_block_read_check(id)

    select case(trim(id))
      case("OPTIONS")
        ! This is the only block we let read it's own entries and set it's own read tracker (has_opt)
        call opt%read_options(reader)
        has_necessary_blocks(2) = .true.
      case("DISCRETIZATION")
        call read_DISCRETIZATION_block(reader)
        has_necessary_blocks(1) = .true.
      case("PARAMETERS")
        call read_PARAMETERS_block(reader)
        has_necessary_blocks(3) = .true.
      case("INPUT_FILES")
        call read_INPUT_FILES_block(reader)
        has_necessary_blocks(4) = .true.
      case("PRINT_DAILY")
        call read_PRINT_DAILY_block(reader)
      case ("END")
        ! Do nothing - errant line? Block reader ended early?
      case DEFAULT
        call error_handler(1,reader%file,"Unknown Block Name: " // trim(id))
    end select

  end subroutine block_redirect
!-------------------------------------------------------------------------------------------------!

  subroutine pre_block_read_check(block_id)
    ! Generic hook for making sure a block has its prerequisite data read in.
    implicit none
    character(*), intent(in)        :: block_id

    select case(trim(block_id))
      case("THING")
        if (has_necessary_blocks(2).eqv. .false.) call error_handler(2,reader%file,"ERROR")
      case DEFAULT
      ! Do nothing
    end select

  end subroutine pre_block_read_check

!-------------------------------------------------------------------------------------------------!

  subroutine read_DISCRETIZATION_block(reader)
    use m_global, only: nmonths, WYstart, nrows, ncols, model_name, nSFR_inflow_segs
    use define_fields, only: npoly, nAgWells, nSpecWells, nlandcover
    use irrigation, only: nsubws
    implicit none
    type(t_file_reader), pointer    :: reader
    logical                         :: has_necessary_items(11) = .false.

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings

    ! Loop until end of block or end of file
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("NMONTHS")
          nmonths = item2int(strings, 2)
          has_necessary_items(1) = .true.
        case("WYSTART")
          WYstart = item2int(strings, 2)
          has_necessary_items(2) = .true.
        case("NPOLY","NFIELDS")
          npoly = item2int(strings, 2)
          has_necessary_items(3) = .true.
        case("NSUBWS")
          nsubws = item2int(strings, 2)
          has_necessary_items(4) = .true.
        case("NLANDCOVER")
          nlandcover = item2int(strings, 2)
          has_necessary_items(5) = .true.
        case("NAGWELLS")
          nAgWells = item2int(strings, 2)
          has_necessary_items(6) = .true.
        case("NSPECWELLS")
          nSpecWells = item2int(strings, 2)
          has_necessary_items(7) = .true.
        case("NSFR_INFLOW_SEGS")
          nSFR_inflow_segs = item2int(strings, 2)
          has_necessary_items(8) = .true.
        case("NAME","MFNAME")
          call item2char(strings, 2, model_name)
          has_necessary_items(9) = .true.
        case("NROWS")
          nrows = item2int(strings, 2)
          has_necessary_items(10) = .true.
        case("NCOLS")
          ncols = item2int(strings, 2)
          has_necessary_items(11) = .true.
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Discretization option: " // trim(id))
      end select
    end do
    ! Really simple input enforcement
    if (any(has_necessary_items .eqv. .false.)) then
      call error_handler(1,reader%file,"Missing items in Discretization Block")
    end if
  end subroutine read_DISCRETIZATION_block

!-------------------------------------------------------------------------------------------------!

  subroutine read_PARAMETERS_block(reader)
    !use define_fields, only: RD_Mult
    implicit none
    type(t_file_reader), pointer    :: reader
    logical                         :: has_necessary_items(1) = .false.

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings

    ! Loop until end of block or end of file
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("NONE_IMPLEMENTED")
          !RD_Mult = item2real(strings, 2)
          has_necessary_items(1) = .true.
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Parameter: " // trim(id))
      end select
    end do
    ! Really simple input enforcement
    !if (any(has_necessary_items==.false.)) then
    !  call error_handler(1,reader%file,"Missing required items in Parameter Block")
    !end if

    end subroutine read_PARAMETERS_block

!-------------------------------------------------------------------------------------------------!
  subroutine read_INPUT_FILES_block(reader)
    use m_global
    implicit none
    type(t_file_reader), pointer    :: reader
    logical                         :: has_necessary_items(12) = .false.

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings

    ! Loop until end of block or end of file
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("PRECIP")
          call item2char(strings, 2, precip_file)
          has_necessary_items(1) = .true.
        case("ET")
          call item2char(strings, 2, et_file)
          has_necessary_items(2) = .true.
        case("ET_EXT_DEPTH")
          call item2char(strings, 2, et_ext_depth_file)
          has_necessary_items(3) = .true.
        case("RECHARGE_ZONES")
          call item2char(strings, 2, recharge_zones_file)
          has_necessary_items(4) = .true.
        case("KC_FRAC")
          call item2char(strings, 2, kc_frac_file)
          has_necessary_items(5) = .true.
        case("SFR_PARTITION")
          call item2char(strings, 2, sfr_partition_file)
          has_necessary_items(6) = .true.
        case("POLY_LANDCOVER")
          call item2char(strings, 2, poly_landcover_file)
          has_necessary_items(7) = .true.
        case("POLY_AGWELL")
          call item2char(strings, 2, poly_agwell_file)
          has_necessary_items(12) = .true.
        case("SFR_NETWORK")
          call item2char(strings, 2, sfr_network_file)
          has_necessary_items(8) = .true.
        case("ETS_TEMPLATE")
          call item2char(strings, 2, ets_template_file)
          has_necessary_items(9) = .true.
        case("WEL_TEMPLATE","WELL_TEMPLATE")
          call item2char(strings, 2, wel_template_file)
          has_necessary_items(10) = .true.
        case("AGWELL_LOCS")
          call item2char(strings, 2, agwell_locs_file)
          has_necessary_items(11) = .true.
        case("SPECWELL_LOCS")
          call item2char(strings, 2, specwell_locs_file)
        case("SPECWELL_VOL")
          call item2char(strings, 2, specwell_vol_file)
        case("SFR_NETWORK_JTF")
          call item2char(strings, 2, sfr_jtf_file)
          has_necessary_items(8) = .true.
        case("IRR_DITCH")
          call item2char(strings, 2, ditch_file)
        case("ET_ZONE_CELLS")
          call item2char(strings, 2, et_zones_file)
        case("MAR_DEPTH")
          call item2char(strings, 2, MAR_depth_file)
        case("CURTAIL_FRAC")
          call item2char(strings, 2, curtail_frac_file)
        case("ET_CORRECTION")
          call item2char(strings, 2, et_cor_file)
        case("WATER_MOVER")
          call item2char(strings, 2, water_mover_file)
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Parameter: " // trim(id))
      end select
    end do
    ! Really simple input enforcement
    if (any(has_necessary_items .eqv. .false.)) then
      call error_handler(1,reader%file,"Missing required items in Input_Files Block")
    end if

  end subroutine read_INPUT_FILES_block
!-------------------------------------------------------------------------------------------------!

  subroutine read_PRINT_DAILY_block(reader)
    use m_global, only: n_daily_out, daily_out_idx, daily_out_nms
    implicit none
    type(t_file_reader), pointer :: reader
    integer                      :: i

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings

    ! How many items?
    n_daily_out = reader%get_block_len()
    allocate(daily_out_idx(n_daily_out), daily_out_nms(n_daily_out))

    ! Read items
    do i=1, n_daily_out
      call reader%next_block_item(status, id, strings, length)
      daily_out_idx(i) = item2int(strings, 1)
      call item2char(strings, 2, daily_out_nms(i))
    end do

    ! Skip to get past "END"
    call reader%skip(1)

  end subroutine read_PRINT_DAILY_block

!-------------------------------------------------------------------------------------------------!
  subroutine read_int_array_file(filename, array, rows, cols, has_header)
    character(*), intent(in)     :: filename
    integer, intent(in)          :: rows, cols
    integer, intent(inout)       :: array(rows, cols)   ! Typical order
    logical, intent(in),optional :: has_header

    reader => open_file_reader(filename)

    ! Skip header
    if (present(has_header)) then
      if (has_header) call reader%skip(1)
    end if

    ! Read array
    read(reader%unit,*) array

    call close_file(reader)

  end subroutine read_int_array_file
!-------------------------------------------------------------------------------------------------!

  subroutine read_real_array_file(filename, array, rows, cols, has_header)
    character(*), intent(in)     :: filename
    integer, intent(in)          :: rows, cols
    real, intent(inout)          :: array(rows, cols)   ! Typical order
    logical, intent(in),optional :: has_header

    reader => open_file_reader(filename)

    ! Skip header
    if (present(has_header)) then
      if (has_header) call reader%skip(1)
    end if

    ! Read array
    read(reader%unit,*) array

    call close_file(reader)

  end subroutine read_real_array_file

!-------------------------------------------------------------------------------------------------!
end module m_read_main_input