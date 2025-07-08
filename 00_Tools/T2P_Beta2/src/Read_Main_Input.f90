module m_read_main_input
  use m_T2P, only: t_t2p
  use m_vstring, only: t_vstring, vstring_new, vstring_toupper
  use m_vstringlist, only: t_vstringlist, vstrlist_search, vstrlist_split, vstrlist_append, VSTRINGLIST_INDEX_UNKNOWN
  use m_file_io
  use m_error_handler, only: error_handler
  implicit none

  type(t_file_reader), pointer :: reader
  contains

!-------------------------------------------------------------------------------------------------!

  subroutine read_main_input(t2p, file)
    implicit none

    type(t_t2p),intent(inout)   :: t2p
    character(100), intent(in)  :: file
    integer                     :: eof_check
    character(100)              :: id

    call t2p%write_status("Reading Input Files")
    reader => open_file_reader(file)

    eof_check = 0
    do
      call reader%next_block_id(eof_check, id)
      if (eof_check == -1) exit
      call block_redirect(t2p, id)
    end do
    !write(*,*) 'DONE READING!'

    call close_file(reader)

  end subroutine read_main_input

!-------------------------------------------------------------------------------------------------!

  subroutine block_redirect(t2p, id)
    implicit none

    type(t_t2p),intent(inout)       :: t2p
    character(*), intent(in)        :: id

    ! Regardless of block
    call pre_block_read_check(t2p, id)

    select case(trim(id))
      case("OPTIONS")
        ! This is the only block we let read it's own entries and set it's own read tracker (has_opt)
        call t2p%opt%read_options(reader)
        call t2p%process_options()
        t2p%has_opt = .true.
      case("FLOW_MODEL")
        call read_FLOW_MODEL_block(t2p, reader)
      case("CLASSES")
        CALL read_class_block(t2p, reader)
      case("COVARIATES")
        CALL read_covariates_block(t2p, reader)
      case("DATASET")
        CALL read_dataset_block(t2p, reader)
      case("VARIOGRAMS")
        CALL read_variogram_block(t2p, reader)
      case("PP_LOCS")
        call pre_block_read_check(t2p, id)
        CALL read_pploc_block(t2p, reader, t2p%pilot_points)
      case("PP_PARAMETERS")
        call pre_block_read_check(t2p, id)
        CALL read_ppparm_block(t2p, reader)
      case("PP_AQUITARD_LOCS")
        call pre_block_read_check(t2p, id)
        CALL read_pploc_block(t2p, reader, t2p%pilot_points_aquitard)
      case ("END")
        ! Do nothing - errant line? Block reader ended early?
      case DEFAULT
        call error_handler(1,reader%file,"Unknown Block Name: " // trim(id))
    end select

  end subroutine block_redirect

!-------------------------------------------------------------------------------------------------!

  subroutine pre_block_read_check(t2p, block_id)
    ! Generic hook for making sure a block has its prerequisite data read in.
    implicit none
    type(t_t2p),intent(inout)       :: t2p
    character(*), intent(in)        :: block_id

    select case(trim(block_id))
      case("CLASSES")
        if (.not. t2p%has_opt) call error_handler(2,reader%file,"OPTIONS block must be declared before CLASSES")
      case("COVARIATES")
        if (.not. t2p%has_classes) call error_handler(2,reader%file,"CLASSES block must be declared before COVARIATES")
        if (t2p%has_vario) call error_handler(2,reader%file,"COVARIATES block cannot be declared after VARIOGRAMS")
      case("VARIOGRAMS")
        if (.not. t2p%has_classes) call error_handler(2,reader%file,"CLASSES block must be declared before VARIOGRAMS")
      case("PP_LOCS")
        if (.not. t2p%has_classes) call error_handler(2,reader%file,"CLASSES block must be declared before PP_LOCS")
      case("PP_PARAMETERS")
        if (.not. t2p%has_pplocs) call error_handler(2,reader%file,"PP_LOCS block must be declared before PP_PARAMETERS")
      case("PP_AQUITARD_LOCS")
        if (.not. t2p%has_classes) call error_handler(2,reader%file,"CLASSES block must be declared before PP_AQUITARD_LOCS")
      case DEFAULT
      ! Do nothing
    end select

  end subroutine pre_block_read_check

!-------------------------------------------------------------------------------------------------!

  subroutine read_FLOW_MODEL_block(t2p, reader)
    use m_flow_model, only: t_flow_model, create_flow_model_object, check_id_validity
    implicit none
    type(t_t2p),intent(inout)       :: t2p
    type(t_file_reader), pointer    :: reader

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings

    ! Loop until end of block or end of file
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("TYPE")
          call item2char(strings, 2, value, toupper=.true.)
          t2p%fmodel => create_flow_model_object(value, t2p%opt%PROJECT_NAME)
        case("SIM_FILE")
          call check_id_validity(id, t2p%fmodel, 2) ! Only IWFM
          call item2char(strings, 2, t2p%fmodel%sim_file)
        case("PREPROC_FILE")
          call check_id_validity(id, t2p%fmodel, 2) ! Only IWFM
          call item2char(strings, 2, t2p%fmodel%preproc_file)
        case("NAME_FILE", "NAM_FILE")
          call check_id_validity(id, t2p%fmodel, 3) ! only MODFLOW2000
          call item2char(strings, 2, t2p%fmodel%sim_file)
        case("GSF_FILE")
          call check_id_validity(id, t2p%fmodel)    ! Grid or MFUSG
          call item2char(strings, 2, t2p%fmodel%grd_file)
          t2p%fmodel%grd_file_type = 'gsf'
        case("GRID_FILE")
          call check_id_validity(id, t2p%fmodel)    ! Only Grid
          call item2char(strings, 2, t2p%fmodel%grd_file)
        case("TEMPLATE_FILE")
          call check_id_validity(id, t2p%fmodel)
          call item2char(strings, 2, t2p%fmodel%template_file)
        case("PP_ZONE_FILE")
          call check_id_validity(id, t2p%fmodel)
          call item2char(strings, 2, t2p%fmodel%ppzone_file)
        case("HSU_FILE")
          call check_id_validity(id, t2p%fmodel)
          call item2char(strings, 2, t2p%fmodel%hsu_file)
        case("XOFF","XOFFSET")
          call check_id_validity(id, t2p%fmodel, 3) ! only MODFLOW2000
          t2p%fmodel%xoff = item2dp(strings, 2)
        case("YOFF","YOFFSET")
          call check_id_validity(id, t2p%fmodel, 3) ! only MODFLOW2000
          t2p%fmodel%yoff = item2dp(strings, 2)
        case("ROT","ROTATION")
          call check_id_validity(id, t2p%fmodel, 3) ! only MODFLOW2000
          t2p%fmodel%rot= item2dp(strings, 2)
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Flow Model option: " // trim(id))
      end select
    end do

    ! Hook for T2P to process flowmodel entries
    call t2p%setup_flow_model()

  end subroutine read_FLOW_MODEL_block

!-------------------------------------------------------------------------------------------------!

  subroutine read_class_block(t2p, reader)
    implicit none
    type(t_t2p),intent(inout)       :: t2p
    type(t_file_reader), pointer    :: reader

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings
    ! sub variables
    integer                       :: i, j, k, n, nclasses, prime_id, class_id
    type(t_vstringlist),pointer   :: class_names

    allocate(class_names)
    call vstrlist_new(class_names)

    ! Pre-read for number of classes
    !call reader%get_block_dim(n, nclasses)  ! returns primary, total classes, or generally (lines, items)
    n = reader%get_block_len()
    if (n < 1) then
      call error_handler(2,reader%file,"No Data Classes Listed")
    end if

    ! Loop over data class lines
    do i=1, n
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      call vstrlist_append(class_names, vstring_toupper(vstrlist_index(strings,1)))
    end do
    read(reader%unit,*)  ! Read past end block

    ! Pass to T2P
    call t2p%setup_data_classes(n,class_names)

  end subroutine read_class_block

!-------------------------------------------------------------------------------------------------!

  subroutine read_covariates_block(t2p, reader)
    implicit none
    type(t_t2p),intent(inout)       :: t2p
    type(t_file_reader), pointer    :: reader
    integer                         :: i,k,nlines,nitems,cid,nsecondary,prime_id
    integer,pointer                 :: connections(:,:)
    character(30)                   :: temp

    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings

    nsecondary = 0

    ! Pre-read for number of classes
    call reader%get_block_dim(nlines, nitems)  ! returns nlines, total items

    allocate(connections(nitems,t2p%nprimary))
    t2p%nsecondary_by_id(:) = 0

    ! Setup matrix, values indicate variogram ID. Can start off assuming each class has a variogram
    connections = 0
    do i = 1, t2p%nprimary
      connections(1, i) = i
    end do

    if (nlines < 1) then
      !call error_handler(2,reader%file,"No Covariates Listed")
      ! Instead, early return
      return
    end if

    ! Ensure covariates are being used with a valid intepolation option
    if (.not. t2p%opt%INTERP_SECOND_OK) call error_handler(4,opt_msg="Covariates are not supported for non-kriging options.")

    ! Loop over data class lines
    do i=1, nlines
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      ! Locate primary data ID
      if (t2p%opt%INTERP_ORDER=="BEFORE") then
        prime_id = t2p%get_class_id(vstrlist_index(strings, 1))
        if (prime_id==0) call error_handler(1,reader%file,"Invalid Class Name in Covariates Block = "//id)
      else ! t2p%opt%INTERP_ORDER=="AFTER")
        prime_id = t2p%get_parameter_id(vstrlist_index(strings, 1))
        if (prime_id==0) call error_handler(1,reader%file,"Invalid Hydraulic Parameter Name in Covariates Block = "//id)
      end if

      ! Loop over specifying the cross relationship(s)
      do k=2, length
        ! New covariate?
        cid = t2p%get_class_id(vstrlist_index(strings, k))
        if (cid==0) then
          call vstrlist_append(t2p%class_names, vstring_toupper(vstrlist_index(strings,k)))
          cid = t2p%get_class_id(vstrlist_index(strings, k))
          ! Important check
          if (t2p%opt%INFER_LAST_CLASS.and.(cid==t2p%nclasses)) then
            call error_handler(1,reader%file,"Cannot specify covariate for final class when INFER_LAST_CLASS is TRUE")
          end if
          nsecondary = nsecondary + 1
        end if
        connections(k,prime_id) = cid
      end do
      t2p%nsecondary_by_id(prime_id) = length-1
    end do
    read(reader%unit,*)  ! Read past end block

    ! Pass to T2P
    call t2p%setup_covariates(nsecondary,connections)

  end subroutine read_covariates_block

!-------------------------------------------------------------------------------------------------!

subroutine read_dataset_block(t2p, reader)
  implicit none
  type(t_t2p),intent(inout)       :: t2p
  type(t_file_reader), pointer    :: reader

  ! Standard file reader variables
  integer                    :: status, length
  character(30)              :: id, value, tmp
  type(t_vstringlist)        :: strings

  ! Sub variables
  logical                    :: fname_set
  type(t_vstring)            :: fname, ftrans

  ! Default values
  call vstring_new (ftrans , "NONE" )

  ! Loop until end of block or end of file
  do
    call reader%next_block_item(status, id, strings, length)
    if (status /= 0) exit  ! exit if end of block or end of file
    select case(trim(id))
      case("FILE")
        fname = vstrlist_index(strings , 2)
        fname_set = .true.
      case("TRANSFORM")
        ftrans = vstring_toupper(vstrlist_index(strings , 2))
      case DEFAULT
        call error_handler(1,reader%file,"Unknown Dataset option: " // trim(id))
    end select
  end do

  ! Did at least filename get set??
  if (.not. fname_set ) then
    call error_handler(2,reader%file,"Dataset Block must include a FILE")
  end if

  ! Add to T2P
  t2p%ndatafiles = t2p%ndatafiles + 1
  call vstrlist_append(t2p%dataset_files, fname)
  call vstrlist_append(t2p%dataset_trans, ftrans)

  ! No Hook (yet) for datasets
  t2p%has_datafiles = .true.

end subroutine read_dataset_block

!-------------------------------------------------------------------------------------------------!

subroutine read_variogram_block(t2p, reader)
  use m_vario, only: t_vgm, get_vgm_from_line
  use m_kriging, only: t_krige
  use tools, only: expand_int_array
  implicit none
  type(t_t2p),intent(inout)       :: t2p
  type(t_file_reader), pointer    :: reader

  ! Standard file reader variables
  integer                    :: status, length
  character(30)              :: id, value
  type(t_vstringlist)        :: strings

  ! Sub variables
  type(t_vgm),pointer        :: vgms(:,:), tmp(:,:)
  integer,pointer            :: nnear(:), nstruct(:)
  integer                    :: prime_id, prime_vidx, cross_id, vario_id, struct_id, vcount, maxread, hsu_id
  logical                    :: iscross, ispp, ishsu
  type(t_vstring)            :: class_name, cross_name, hsu_name
  type(t_vstringlist)        :: class_temp
  character(100)             :: msg

  ! Allocate variograms
  ! nvario is currently set during the class block read, max structures in settings
  allocate(vgms(t2p%opt%max_vstruct,t2p%nvario), nnear(t2p%nclasses+1), nstruct(t2p%nvario))
  vcount = 0
  nnear = 0
  nstruct = 0

  ! Loop until end of block or end of file
  status = 0
  do
    if (status /= 0) exit  ! Exit if inner loop variogram read loop found the end
    call reader%next_block_item(status, id, strings, length)
    if (status /= 0) exit  ! exit if end of block or end of file
    select case(trim(id))
      case("CLASS")
        ! Now we need to figure out what variogram we have, and read all the structures for it
        iscross = .false.
        ishsu   = .false.
        ispp    = .false.
        class_name = vstrlist_index(strings , 2)

        ! Handle "CLASS a:b%HSU" or "CLASS a : b % HSU"
        ! Step 1: Detect cross variogram (:)
        if (length > 3 .and. vstrlist_search(strings, ":") /= VSTRINGLIST_INDEX_UNKNOWN) then
          ! CLASS a : b syntax
          class_name = vstrlist_index(strings, 2)
          cross_name = vstrlist_index(strings, 4)
          iscross = .true.
        else if (vstring_match(vstrlist_index(strings,2), "*:*")) then
          ! CLASS a:b%HSU or a:b
          class_temp = vstrlist_split(vstrlist_index(strings,2), ":")
          class_name = vstrlist_index(class_temp, 1)
          cross_name = vstrlist_index(class_temp, 2)
          iscross = .true.
        else
          ! Single class
          class_name = vstrlist_index(strings,2)
          cross_name = class_name
        end if

        ! Step 2: Detect HSU (%), from either side
        if (length > 3 .and. vstrlist_search(strings, "%") /= VSTRINGLIST_INDEX_UNKNOWN) then
          ! CLASS a : b % HSU
          hsu_name = vstrlist_index(strings,6)
          ishsu = .true.
        else if (vstring_match(class_name, "*%*")) then
          ! CLASS a%HSU
          class_temp = vstrlist_split(class_name, "%")
          class_name = vstrlist_index(class_temp, 1)
          hsu_name   = vstrlist_index(class_temp, 2)
          ishsu = .true.
        else if (vstring_match(cross_name, "*%*")) then
          ! CLASS a:b%HSU
          class_temp = vstrlist_split(cross_name, "%")
          cross_name = vstrlist_index(class_temp, 1)
          hsu_name   = vstrlist_index(class_temp, 2)
          ishsu = .true.
        end if

        ! Get variogram ids
        ! Check for pilot point class
        if (vstring_match(class_name,"PilotPoints",nocase=.true.)) then
          ispp = .true.
          vario_id = t2p%nvario  ! PP vario is max
          !prime_id = size(nnear,1)
          prime_vidx = size(nnear,1)
        else
        ! Not PP
          select type(p => t2p%interp_data)
            class is(t_krige)
              if (t2p%opt%INTERP_ORDER=="BEFORE") then
                prime_id = t2p%get_class_id(class_name, adjust_for_infer=.true.)
                prime_vidx = t2p%get_class_id(class_name)
                ! If you're wondering -
                ! connections is indexed by prime_id, which aligns with the active class number
                ! global_vario_idx,nnear,vgms,and nstruct is indexed by prime_vidx, which aligns with the class number
                if (prime_id==0) then
                  call error_handler(1,reader%file,"Invalid Class Name in Variogram Assignment")
                end if
              else ! this%opt%INTERP_ORDER=="AFTER")
                prime_id = t2p%get_parameter_id(class_name)
                if (prime_id==0) then
                  call error_handler(1,reader%file,"Invalid Hydraulic Parameter Name in Variogram Assignment")
                end if
              end if
              if (iscross) then
                cross_id = t2p%get_class_id(cross_name)
                if (cross_id==0) then
                  call error_handler(1,reader%file,"Invalid Class Name in Cross Variogram Assignment")
                end if
              else
                cross_id = prime_vidx
              end if
              if (ishsu) then
                hsu_id = p%add_zonal_variogram(hsu_name, prime_vidx, cross_id, t2p%opt, t2p%nvario)
                ! Resize nstruct & vgm from nvario update...
                call expand_int_array(nstruct,t2p%nvario, init=0)
                allocate(tmp(t2p%opt%max_vstruct,t2p%nvario))
                tmp(:, 1:size(vgms)) = vgms
                deallocate(vgms); nullify(vgms)
                vgms => tmp
              else
                hsu_id = 0
              end if
              vario_id = p%global_vario_idx(hsu_id, prime_vidx, cross_id)
              if (vario_id==0) then
                call error_handler(1,reader%file,"Invalid (Cross) Variogram Assignment - Check Covariates Block.")
              end if
            class default
              call error_handler(4,opt_msg="Variograms cannot be used without kriging as the interpolator")
          end select
        end if

        ! Now we know what variogram, loop and read in structures
        do
          call reader%next_block_item(status, id, strings, length)
          if (status /= 0) exit       ! exit if end of block or end of file
          if (trim(id)=="CLASS") then ! move back one line & exit if it's the next class
            call reader%back(1)
            exit
          end if
          ! Read structure
          struct_id = item2int(strings, 1)
          nstruct(vario_id) = struct_id

          !TODO validate struct_id
          ! Is the variogram line long enough?
          write(msg, '(2(a,i3))') 'variogram', vcount+1, ', structure', struct_id
          if (t2p%opt%INTERP_DIM==2 .and. length < 7) then
            call error_handler(2,reader%file,"Too few variogram parameters for 2D (7+ expected). See "//msg)
          else if (t2p%opt%INTERP_DIM==3 .and. length < 10) then
            call error_handler(2,reader%file,"Too few variogram parameters for 3D (10+ expected). See "//msg)
          end if
          ! Get full variogram item from the rest of the line
          call get_vgm_from_line(vgms(struct_id,vario_id), strings, struct_id, t2p%OPT%INTERP_DIM, maxread)
          ! NNear on first struct read, if not cross, advance vcount
          if (struct_id == 1) then
            if (iscross.or.ishsu) then
              ! Can't get n cross points...
            else
              nnear(prime_vidx) = item2int(strings, maxread+1)
            end if
            vcount = vcount + 1
          end if
          if (t2p%opt%ANISOTROPIC_SEARCH .and. struct_id>1) then
            ! check anisotropy
            if (any(vgms(struct_id,vario_id)%rotmat /= vgms(1,vario_id)%rotmat)) then
              call t2p%write_status("Different anisotropy are used while anisotropic search is on."//new_line('1')//&
                                    "  Only the first structure anisotropy will be used.")
            end if
          end if
        end do
        ! End of variogram structure reading, moves to next class variogram or end of block
      case DEFAULT
        call error_handler(1,reader%file,"Unknown Variogram option: " // trim(id))
    end select
  end do ! end of variogram reading loop

  ! Check we have all variograms, send to t2p object
  if (vcount < t2p%nvario) then
    write(msg, '(2(a,i3))') "Too Few Variograms, expected ", t2p%nvario, ", found: ", vcount
    call error_handler(2,reader%file,msg)
  end if
  call t2p%setup_variograms(vgms, nnear, nstruct)  ! t2p%interp_pp, t2p%interp_data)

end subroutine read_variogram_block

!-------------------------------------------------------------------------------------------------!

subroutine read_pploc_block(t2p, reader, p)
  use m_pilotpoints, only: t_pilotpoints, create_pilotpoint_object
  implicit none
  type(t_t2p),intent(inout)       :: t2p
  type(t_file_reader), pointer    :: reader
  type(t_pilotpoints),pointer     :: p

  ! Standard file reader variables
  integer                    :: status, length
  character(30)              :: id, value
  type(t_vstringlist)        :: strings

  ! Sub variables
  integer,pointer            :: pp_id(:)
  integer,allocatable        :: zone_tracker(:)
  integer                    :: i, j, npp, pid, nzones
  logical                    :: zone_counted

  ! Assume block rows = # of pilot points
  npp = reader%get_block_len()

  ! Allocate & setup PP object
  allocate(pp_id       (npp), &
           zone_tracker(npp)  )
  p => create_pilotpoint_object(npp, t2p%ntexclasses)
  nzones = 0
  zone_tracker = 0

  ! Now read the pilot point locations
  do i=1, npp
    call reader%next_block_item(status, id, strings, length)
    if (status /= 0) exit  ! exit if end of block or end of file
    ! Should this be read with it's own function?
    pp_id(i)            = item2int(strings, 1)
    p%grid%coords(1,i)  = item2dp (strings, 2)
    p%grid%coords(2,i)  = item2dp (strings, 3)
    p%category(i)       = item2int(strings, 4)
    !t2p%pilot_points%lay_type(i)       = find_string_index_in_list(strings, 5, "A,T", .true.)

    ! Early input checking & handling
    do j=1, i-1
      if (pp_id(j) == pp_id(i)) call error_handler(1,reader%file,"All pilot point IDs must be unique")
    end do

    ! Zone tracking
    zone_counted = .false.
    do j=1, nzones
      if (zone_tracker(j) == p%category(i)) zone_counted = .true.
    end do
    if (.not. zone_counted ) then
      nzones = nzones + 1
      zone_tracker(nzones) = p%category(i)
    end if
  end do

  ! Should be at block end, check to be sure!
  call reader%next_block_item(status, id, strings, length)
  if (trim(id)/="END") then
    ! Assumption is the input file is formatted incorrectly... hopefully that's why
    call error_handler(1,reader%file,"PP_LOCS section improperly formatted.")
  end if

  ! Run pplocs setup hook
  call t2p%setup_pplocs(pp_id, nzones, zone_tracker, p)

end subroutine read_pploc_block

!-------------------------------------------------------------------------------------------------!

subroutine read_ppparm_block(t2p, reader)
  use m_global, only: NODATA
  use m_pilotpoints, only: global_props, nglobal_props
  implicit none
  type(t_t2p),intent(inout)       :: t2p
  type(t_file_reader), pointer    :: reader
  integer                         :: class_id

  ! Standard file reader variables
  integer                    :: status, length, i
  character(30)              :: id, value
  type(t_vstringlist)        :: strings

  ! Subroutine variables
  integer                    :: pp_index

  ! Loop until end of block or end of file
  status = 0
  do
    if (status /= 0) exit  ! Exit if inner loop pp read loop found the end
    call reader%next_block_item(status, id, strings, length)
    if (status /= 0) exit  ! exit if end of block or end of file
    select case(trim(id))
      case("TYPE")  ! Only valid id (for now)
        call item2char(strings, 2, value, toupper=.true.)  ! Get type
        ! Internal Loop over pilot points, whatever type they are (global or class)
        do
          call reader%next_block_item(status, id, strings, length)
          if (status /= 0) exit  ! exit if end of block or end of file
          if (trim(id)=="TYPE") then ! move back one line & exit if it's the next class
            call reader%back(1)
            exit
          end if
          select case(value)
            case("GLOBAL","GLOBAL_AQUIFER")
              call t2p%pilot_points%read_pp_line(id, value, strings, length, 0)
            case("GLOBAL_AQUITARD")
              call t2p%pilot_points_aquitard%read_pp_line(id, value, strings, length, 0)
            case("AQUIFER")
              class_id = t2p%get_class_id(vstrlist_index(strings, 2))
              call t2p%pilot_points%read_pp_line(id, value, strings, length, class_id)
            case("AQUITARD")
              class_id = t2p%get_class_id(vstrlist_index(strings, 2))
              call t2p%pilot_points_aquitard%read_pp_line(id, value, strings, length, class_id)
            case DEFAULT
              call error_handler(1,reader%file,"Unknown Pilot Point Parameter Type: " // trim(value))
          end select
        end do ! End of pp reading loop
      case DEFAULT
        call error_handler(1,reader%file,"Unknown Pilot Point Parameter option: " // trim(id))
    end select
  end do

  ! Did we get all the pp data we needed?
  ! Todo: where should we check for aquitard pp values? Should this all be checked later?
  if (any(t2p%pilot_points%values(:,global_props(1):global_props(nglobal_props),:)==NODATA)) then
    call error_handler(2,reader%file,"Missing global parameters for pilot point(s)")
  else
    do i=1, t2p%ntexclasses
      if (any(t2p%pilot_points%values(:,1:global_props(1)-1,i)<0.0)) then
        call item2char(t2p%class_names, i, value)
        call error_handler(2,reader%file,"Missing parameters for pilot point(s) for texture: "//trim(value))
      end if
    end do
  end if

  ! May need to be replaced with setup hook
  t2p%has_pp = .true.

end subroutine read_ppparm_block

!-------------------------------------------------------------------------------------------------!

end module m_read_main_input