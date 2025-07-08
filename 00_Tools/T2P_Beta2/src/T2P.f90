module m_T2P
  use m_global, only: NODATA
  use m_vstringlist, only: t_vstringlist, vstrlist_search, vstrlist_append, vstrlist_range
  use m_vstring, only: t_vstring, vstring_toupper
  use m_error_handler, only: error_handler,warnings
  use m_options, only: t_options
  use m_datasets, only: t_layerdata, t_dataset, write_layerdata
  use m_flow_model, only: t_flow_model, t_iwfm_model
  use m_pilotpoints, only: t_pilotpoints
  use m_interpolator, only: t_interpolator, t_idw
  use m_kriging, only: t_krige, t_globkrige, t_treekrige
  use m_sgsim, only: t_sgsim
  use m_categories, only: t_category
  use m_vario, only: t_vgm
  use m_grid, only: t_grid, create_grid_object
!-------------------------------------------------------------------------------------------------!

#ifdef __INTEL_COMPILER
  integer :: nblank = 0
#else
  integer :: nblank = 1
#endif

  type t_T2P
    type(t_options)                  :: opt  ! avoid conflict with Fortran intrinsic 'options'
    class(t_flow_model),pointer      :: fmodel      => null()
    class(t_interpolator),pointer    :: interp_data => null()
    class(t_interpolator),pointer    :: interp_pp   => null()
    type(t_layerdata),pointer        :: datasets(:,:)           ! Datasets (dataset, layer) (1+nsecondary, layer)
    type(t_layerdata),pointer        :: dset_intermediate(:,:)  ! texture or parameters data interpolated to model grid (nsets, nlayers)
    type(t_layerdata),pointer        :: par(:)            ! Final model hydraulic parameters (nlayers)&values(npar, npoints)
    !type(t_vgm),pointer              :: variograms(:,:)   ! (stuctures, variogram)  ! Moved to interpolator
    type(t_pilotpoints),pointer      :: pilot_points
    type(t_pilotpoints),pointer      :: pilot_points_aquitard

    ! Counters
    integer                          :: npar        ! Kh, Kv, Ss, Sy
    integer                          :: nclasses    ! Number of data classes
    integer                          :: ntexclasses ! Texture datasets (for which hydraulic properties exist) (one may be inferred)
    integer                          :: nprimary    ! Number of active primary datasets for kriging (texture or parameter, see opt%INTERP_ORDER)
    integer                          :: nsecondary  ! Number of covariate data classes
    integer                          :: ndatasets   ! Number of dataset objects (one for primary datasets, n for secondary)
    integer                          :: nvario      ! Number of variograms, eventually passed to interpolator (if kriging)
    integer                          :: ndatafiles  ! Number of data files to read in
    integer                          :: nhsus       ! Number of Hydrostratigraphic Units (HSUs)
                                                    ! (Formerly "geologic zones")

    ! Trackers
    type(t_vstringlist),pointer      :: class_names           ! Data class names
    type(t_vstringlist)              :: parnames              ! Names of output parameters calculated
    integer,pointer                  :: nsecondary_by_id(:)=>null() ! Connections (covariates) per [class, hyd property]
    integer,pointer                  :: connections(:,:)   =>null() ! (connections, [classes or hyd properties])
    integer,allocatable              :: class_dataset(:)      ! Translation from class id to dataset_id, ie datasets(dataset_id, layer)
    type(t_vstringlist)              :: dataset_files         ! dataset filenames
    type(t_vstringlist)              :: dataset_trans         ! dataset transformations (same order as filenames)
    type(t_vstringlist)              :: dataset_trans_params  ! dataset transformation parameters (same order as filenames) (unimplemented)
    type(t_category),pointer         :: dataset_ppzones       ! Pilot point zone dataset grid arrays & tracking (when INTERP_ORDER=AFTER)

    ! Read trackers - used to check if a block has been read (and make sure it exists)
    logical                          :: has_opt
    logical                          :: has_classes
    logical                          :: has_covar
    logical                          :: has_interp
    logical                          :: has_vario
    logical                          :: has_fmodel
    logical                          :: has_pplocs
    logical                          :: has_pplocs_aquitards
    logical                          :: has_pp
    logical                          :: has_datafiles

    ! Groups (HSUs, PP Zones)
    logical                          :: has_hsus
    logical                          :: has_ppzones

  contains
    ! Setup procedures
    procedure,public   :: initialize
    procedure,public   :: process_options
    procedure,public   :: setup_flow_model
    procedure,public   :: setup_data_classes
    procedure,public   :: setup_covariates
    procedure,public   :: setup_variograms
    procedure,public   :: setup_pplocs
    procedure,public   :: setup_dataset_ppzones
    procedure,public   :: setup_categories   ! AKA HSUs & PPZones
    procedure,public   :: check_major_inputs
    !procedure,private  :: check_pp_zone_layertype_assignments
    procedure,private  :: nvario_estimator
    procedure,private  :: no_covariates

    ! Main Procedures
    procedure,public   :: predict
    procedure,private  :: interpolate_pp
    procedure,private  :: interpolate_model
    procedure,private  :: interpolate_prepare
    procedure,private  :: calc_block_parameters

    ! I/O Procedures
    procedure,public   :: write_output
    procedure,public   :: write_dataset_files
    procedure,public   :: write_dataset_file
    procedure,public   :: write_dataset_file_aqtard
    procedure,public   :: write_connections
    procedure,public,nopass   :: write_status
    procedure,public,nopass   :: write_minor_status

    ! Access procedures
    procedure, public  :: get_class_id
    !procedure, public  :: get_covariate_id
    procedure, public  :: get_parameter_id
    procedure, public  :: get_primary_id
    procedure, public  :: get_active_texture_classes
    procedure, public  :: get_connection_list
    procedure, public  :: get_dataset_idx_list
    procedure, public  :: get_dataval_idx_list

  end type t_T2P
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! SETUP PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize(this)
    use m_vstringlist, only: vstrlist_new, vstrlist_length
    implicit none
    class(t_T2P), intent(inout)      :: this

    ! Initialize Read Trackers
    this%has_opt          = .false.
    this%has_classes      = .false.
    this%has_covar        = .false.
    this%has_interp       = .false.
    this%has_vario        = .false.
    this%has_fmodel       = .false.
    this%has_pplocs       = .false.
    this%has_pp           = .false.
    this%has_datafiles    = .false.
    this%has_pplocs_aquitards = .false.
    ! And group trackers...
    this%has_hsus         = .false.
    this%has_ppzones      = .false.

    ! Initialize Elements
    call this%opt%initialize()
    call vstrlist_new (this%dataset_files)
    call vstrlist_new (this%dataset_trans)

    ! Initialize Aquifer Parameter list - MUST BE IN CAPS
    call vstrlist_new(this%parnames)
    call vstrlist_append (this%parnames, "KH" )
    call vstrlist_append (this%parnames, "KV" )
    call vstrlist_append (this%parnames, "SS" )
    call vstrlist_append (this%parnames, "SY" )
    this%npar = vstrlist_length(this%parnames)

    ! Intialize counters (useful to determine when a block is missing or has not been read yet)
    this%nclasses = 0
    this%nvario   = 0
    this%nsecondary = 0

    ! Set class defaults
    this%nhsus    = 1

    ! Setup warnings
    call vstrlist_new (warnings)  ! belongs to m_error_handler

    ! Open the overwrite unit
#ifdef __INTEL_COMPILER
    open(6, carriagecontrol='fortran')
#endif
  end subroutine initialize

!-------------------------------------------------------------------------------------------------!

  subroutine process_options(this)
    ! Hook for after options are read
    implicit none
    class(t_T2P), intent(inout)   :: this

    ! Handle Interpolation Method
    select case (this%opt%INTERP_METHOD)
      case("OK")
        if (this%opt%NSIM == 0) then
        allocate(t_treekrige::this%interp_data)
        else
          allocate(t_sgsim::this%interp_data)
        end if
        allocate(t_treekrige::this%interp_pp)
        this%opt%INTERP_SECOND_OK = .true.
      case("SK")
        allocate(t_globkrige::this%interp_data)
        allocate(t_treekrige::this%interp_pp)  ! Still should use OK - mean addition/subtraction not implemented!
        this%opt%INTERP_SECOND_OK = .true.
      case("IDW")
        allocate(t_idw::this%interp_data)
        allocate(t_idw::this%interp_pp)
        this%has_interp = .true.
      case DEFAULT
        call error_handler(1,opt_msg="Invalid Interpolation Method: " // this%opt%INTERP_METHOD)
    end select
    call this%interp_data%initialize()  ! Todo - custom parameters?
    call this%interp_pp%initialize()    ! Todo - custom parameters?

    ! Enforce if ONLY_TEXTURE
    if (this%opt%ONLY_TEXTURE) then
      this%opt%WRITE_NODE_FILES = .true.
      this%opt%WRITE_NONE = .true.
    end if

    ! Invalid combinations...
    if (this%opt%INTERP_ORDER=="AFTER") then
      if (this%opt%ONLY_TEXTURE) call error_handler(1,opt_msg='ONLY_TEXTURE option is only valid for INTERP_ORDER=="BEFORE"')
      if (this%opt%INFER_LAST_CLASS) call error_handler(1,opt_msg='Cannot use INFER_LAST_CLASS with INTERP_ORDER=="AFTER"')
    end if
    
    if (this%opt%WRITE_GRID_WEIGHTS.and.this%opt%READ_GRID_WEIGHTS) then
      call error_handler(1,opt_msg='Cannot both WRITE_GRID_WEIGHTS and READ_GRID_WEIGHTS - only one')
    end if
    
    if (this%opt%WRITE_PP_WEIGHTS.and.this%opt%READ_PP_WEIGHTS) then
      call error_handler(1,opt_msg='Cannot both WRITE_PP_WEIGHTS and READ_PP_WEIGHTS - only one')
    end if
    
    if (this%opt%nsim > 1) then
      if (this%opt%WRITE_GRID_WEIGHTS) call error_handler(1,opt_msg='Cannot WRITE_GRID_WEIGHTS while using SGSIM (NSIM>1)')
      if (this%opt%READ_GRID_WEIGHTS) call error_handler(1,opt_msg='Cannot READ_GRID_WEIGHTS while using SGSIM (NSIM>1)')
    end if

  end subroutine process_options

!-------------------------------------------------------------------------------------------------!

  subroutine setup_flow_model(this)
    implicit none
    class(t_T2P), intent(inout)   :: this

    if (len_trim(this%fmodel%sim_file)>0.or.len_trim(this%fmodel%grd_file)>0) then
      this%has_fmodel = .true.
    end if
    if (len_trim(this%fmodel%hsu_file)>0   )  this%has_hsus = .true.
    if (len_trim(this%fmodel%ppzone_file)>0)  this%has_ppzones = .true.
    if (this%fmodel%name=="GRID") this%opt%WRITE_NODE_FILES = .true.

  end subroutine setup_flow_model

!-------------------------------------------------------------------------------------------------!

  subroutine setup_data_classes(this, nclasses, class_names)
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer,intent(in)            :: nclasses
    type(t_vstringlist),pointer   :: class_names
    integer                       :: i,n

    this%nclasses = nclasses
    this%ntexclasses = nclasses
    this%class_names => class_names
    this%ndatasets = 1

    ! Allocate
    allocate(this%nsecondary_by_id(nclasses))
    this%nsecondary_by_id(1:) = 0

    call this%nvario_estimator()

    this%has_classes = .true.
  end subroutine setup_data_classes

!-------------------------------------------------------------------------------------------------!

  subroutine setup_covariates(this, nsecondary, connections)
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer,intent(in)            :: nsecondary
    type(t_vstringlist),pointer   :: covnames
    integer,pointer               :: connections(:,:)

    this%nsecondary   =  nsecondary
    this%nclasses     = this%nclasses + nsecondary
    this%ndatasets    = 1 + this%nsecondary
    this%connections => connections

    ! Now we can setup the dataset-class connection
    allocate(this%class_dataset(this%nclasses))
    this%class_dataset(1:) = 0

    call this%nvario_estimator()

    if (maxval(this%nsecondary_by_id) > 0) then
      this%has_covar = .true.
    end if
  end subroutine setup_covariates

!-------------------------------------------------------------------------------------------------!

  subroutine setup_dataset_ppzones(this)
    ! Some copying b/c we need PPZONE data at dataset points when opt%INTERP_ORDER=="AFTER"
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer :: nlayers, nnodes, i, j, idx

    ! Initialize...
    call this%dataset_ppzones%initialize(this%fmodel%ppzones%narr, this%fmodel%nlayers, this%fmodel%nnodes(1), 0)

    ! Copy over all categories
    this%dataset_ppzones%lay2arr = this%fmodel%ppzones%lay2arr
    this%dataset_ppzones%from_file = this%fmodel%ppzones%from_file
    ! Loop over each array and add each category stored in arrcat
    do i = 1, this%fmodel%ppzones%narr
      do j = 1, this%fmodel%ppzones%arrncat(i)
        call this%dataset_ppzones%add_category_int(this%fmodel%ppzones%catlist(j, i), i, idx)
      end do
    end do

  end subroutine setup_dataset_ppzones

!-------------------------------------------------------------------------------------------------!

  subroutine nvario_estimator(this)
    ! Need x(x-1)/2 variograms per class where x is classes involved (prime + secondary)
    ! Called during setup_data_classes AND setup_covariates as more info comes in
    ! LATER nvario is passed to the interpolator (if kriging)
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer, allocatable          :: vario_map(:,:)  ! Keeps track of assigned variogram indices
    integer                       :: i, j, k, x, vidx, nconnected, nprimaries

    ! Determine number of primaries based on INTERP_ORDER
    if (this%opt%INTERP_ORDER == "BEFORE") then
      nprimaries = this%ntexclasses
      if (this%opt%INFER_LAST_CLASS) nprimaries = nprimaries - 1  ! Exclude inferred class
      allocate(vario_map(this%nclasses, this%nclasses))
    else !if (this%opt%INTERP_ORDER == "AFTER") then
      nprimaries = this%npar  ! Hydraulic properties
      allocate(vario_map(this%npar + this%nsecondary, this%npar + this%nsecondary))
    end if
    this%nprimary = nprimaries

    ! No need to setup if we're not kriging
    if (.not. (this%opt%INTERP_METHOD=="OK".or.this%opt%INTERP_METHOD=="SK")) return

    ! Initialize
    vidx = 0  ! Start variogram index counter
    vario_map = 0  ! Initialize to zero (unassigned)

    ! Loop over each primary dataset
    do i = 1, this%nprimary
      ! Variogram for the primary dataset itself
      vidx = vidx + 1
      vario_map(i, i) = vidx  ! Assign variogram index

      if (associated(this%connections)) then  ! has covariates
        ! Loop over datasets connected to this primary
        do j = 1, this%nsecondary_by_id(i)+1
          k = this%connections(j, i)  ! Connected dataset ID
          if (this%opt%ASYMMETRIC_COV) then
            ! Assign asymmetric variograms for i -> k
            if (vario_map(i, k) == 0) then
              vidx = vidx + 1
              vario_map(i, k) = vidx
            end if
          else
            ! Assign symmetric variograms for i <-> k
            if (vario_map(i, k) == 0) then
              vidx = vidx + 1
              vario_map(i, k) = vidx
              vario_map(k, i) = vidx  ! Enforce symmetry
            end if
          end if
        end do
      end if
    end do

    ! Loop over secondary-secondary pairs (assumed always symmetric)
    do i = 1, this%nsecondary
      k = this%ntexclasses + i  ! Secondary datasets start after primaries
      vidx = vidx + 1
      vario_map(k, k) = vidx  ! Self-variogram for secondary

      do j = i + 1, this%nsecondary  ! Only consider i < j to avoid duplicates
        k = this%ntexclasses + j
        if (vario_map(i, k) == 0) then
          vidx = vidx + 1
          vario_map(i, k) = vidx
          vario_map(k, i) = vidx  ! Enforce symmetry
        end if
      end do
    end do

    ! Add Pilot Point Variogram (if used/kriging)
    if (.not. this%opt%ONLY_TEXTURE) then
      vidx = vidx + 1
      select type(p => this%interp_pp)
        class is(t_krige)
          if (.not. allocated(p%global_vario_idx)) allocate(p%global_vario_idx(1, 1, 1))
          p%global_vario_idx(1, 1, 1) = 1  ! Assign to pilot point interpolator
      end select
    end if

    ! Update the total number of variograms
    this%nvario = vidx

    ! Pass variogram indices to the kriging object
    select type(p => this%interp_data)
      class is(t_krige)
        if (allocated(p%global_vario_idx)) deallocate(p%global_vario_idx)
        allocate(p%global_vario_idx(0:1, size(vario_map, 1), size(vario_map, 2)))
        do i=0, 1
          p%global_vario_idx(i,:,:) = vario_map
        end do
      class default
        call error_handler(4, opt_msg="Unexpected non-kriging interpolator detected.")
    end select

    ! Deallocate the tracking matrix
    if (allocated(vario_map)) deallocate(vario_map)

  end subroutine nvario_estimator

!-------------------------------------------------------------------------------------------------!
  subroutine no_covariates(this)
  ! Run during check_major_inputs (currently before datasets are read)
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer                       :: i

    allocate(this%connections(1,this%nprimary))
    this%nsecondary_by_id(:) = 0

    ! Setup matrix, values indicate variogram ID. Can start off assuming each class has a variogram (fill diagonal)
    this%connections = 0
    do i = 1, this%nprimary
      this%connections(1, i) = i
    end do

    ! Now we can setup the dataset-class connection
    allocate(this%class_dataset(this%nclasses))
    this%class_dataset(1:) = 0

    !call this%nvario_estimator()

  end subroutine no_covariates
!-------------------------------------------------------------------------------------------------!

  subroutine setup_variograms(this, variograms, nnear, nstruct)
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer, pointer              :: nnear(:), nstruct(:)
    type(t_vgm),pointer           :: variograms(:,:)
    class(*),pointer              :: p
    integer                       :: nsets, nclasses

    nsets = this%nprimary+this%nsecondary

    ! Place directly in the kriging interpolator objects
    select type(p => this%interp_data)
      class is(t_krige)
        ! First n are for classes/properties
        p%nvario     = this%nvario
        if (.not. this%opt%ONLY_TEXTURE) p%nvario = this%nvario -1
        p%nstruct    => nstruct(1:p%nvario)
        p%variograms => variograms(:,1:p%nvario)
        p%nnear      => nnear(1:this%nclasses)
    end select
    ! PP is the last one
    if (.not. this%opt%ONLY_TEXTURE) then
      select type(p => this%interp_pp)
        class is(t_krige)
          p%nvario     = 1
          p%nstruct    => nstruct(this%nvario:this%nvario)
          p%variograms => variograms(:,this%nvario:this%nvario)
          p%nnear      => nnear(this%nclasses+1:this%nclasses+1)
      end select
    end if

    this%has_interp = .true.
    this%has_vario  = .true.
  end subroutine setup_variograms

!-------------------------------------------------------------------------------------------------!

  subroutine setup_pplocs(this, pp_id, nzones, zone_tracker, p)
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer, pointer              :: pp_id(:)
    integer, intent(in)           :: nzones, zone_tracker(:)
    integer                       :: i
    type(t_pilotpoints),pointer   :: p

    ! Called for both aquifer & aquitard pilot points
    ! p is a pointer to either the aquifer or aquitard pilot point t2p objects

    p%pp_id => pp_id
    allocate(p%zonelist(nzones))

    p%nppzones = nzones
    do i=1, nzones
      p%zonelist(i) = zone_tracker(i)
    end do

    ! But at some point we have to know which it is!
    if (associated(p, this%pilot_points)) then
      this%has_pplocs = .true.
    else if (associated(p, this%pilot_points_aquitard)) then
      this%has_pplocs_aquitards = .true.
    end if

  end subroutine setup_pplocs

!-------------------------------------------------------------------------------------------------!
  
  subroutine setup_categories(this)
    implicit none
    class(t_T2P), intent(inout)   :: this
    
    ! Read in category files
    call this%fmodel%read_category_files()
    this%nhsus = this%fmodel%HSUS%ncat
    
    ! Setup HSU variograms
    select type(p => this%interp_data)
      class is(t_krige)
        allocate(p%cat2zone(this%fmodel%hsus%ncat))
        p%cat2zone = 0
        call p%init_zones(this%fmodel%hsus%strlist_glo)
    end select
      
    ! Setup PP zone variograms - don't have different variograms (yet)
    if (.not. this%opt%ONLY_TEXTURE) then
      select type(p => this%interp_pp)
        class is(t_krige)
          allocate(p%cat2zone(this%pilot_points%nppzones))
          p%cat2zone = 1
      end select
    end if
    
  end subroutine setup_categories
  
!-------------------------------------------------------------------------------------------------!

  subroutine check_major_inputs(this)
    ! Run in main Texture2Par file, currently before datasets are read but after fmodel/categories
    implicit none
    class(t_T2P), intent(inout)   :: this

    ! A real simple one - you can't just have one texture class (what about ONLY_TEXTURE?)
    if (this%ntexclasses < 2) call error_handler(1,opt_msg="Must have at least 2 texture classes")

    ! Do we have everything we need to run the model?
    if(.not. this%has_opt      ) call error_handler(1,opt_msg="Missing Required OPTIONS Block")
    if(.not. this%has_classes  ) call error_handler(1,opt_msg="Missing Required CLASSES Block")
    if(.not. this%has_fmodel   ) call error_handler(1,opt_msg="Missing Required FLOW_MODEL Block")
    if(.not. this%has_datafiles) call error_handler(1,opt_msg="Missing Required DATASET Block(s)")
    if(.not. this%has_covar    ) call this%no_covariates()  ! Allowable, just needs to be handled
    if(.not. this%has_interp   ) then
      if (this%opt%INTERP_METHOD=="OK".or.this%opt%INTERP_METHOD=="SK") then
        call error_handler(1,opt_msg="Missing Required VARIOGRAMS Block")
      else  ! Unknown interpolator parameters required??
        call error_handler(1,opt_msg="Missing Required Interpolation Method Parameters")
      end if
    end if

    if (.not. this%opt%ONLY_TEXTURE) then  ! Pilot point info
      if(.not. this%has_pplocs) call error_handler(1,opt_msg="Missing Required PP_LOCS Block")
      if(.not. this%has_pp    ) call error_handler(1,opt_msg="Missing Required PP_PARAMETERS Block")
      ! Check pilot point zones
      ! Doesn't ensure they are the same ID values...
      if (this%pilot_points%nppzones /= this%fmodel%get_active_ppzones()) then
        call error_handler(1,opt_msg="Mismatch between Pilot Point Zones assigned to Pilot Points vs Model Grid")
      end if

      ! Make sure pilot points and flow model agree on existance of aquitards
      ! TODO - more robust? Nothing checks to ensure values at this points
      if (this%has_pplocs_aquitards .neqv. this%fmodel%has_aquitards) then
        call error_handler(1,opt_msg="Mismatch in aquitard existance between Pilot Points and Model")
      end if
    end if

  end subroutine check_major_inputs

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! MAIN PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine predict(this)
    use m_grid, only: t_grid
    implicit none
    class(t_T2P), intent(inout)   :: this
    class(t_grid),pointer         :: pp_to                                      ! Where we interpolate the pilot points to
    type(t_category),pointer      :: cat_to                                     ! Categories the pilot points are interpolated using
    logical,allocatable           :: active_to(:)                               ! Whether the given to point is active (skipped if inactive)
    type(t_pilotpoints)           :: pp_intermediate(this%fmodel%ppzones%narr)  ! The pilot point data interpolated to pp_to (by ppzone array)
    integer                       :: i,j

    ! Allocate the final parameter objects
    allocate(this%par(this%fmodel%nlayers))

    if (this%opt%ONLY_TEXTURE) then
      ! If just texture, our jobs is very easy
      allocate(this%dset_intermediate(1,this%fmodel%nlayers))
      call this%write_status("Interpolating Datasets to "//trim(this%fmodel%name)//" Grid", nblank)
      call this%interpolate_model(this%datasets, this%dset_intermediate(1,1:))
      RETURN
    end if

    ! Assign PPTO_grid (data or fmodel)
    if (this%opt%INTERP_ORDER=="BEFORE") then
      call this%write_status("Interpolating Pilot Point Parameters to "//trim(this%fmodel%name)//" Grid")
      ! T2P 1.0 Default Method
      allocate(this%dset_intermediate(1,this%fmodel%nlayers))
      allocate(active_to(this%fmodel%grid(1)%ptrgrid%n))
      ! Uses layer 1 grid
      pp_to => this%fmodel%grid(1)%ptrgrid
      cat_to => this%fmodel%ppzones
      active_to = this%fmodel%active_nodes_flat  ! copy over
    else if (this%opt%INTERP_ORDER=="AFTER") then
      call this%write_status("Interpolating Pilot Point Parameters to Dataset Grid")
      allocate(this%dset_intermediate(size(this%datasets,1),this%fmodel%nlayers))
      allocate(active_to(this%datasets(1,1)%grid%n))
      this%dset_intermediate(this%nprimary:,1:) => this%datasets(this%nprimary:,1:)  ! All secondary data gets pointed over
      ! Uses primary dataset layer 1 grid
      ! Again, assumes uniformity of grid by layer
      pp_to => this%datasets(1,1)%grid
      cat_to => this%dataset_ppzones
      active_to = 1  ! all dataset points must be active
    end if

    ! Interpolate PP to PPTO_grid
    call this%interpolate_pp(pp_to, cat_to, active_to, pp_intermediate)

    if (this%opt%INTERP_ORDER=="BEFORE") then
      ! Krige textures to fmodel grid
      call this%write_status("Interpolating Datasets to "//trim(this%fmodel%name)//" Grid", nblank)
      call this%interpolate_model(this%datasets, this%dset_intermediate(1,1:))
      ! Calc parameters at fmodel grid locations (pp,texture,result)  Kh, Kv, Ss, Sy
      call this%write_status("Calculating Hydraulic Parameters", nblank)
      call this%calc_block_parameters(pp_intermediate, this%dset_intermediate(1,1:), this%par)

    else if (this%opt%INTERP_ORDER=="AFTER") then
      ! Calc parameters at data locations
    call this%write_status("Calculating Hydraulic Parameters", nblank)
      call this%calc_block_parameters(pp_intermediate, this%datasets(1,1:), this%dset_intermediate(1,1:))
      ! Krige parameters to fmodel grid
      call this%write_status("Interpolating Hydraulic Parameters to "//trim(this%fmodel%name)//" Grid")
      call this%interpolate_model(this%dset_intermediate, this%par)
    end if
    call this%write_status("Calculations finished!", nblank)

  end subroutine predict
!-------------------------------------------------------------------------------------------------!

  subroutine interpolate_pp(this, to_grid, to_categories, to_active, results)
    use m_grid, only: t_grid
    use m_weights, only: t_weights
    use m_pilotpoints, only: nproperties, naquitard_props, aquitard_props
    ! Pilot Point type in, Pilot Point type out
    implicit none
    class(t_T2P), intent(inout)     :: this
    type(t_pilotpoints)             :: results(this%fmodel%ppzones%narr)
    class(t_grid),pointer           :: to_grid
    type(t_category)                :: to_categories
    logical                         :: to_active(:)
    type(t_weights)                 :: weights
    integer                         :: i,j,k
    integer                         :: class_idx(1)
    type(t_pilotpoints)             :: obs_data(1)  ! a little trick to appease calc_weights
    character(5)                    :: temp
    real,pointer                    :: obs_vec(:)
    integer                         :: nrows, m

    class_idx(1) = 1

    ! Go by pilot point zone arrays (up to nlayers)
    do k=1, this%fmodel%ppzones%narr
      call this%write_minor_status('- Array:',k)

      ! Aquifer or Aquitard array?
      if (.not. this%fmodel%pparr_aqtard(k)) then
        obs_data(1) = this%pilot_points
      else
        obs_data(1) = this%pilot_points_aquitard
      end if

      ! Initialize weights & results for this array
      call weights%initialize(obs_data(1)%grid%n, to_grid%n)  !input_n, output_n
      call results(k)%initialize(to_grid%n, to_grid) ! Copies grid over
      allocate(results(k)%values(to_grid%n, size(obs_data(1)%values,2), this%ntexclasses))
      results(k)%values = NODATA

      ! Check if we have precalculated weights for the pilot point interpolation
      if (this%opt%READ_PP_WEIGHTS) then
        write(temp,'(i0)') k
        call weights%load_from_file('t2p_ppweights_arr'//trim(temp)//'.wts', this%opt%PP_WEIGHTS_TEXT)
      else
        call this%interp_pp%calc_weights(obs_data, &
                                         class_idx,  &
                                         to_grid,  &
                                         to_active, &
                                         weights%values, &
                                         to_categories%catlist(1:to_categories%arrncat(k),k), &
                                         to_categories%array(:,k),&
                                         this%opt)
      end if

      ! Write PP weights
      if (this%opt%WRITE_PP_WEIGHTS) then
        write(temp,'(i0)') k
        call weights%save_to_file('t2p_ppweights_arr'//trim(temp)//'.wts', this%opt%PP_WEIGHTS_TEXT)
      end if

      ! Apply weights and create results
      if (.not. this%fmodel%pparr_aqtard(k)) then
        do i=1, this%ntexclasses
          do j=1, nproperties
            !results(k)%values(:,j,i) = matmul(weights%values,obs_data(1)%values(:,j,i))
            ! Avoid copying
            call dgemv('N', to_grid%n, obs_data(1)%grid%n, 1.0, weights%values, to_grid%n, obs_data(1)%values(:,j,i), 1, 0.0, results(k)%values(:,j,i), 1)
          end do
        end do
      else ! Aquitard - subset of parameters
        do i=1, this%ntexclasses
          do j=1, naquitard_props
            !results(k)%values(:,aquitard_props(j),i) = matmul(weights%values,this%pilot_points_aquitard%values(:,aquitard_props(j),i))
            ! Avoid copying
            call dgemv('N', to_grid%n, this%pilot_points_aquitard%grid%n, 1.0, weights%values, to_grid%n, this%pilot_points_aquitard%values(:,aquitard_props(j),i), 1, 0.0, results(k)%values(:,aquitard_props(j),i), 1)
          end do
        end do
      end if
      ! reset weights
      weights%values = 0.0
    end do

  end subroutine interpolate_pp

!-------------------------------------------------------------------------------------------------!

  subroutine interpolate_model(this, obs, dout)
    use m_grid, only: t_grid
    use m_weights, only: t_weights

    implicit none
    class(t_T2P), intent(inout)   :: this
    class(t_layerdata)            :: obs(:,:)  ! (nprimary, nlayers)
    type(t_layerdata)             :: dout(this%fmodel%nlayers)
    integer                       :: k, i, j, x, k_active, obslayer
    integer                       :: wstart, wend
    integer,allocatable           :: class_idx(:), dataset_idx(:), dataval_idx(:)
    real                          :: v_adjust
    type(t_grid),pointer          :: modelgrid
    !type(t_weights)               :: weights

    ! Loop over model layers
    do k=1, this%fmodel%nlayers

      ! Initialize results
      if (this%opt%INFER_LAST_CLASS) then
        call dout(k)%initialize(this%fmodel%grid(1)%ptrgrid%n, this%nprimary+1, default_value=NODATA)
      else
        call dout(k)%initialize(this%fmodel%grid(1)%ptrgrid%n, this%nprimary, default_value=NODATA)
      end if
      !dout(k)%values = 0.0

      ! Check if layer is active (generally IWFM: means it has thickness)
      if (.not. this%fmodel%is_active_layer(k)) cycle
      call this%interpolate_prepare(k, obslayer, modelgrid)
      call this%write_minor_status('- Layer:',this%fmodel%get_layer_id(k))

      ! Loop over primary classes
      do i=1, this%nprimary
        ! Interpolate
        class_idx   = this%get_connection_list(i)           ! Classes involved
        dataset_idx = this%get_dataset_idx_list(class_idx)  ! What datasets those classes are in
        dataval_idx = this%get_dataval_idx_list(class_idx)  ! What index is associated with those values

        x = this%fmodel%hsus%lay2arr(k)  ! Array id for this layer
        call this%interp_data%calc(obs(dataset_idx,obslayer), &
                                            class_idx,  &
                                            dataval_idx,&
                                            modelgrid,  &
                                            this%fmodel%active_nodes_bylay(1:,k), &
                                            dout(k)%values(i,1:), &
                                            this%fmodel%hsus%catlist(1:this%fmodel%hsus%arrncat(x),x), &
                                            this%fmodel%hsus%array(1:,x),&
                                            this%opt,&
                                            k)
        ! Enforce values must be greater than zero
        where(dout(k)%values(i,1:) < 0.0) dout(k)%values(i,1:) = 0.0
        ! If we're kriging texture - must be below 1.0
        if (this%opt%INTERP_ORDER=="BEFORE") where(dout(k)%values(i,1:) > 1.0) dout(k)%values(i,1:) = 1.0
        ! Inactive cells should be NODATA
        where(.not.this%fmodel%active_nodes_bylay(1:,k)) dout(k)%values(i,1:) = NODATA
      end do
      ! If we need to, infer the last class
      if (this%opt%INFER_LAST_CLASS) then
        do i=1, size(dout(k)%values, dim=2)
          if (this%fmodel%active_nodes_bylay(i,k)) then
            dout(k)%values(this%nprimary+1,i) = 1.0 - sum(dout(k)%values(1:this%nprimary,i), dim=1)
          end if
        end do
      end if
    end do

  end subroutine interpolate_model

!-------------------------------------------------------------------------------------------------!

  subroutine interpolate_prepare(this, k, obslayer, modelgrid)
    ! MOU: prepare data for interpolation for layer `k`
    !TODO Would it be easier to just set z at the midpoint when reading in the model, and only use it when 3D kriging? -LS
    class(t_T2P), intent(inout)   :: this
    integer, intent(in )          :: k
    integer, intent(out)          :: obslayer
    type(t_grid),pointer          :: modelgrid

    if (this%opt%INTERP_DIM==3) then
      obslayer = 1
      if (this%fmodel%get_layer_id(k)==1) then
        modelgrid => create_grid_object(3, this%fmodel%grid(1)%ptrgrid%n)
        modelgrid%coords(1:2,:) = this%fmodel%grid(1)%ptrgrid%coords(1:2,:)
      end if
      modelgrid%coords(3,:) = (this%fmodel%elev(:, k-1) + this%fmodel%elev(:, k)) * 0.5 ! set the Z coordintes at node center
    else
      obslayer = k
      if (this%fmodel%get_layer_id(k)==1) modelgrid => this%fmodel%grid(1)%ptrgrid
    end if
  end subroutine
!-------------------------------------------------------------------------------------------------!

  subroutine calc_block_parameters(this, pp_obj, tex_obs, par_out)
    implicit none
    class(t_T2P), intent(inout)    :: this
    type(t_pilotpoints),intent(in) :: pp_obj(this%fmodel%ppzones%narr) ! Kmin, Kmax, Ss, Sy, Aniso, Kd, KHp, KVp, STp
    type(t_layerdata),intent(inout):: tex_obs(this%fmodel%nlayers)  !  values(ntexclasses, npoints)
    type(t_layerdata),intent(out)  :: par_out(this%fmodel%nlayers)  !values([Kh, Kv, Ss, Sy], npoints)
    integer                        :: i, k, r
    real    ,allocatable           :: depths(:),KhD(:,:),KvD(:,:)

    ! Loop over layers
    do k=1, this%fmodel%nlayers

      ! Initialize Par
      call par_out(k)%initialize(this%fmodel%nnodes(k), this%npar, default_value=NODATA)

      ! Check if layer is active (generally IWFM: means it has thickness)
      if (.not. this%fmodel%is_active_layer(k)) cycle
      par_out(k)%grid => this%fmodel%grid(this%fmodel%lay2grid(k))%ptrgrid
      call this%write_minor_status('- Layer:',this%fmodel%get_layer_id(k))

      ! Allocate arrays
      if (allocated(depths)) deallocate(depths)
      allocate(depths(this%fmodel%nnodes(k)))
      allocate(KhD(this%fmodel%nnodes(k), this%ntexclasses))
      allocate(KvD(this%fmodel%nnodes(k), this%ntexclasses))

      ! Get node midpoint depths
      call this%fmodel%get_midpoint_depths(k, depths)

      ! Get pilot point array  (point, parameter, class_id)
      r = this%fmodel%ppzones%lay2arr(k)

      do i=1, this%fmodel%nnodes(k)
        ! Skip if any texture class value is missing/invalid
        if (any(tex_obs(k)%values(:,i) < 0.0)) cycle
        ! Skip if cell is inactive (would the above cycle catch that?)
        if (.not.this%fmodel%active_nodes_bylay(i,k)) cycle
        ! Ensure classes add to one
        tex_obs(k)%values(:,i) = tex_obs(k)%values(:,i) / sum(tex_obs(k)%values(:,i))
        ! Kh - Depth adjusted
        KhD(i,:) = pp_obj(r)%values(i, 1, :) + pp_obj(r)%values(i, 2, :) * exp(-1 *pp_obj(r)%values(i, 6, :) * depths(i))
        ! Kv - Depth adjusted, calculated from anisotropy
        KvD(i,:) = KhD(i,:) / pp_obj(r)%values(i, 5, :)
        ! Kh
        par_out(k)%values(1,i) = DOT_PRODUCT(KhD(i,:)**pp_obj(r)%values(i, 7, :), tex_obs(k)%values(:,i)) ** (1.0 / pp_obj(r)%values(i, 7, 1))
        ! Kv
        par_out(k)%values(2,i) = DOT_PRODUCT(KvD(i,:)**pp_obj(r)%values(i, 8, :), tex_obs(k)%values(:,i)) ** (1.0 / pp_obj(r)%values(i, 8, 1))
        ! Ss
        par_out(k)%values(3,i) = DOT_PRODUCT(pp_obj(r)%values(i,3,:)**pp_obj(r)%values(i, 9, :), tex_obs(k)%values(:,i)) ** (1.0 / pp_obj(r)%values(i, 9, 1))
        ! Sy
        par_out(k)%values(4,i) = DOT_PRODUCT(pp_obj(r)%values(i,4,:)**pp_obj(r)%values(i, 9, :), tex_obs(k)%values(:,i)) ** (1.0 / pp_obj(r)%values(i, 9, 1))
      end do

      deallocate(KhD, KvD)

      ! To match T2P 1 (move somewhere else? Do for all layers?)
      if (this%fmodel%lay_aqtard(k)) then
        where ((this%fmodel%elev(:,k-1) - this%fmodel%elev(:,k)) < 1e-6)
          par_out(k)%values(2,:) = 0.0
        end where
      end if

    end do

  end subroutine calc_block_parameters

!-------------------------------------------------------------------------------------------------!

  subroutine write_output(this)
    implicit none
    class(t_T2P), intent(inout)   :: this
    type(t_layerdata),pointer     :: p(:)

    call this%write_status("Writing output files")

    ! Write parameters by node/cell, and if texture was kriged, textures by node/cell
    if (.not. (this%opt%WRITE_NODE_FILES.and.this%opt%ONLY_TEXTURE)) then
      call this%write_minor_status('Writing Parameter Node Files')
      call this%fmodel%write_node_files(this%par, this%parnames)
    end if
    if (this%opt%WRITE_NODE_FILES.and.this%opt%INTERP_ORDER=="BEFORE") then
      call this%write_minor_status('Writing Class Node Files')
      p=>this%dset_intermediate(1,:)
      call this%fmodel%write_node_files(p, vstrlist_range(this%class_names, 1, this%ntexclasses))
    end if

    ! Write the observed datasets that were averaged by layer
    if (this%opt%WRITE_DATASET_FILES) then
      if (this%opt%INTERP_DIM==2) call this%write_minor_status('Writing Layer-Averaged Dataset File(s)')
      if (this%opt%INTERP_DIM==3) call this%write_minor_status('Writing Dataset XYZ File(s)')
      call this%write_dataset_files()
    end if

    if (.not. this%opt%WRITE_NONE) then
      ! Write out flow model input files
      call this%write_minor_status("Writing "//trim(this%fmodel%name)//" Input Files")
      call this%fmodel%write_fmodel_input(this%par)
    end if

    call this%write_minor_status('Done Writing.')
  end subroutine write_output

!-------------------------------------------------------------------------------------------------!

  subroutine write_dataset_files(this)
    use m_file_io, only: item2char
    implicit none
    class(t_T2P), intent(inout)   :: this
    integer                       :: i
    character(100)                :: fname, suffix(2), temp
    type(t_layerdata),pointer     :: p(:)

    if (this%opt%INTERP_DIM==2) then
      suffix(1) = '_layavg.csv'
      suffix(2) = '_aqtard_layavg.csv'
    else if (this%opt%INTERP_DIM==3) then
      suffix(1) = '_logxyz.csv'
      suffix(2) = '_aqtard_logxyz.csv'
    end if

    do i=1, this%get_active_texture_classes()
      p=>this%datasets(1,:)
      call item2char(this%class_names, i, temp)

      write(fname,'(3a)') trim(this%opt%PROJECT_NAME)//'_', trim(temp), trim(suffix(1))
      call this%write_dataset_file(fname, p, i)
      if (this%fmodel%has_aquitards) then
        write(fname,'(3a)') trim(this%opt%PROJECT_NAME)//'_', trim(temp), trim(suffix(2))
        call this%write_dataset_file_aqtard(fname, p, i)
      end if
    end do

    do i=1, this%nsecondary
      call item2char(this%class_names, this%ntexclasses + i, temp)
      write(fname,'(3a)') trim(this%opt%PROJECT_NAME)//'_', trim(temp), trim(suffix(1))
      p=>this%datasets(1+i,:)
      call this%write_dataset_file(fname, p,1)
    end do

  end subroutine write_dataset_files

!-------------------------------------------------------------------------------------------------!

  subroutine write_dataset_file(this, fname, dataset, idx, aquitard)
    use m_flow_model, only: t_flow_model,t_modflow_model, t_iwfm_model
    use m_file_io, only: t_file_writer, open_file_writer
    implicit none
    class(t_T2P), intent(inout)   :: this
    character(*)                  :: fname
    type(t_layerdata),pointer     :: dataset(:)  ! (nlayers)
    integer, intent(in)           :: idx
    class(t_flow_model),pointer   :: fm
    integer, intent(in), optional :: aquitard
    integer                       :: i, unit, kstart, kintrvl, nwell, aqt
    character(30), allocatable    :: rownames(:)
    character(10)                 :: nodecol, lname

    kstart  = 1   ! Start layer
    kintrvl = 1   ! Interval of layers
    aqt = 0       ! aquifer or aquitard
    nodecol = 'Node'
    lname = "Layer"
    if (present(aquitard)) then
      aqt = aquitard
      lname = "Aquitard"
    end if
    select type(fm=>this%fmodel)
      class is(t_iwfm_model)
        kstart  = 2 + aqt ! aquitard starts Layer 1
        kintrvl = 2
        nodecol = 'Element'
    end select

    nwell = dataset(1)%grid%n
    allocate(rownames(0:nwell))
    rownames(0) = "Well"
    do i=1, nwell
      ! TODO: use well names?
      write(rownames(i), "(I0)") i
    end do

    call write_layerdata(               &
      fname,                            &
      dataset,                          &
      dataset(1)%grid%coords,           &
      kstart,                           &
      kintrvl,                          &
      idx,                              &
      gridncol=this%fmodel%ncol,        &
      rownames=rownames,                &
      modelnodes=dataset(1)%fmodel_loc, &
      mdim=this%opt%INTERP_DIM,         &
      nodecol=nodecol)
  end subroutine write_dataset_file

!-------------------------------------------------------------------------------------------------!

  subroutine write_dataset_file_aqtard(this, fname, dataset, idx)
    use m_flow_model, only: t_flow_model,t_modflow_model, t_iwfm_model
    use m_file_io, only: t_file_writer, open_file_writer
    ! Only IWFM implemented - really should use fmodel%lay_aqtard(k) to determine aquitards
    ! (not that there is another fmodel with aquitards we have implemented or plan to...)
    implicit none
    class(t_T2P), intent(inout)   :: this
    character(*)                  :: fname
    type(t_layerdata),pointer     :: dataset(:)  ! (nlayers)
    integer, intent(in)           :: idx
    class(t_flow_model),pointer   :: fm
    integer                       :: i, unit, nwell
    character(30), allocatable    :: rownames(:)

    call this%write_dataset_file(fname, dataset, idx, aquitard=-1)

  end subroutine write_dataset_file_aqtard

!-------------------------------------------------------------------------------------------------!
  
  subroutine write_connections(this)
    use m_global, only: log
    use m_vstringlist
    use m_vstring, only: vstring_cast
    implicit none
    class(t_T2P), intent(inout)  :: this
    integer                      :: i,j
    character(128)               :: line, temp, id
    
    if (this%has_covar) then
      call log%write_line('-- INTERPOLATION SETUP: [Primary] : [Secondary]', blank_start=1)
      do i=1, this%nprimary
        write(id, '(i)') i
        call vstring_cast(vstrlist_index(this%class_names, i), temp)
        line = '  ' // trim(adjustl(id)) // '. ' // trim(temp) // ' :'
        if (this%nsecondary_by_id(i)==0) then
          line = line // ' [NONE]'
          cycle
        end if
        do j=1, this%nsecondary_by_id(i)
          call vstring_cast(vstrlist_index(this%class_names, this%connections(j+1,i)), temp)
          if (j==1) then
            line = trim(line) // ' ' // trim(temp)
          else
            line = trim(line) // ', ' // trim(temp)
          end if
        end do
        call log%write_line(line)
      end do
    else
      call log%write_line('-- INTERPOLATION SETUP: (no covariates)', blank_start=1)
      do i=1, this%nprimary
        write(id, '(2x,i)') i
        call vstring_cast(vstrlist_index(this%class_names, i), temp)
        line = trim(id) // '. ' // trim(temp)
        call log%write_line(line)
      end do
    end if
    
  end subroutine write_connections
  
!-------------------------------------------------------------------------------------------------!

  subroutine write_status(line, blank_start, blank_end)
    use m_global, only: log
    implicit none
    character(len=*)                 :: line
    integer,intent(inout),optional   :: blank_start, blank_end
    integer                          :: i

    ! To log
    call log%write_line(line, blank_start, blank_end)

    ! To screen
    if (present(blank_start)) then
      do i=1, blank_start
        write(*,*)
      end do
    end if

    write(*,fmt='(2x,a)') line

    if (present(blank_end)) then
      do i=1, blank_end
        write(*,*)
      end do
    end if

  end subroutine write_status

!-------------------------------------------------------------------------------------------------!

  subroutine write_minor_status(line, value)
      use m_global, only: log
    implicit none
    character(len=*)                 :: line
    integer,intent(in),optional      :: value
    character(100)                   :: line_out, temp

    if (present(value)) then
      write(temp,'(i)') value
    else
      temp = ''
    end if

    write(line_out,'(2x,a98)') line//' '//temp

    ! To log
    call log%write_line(line_out)

    ! To Screen (with overwrite)
#ifdef __INTEL_COMPILER
    write(6,'("+",2x,a100)') line_out
#else
    write(6,fmt='(a1,2x,a100)',advance="no") char(13),line_out
#endif

  end subroutine write_minor_status

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! ACCESS PROCEDURES
!-------------------------------------------------------------------------------------------------!

  function get_class_id(this, string, adjust_for_infer) result (id)
    ! Returns zero if not found
    implicit none
    class(t_T2P), intent(inout)   :: this
    type(t_vstring),intent(in)    :: string
    integer                       :: id
    logical,optional,intent(in)   :: adjust_for_infer
    id = vstrlist_search(this%class_names, vstring_toupper(string), exact=.true.)
    if (present(adjust_for_infer)) then
      ! If you're inferring the last class, it gets off past one after ntexclasses
      if (adjust_for_infer.and.this%opt%INFER_LAST_CLASS.and.(id>this%ntexclasses)) id = id -1
    end if
    return
  end function get_class_id

!-------------------------------------------------------------------------------------------------!

  function get_parameter_id(this, string) result (id)
    ! Returns zero if not found
    implicit none
    class(t_T2P), intent(inout)   :: this
    type(t_vstring),intent(in)    :: string
    integer                       :: id
    id = vstrlist_search(this%parnames, vstring_toupper(string), exact=.true.)
    return
  end function get_parameter_id

!-------------------------------------------------------------------------------------------------!

  function get_primary_id(this, string) result(prime_id)
    ! Returns zero if not found
    implicit none
    class(t_T2P), intent(inout)   :: this
    type(t_vstring),intent(in)    :: string
    integer                       :: prime_id

    if (this%opt%INTERP_ORDER=="BEFORE") then
      prime_id = this%get_class_id(string, adjust_for_infer=.true.)
    else ! this%opt%INTERP_ORDER=="AFTER")
      prime_id = this%get_parameter_id(string)
    end if

  end function get_primary_id

!-------------------------------------------------------------------------------------------------!

  function get_connection_list(this,prime) result(connlist)
    implicit none
    class(t_T2P)            :: this
    integer,intent(in)      :: prime
    integer,allocatable     :: connlist(:)

    if (allocated(connlist)) deallocate(connlist)
    allocate(connlist(this%nsecondary_by_id(prime)))
    connlist = this%connections(1:1+this%nsecondary_by_id(prime),prime)
    return
  end function

!-------------------------------------------------------------------------------------------------!

  function get_dataset_idx_list(this,class_ids) result(dataset_idx)
    implicit none
    class(t_T2P)            :: this
    integer,intent(in)      :: class_ids(:)
    integer,allocatable     :: dataset_idx(:)

    if (allocated(dataset_idx)) deallocate(dataset_idx)
    allocate(dataset_idx(size(class_ids)))
    dataset_idx(:) = this%class_dataset(class_ids)
    return
  end function

!-------------------------------------------------------------------------------------------------!

  function get_dataval_idx_list(this, class_ids) result (dataval_idx)
    implicit none
    class(t_T2P)            :: this
    integer,intent(in)      :: class_ids(:)
    integer,allocatable     :: dataval_idx(:)
    integer                 :: nactive

    if (allocated(dataval_idx)) deallocate(dataval_idx)
    nactive = this%get_active_texture_classes()
    dataval_idx = class_ids
    where (dataval_idx>nactive) dataval_idx = 1  ! All covariates use idx=1 for values

  end function get_dataval_idx_list

!-------------------------------------------------------------------------------------------------!

  function get_active_texture_classes(this) result(n)
    implicit none
    class(t_T2P)            :: this
    integer                 :: n
    n = this%ntexclasses
    if (this%opt%INFER_LAST_CLASS) n = n-1
    return
  end function get_active_texture_classes

!-------------------------------------------------------------------------------------------------!

  subroutine finalize(this)
    implicit none
    class(t_T2P)            :: this
    ! Main data storage
    if(associated(this%fmodel               )) deallocate(this%fmodel               )
    if(associated(this%interp_data          )) deallocate(this%interp_data          )
    if(associated(this%interp_pp            )) deallocate(this%interp_pp            )
    if(associated(this%datasets             )) deallocate(this%datasets             )
    if(associated(this%dset_intermediate    )) deallocate(this%dset_intermediate    )
    if(associated(this%par                  )) deallocate(this%par                  )
    if(associated(this%pilot_points         )) deallocate(this%pilot_points         )
    if(associated(this%pilot_points_aquitard)) deallocate(this%pilot_points_aquitard)

    ! Trackers
    if(allocated(this%class_dataset    )) deallocate(this%class_dataset   )
    if(associated(this%class_names     )) deallocate(this%class_names     )
    if(associated(this%nsecondary_by_id)) deallocate(this%nsecondary_by_id)
    if(associated(this%connections     )) deallocate(this%connections     )
    if(associated(this%dataset_ppzones )) deallocate(this%dataset_ppzones )

    ! close overwrite unit
    close(6)

  end subroutine finalize
!-------------------------------------------------------------------------------------------------!
end module m_T2P