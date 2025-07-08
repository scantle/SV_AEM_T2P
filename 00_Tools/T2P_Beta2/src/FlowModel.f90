module m_flow_model
  use m_file_io
  use m_grid, only: t_grid, create_grid_object
  use m_error_handler, only: error_handler
  use m_categories, only: t_category
  use m_options, only: t_options
  !use kdtree2_module, only: kdtree2, kdtree2_create
!-------------------------------------------------------------------------------------------------!
! ADDING A NEW FLOW MODEL OR GRID TYPE
! 1. Create a new child class inheriting from t_flow_model or t_flow_fdmodel
! 2. The class need to override (at a minimum) the following methods from the base class:
!   - read_model
!   - find_in_grid
!   Other methods may need to be overridden as well, for instance, `initialize()` or `get_elev_at_point()`
!-------------------------------------------------------------------------------------------------!
  implicit none
  ! Module variables
  type(t_file_reader), pointer :: reader          ! For reading input files
  integer,parameter            :: max_node_elem_members = 30
!-------------------------------------------------------------------------------------------------!

  type t_grid_arr
    type(t_grid),pointer       :: ptrgrid            ! wrapper of grid to put in an array
  end type

  type :: t_flow_model  ! Abstract Base Class
  ! Base class contains some derived type variables (e.g., the IWFM preproc file) since otherwise
  ! the compiler gets upset when you try to set them on the polymorphic t2p%fmodel type
  ! The alternative was to write a lot of get/set-type methods that would have to be overwritten
  ! by each derived class.

    integer                       :: model_type              ! 1 - grid, 2 - IWFM, 3 - MODFLOW 2000
    real                          :: xoff,yoff,rot           ! Conversion from/to global coordinates
    integer                       :: nlayers                 ! Number of model layers
    integer                       :: ngrids                  ! Number of unique model grid (xy) layers
    integer                       :: ncol=0                  ! Number of columns for structured grid
    integer,allocatable           :: nnodes(:)               ! Number of nodes (points at which flow is solved) by layer
    integer,pointer               :: lay2grid(:)             ! Which grid each layer uses (e.g., same xy between layers)
    character(30)                 :: name                    ! For printing nicely to log/screen
    character(100)                :: preproc_file            ! IWFM main discretization file (empty for others)
    character(100)                :: sim_file                ! Simulation File or MODFLOW name file
    character(100)                :: ppzone_file             ! File of pilot point zones defined for each node
    character(100)                :: hsu_file                ! File of Hydrostratigraphic Units (HSUs) defined for each node
    character(100)                :: template_file           ! File copied for output_file
    character(3)                  :: grd_file_type           ! Character string of grid file type (GSF, ...)
    character(100)                :: grd_file                ! Grid specification file for grid type or MODFLOW-USG
    character(100)                :: output_file             ! File where aquifer parameters are written
    character(60)                 :: projectname             ! project name from T2P, used as output prefix
    real    , allocatable         :: elev(:,:)               ! Bottom Elevations for each layer (node, layer) (indexed from zero = model top)
    type(t_grid_arr),allocatable  :: grid(:)                 ! xy grids for layers
    type(t_category)              :: ppzones                 ! Pilot point zone fmodel grid arrays & tracking
    type(t_category)              :: hsus                    ! HSU fmodel grid arrays & tracking
    logical                       :: has_aquitards           ! For now, only in IWFM - but helps T2P w/ aquitard pilot points
    logical                       :: structured              ! T/F Structured (row/col) model
    logical,allocatable           :: lay_aqtard(:)           ! Tracks whether layer is an aquitard
    logical,allocatable           :: pparr_aqtard(:)         ! Tracks if a *pilot_point* array is an aquitard
    logical,allocatable           :: active_nodes_bylay(:,:) ! T/F for each node if it is active in the flow model (primarily for MODFLOW) (node,layer)
    logical,allocatable           :: active_nodes_flat(:)    ! Same as above, but flat (T if active in *any* layer) (used for PP interp)

    contains
      procedure,public            :: initialize
      procedure,public            :: read_model
      procedure,public            :: build_tree
      procedure,public            :: read_category_files
      procedure,public            :: write_input_summary
      procedure,public            :: find_in_grid
      procedure,public            :: get_elev_at_point
      procedure,public            :: get_midpoint_depths
      procedure,public            :: get_active_ppzones
      procedure,public            :: get_layer_id   ! For when layer =/ layer_id (e.g., IWFM aquitards): id = aquifer id... iwfm == k/2
      procedure,public            :: get_layer_no   ! Other direction k*2. TODO come up with a clearer name
      procedure,public            :: is_active_layer
      procedure,public            :: is_aquitard_layer
      procedure,public            :: write_fmodel_input
      procedure,public            :: write_node_files
      procedure,public            :: write_node_file
      procedure,public            :: write_node_file_aqtard
      procedure,public            :: cell2rowcol
      procedure,private           :: alloc_layers
      procedure,private           :: alloc_nodes

  end type t_flow_model
!-------------------------------------------------------------------------------------------------!
  type,extends(t_flow_model) :: t_flow_fdmodel    ! Finite Difference Model
    ! Ineligant solution to both MF-USG and Grid using the GSF
    integer                   :: nvert
    ! I am not sure we actually need either of these vert arrays since we have node coordinates
    !integer,allocatable       :: vertnnode(:)   ! Number of nodes that include each vertex
    !integer,allocatable       :: vert2node(:,:) ! Nodes that include each vertex
    ! These two are used to confirm a point is within a node (cell) in find_in_grid_gsf
    integer,allocatable       :: nodenvert(:)   ! Number of verticies attached to each node (vert)
    integer,allocatable       :: node2vert(:,:) ! ID of verticies attached to each node (vert, node)
    type(t_grid),pointer      :: vertgrid       ! xyz grid for vertices

    contains
      procedure,private       :: read_gsf
      !procedure,private       :: add_node2vert2node
  end type t_flow_fdmodel
!-------------------------------------------------------------------------------------------------!
  type, extends(t_flow_fdmodel) :: t_mgrid  ! GSF - rename?

  contains
    procedure,public          :: initialize => init_mgrid
    procedure,public          :: read_model => read_mgrid
    procedure,public          :: find_in_grid => find_in_grid_gsf

  end type
!-------------------------------------------------------------------------------------------------!
  type, extends(t_flow_model) :: t_simpgrid  ! X Y LayerElevations - "Finite Element" style
  contains
    procedure,public          :: initialize => init_simpgrid
    procedure,public          :: read_model => read_simpgrid
    procedure,public          :: find_in_grid => find_in_grid_simpgrid
    procedure,public          :: write_node_file => write_node_file_simpgrid
  end type t_simpgrid
!-------------------------------------------------------------------------------------------------!
  type, extends(t_flow_model) :: t_iwfm_model   ! I DO realize the "m" in IWFM is for model

    character(100)            :: elem_file
    character(100)            :: node_file
    character(100)            :: strt_file
    integer                   :: nelements
    integer,allocatable       :: nodeid(:)      ! IWFM Node ID for each node
    integer,allocatable       :: nodebyid(:)    ! Given an IWFM node ID, contains internal t2p node index
    integer,allocatable       :: elements(:,:)  ! Comprised of 3-4 nodes (inode, nelem)
    integer,allocatable       :: elemid(:)      ! IWFM Elem ID for each element
    integer,allocatable       :: elemnnodes(:)  ! Number of nodes forming each element
    integer,allocatable       :: nodenelem(:)   ! Number of elements that include each node
    integer,allocatable       :: node2elem(:,:) ! Elements that include each node (elemid,nodes)
    logical,allocatable       :: lay_exist(:)   ! Tracks whether layer has thickness > 0.00

  contains
    procedure,public          :: initialize => init_iwfm
    procedure,public          :: read_model => read_iwfm
    procedure,public          :: find_in_grid => find_in_grid_elem
    procedure,public          :: get_elev_at_point =>  get_elev_at_point_elem
    procedure,public          :: get_layer_id => get_layer_id_iwfm
    procedure,public          :: get_layer_no => get_layer_no_iwfm
    procedure,public          :: is_active_layer => is_active_layer_iwfm
    procedure,public          :: is_aquitard_layer => is_aquitard_layer_iwfm
    !procedure,public          :: get_active_ppzones => get_active_ppzones_iwfm
    procedure,public          :: write_fmodel_input => write_fmodel_input_iwfm
    procedure,public          :: write_node_file => write_node_file_iwfm
    procedure,public          :: write_node_file_aqtard => write_node_file_aqtard_iwfm
    procedure,private         :: iwfm_elem_elev_idw
    procedure,private         :: alloc_layers => alloc_layers_iwfm
    procedure,private         :: alloc_nodes  => alloc_nodes_iwfm
    procedure,private         :: alloc_elems
    procedure,private         :: init_nodebyid
    procedure,private         :: process_iwfm_layers
    procedure,private         :: add_elem2nodeelem

  end type
!-------------------------------------------------------------------------------------------------!
  type, extends(t_flow_fdmodel) :: t_modflow_model

  type(t_grid_arr),allocatable:: loc_grid(:)   ! LOCAL xy grids for layers (not global coords)
  integer                     :: layfile       ! LPF/UPW IUNIT number
  character(3)                :: gwftyp        ! LPF or UPW, for printing

  contains
    procedure,public          :: initialize => init_modflow2000
    procedure,private         :: alloc_layers => alloc_layers_mf
    procedure,private         :: alloc_nodes  => alloc_nodes_mf
    procedure,public          :: read_model => read_modflow2000
    procedure,public          :: find_in_grid => find_in_grid_cell
    procedure,public          :: write_node_file => write_node_file_mf
    procedure,public          :: write_fmodel_input => write_fmodel_input_mf
    procedure,private         :: get_cell_local
    procedure,private         :: calc_mfcell_centers

  end type
!-------------------------------------------------------------------------------------------------!
  
  type, extends(t_modflow_model) :: t_flow_fdgrid
  ! Basically a MODFLOW model defined by a simplied DIS file
  
  integer                     :: nx, ny      ! ncol, nrow
  real                        :: dx, dy, dz  ! delr, delc, delz

  contains
    procedure,public          :: initialize => init_fdgrid
    procedure,public          :: read_model => read_fdgrid
    procedure,public          :: write_fmodel_input => write_fdgrid_out
    procedure,private         :: get_cell_local => get_cell_local_fd
    procedure,private         :: calc_fdcell_centers
  end type

!-------------------------------------------------------------------------------------------------!
  
  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  function create_flow_model_object(model_type_char, projectname) result(object)
    implicit none

    character(*),intent(in)        :: model_type_char
    class(t_flow_model),pointer    :: object
    character(60), optional        :: projectname

    select case(model_type_char)
      case ("IWFM")
        allocate(t_iwfm_model::object)
      case ("MODFLOW")
        allocate(t_modflow_model::object)
      case ("GRID")
        allocate(t_flow_fdgrid::object)
      case ("GSF_GRID")
        allocate(t_mgrid::object)
      case("SIMPLE_GRID")
        allocate(t_simpgrid::object)
      case DEFAULT
        call error_handler(1,opt_msg="Unknown Flow Model Type Name: " // trim(model_type_char))
    end select

    if (present(projectname)) then
      object%projectname = trim(projectname)
    else
      object%projectname = "t2p"
    end if

    ! Initialize new object
    call object%initialize()

  end function create_flow_model_object

!-------------------------------------------------------------------------------------------------!

  subroutine check_id_validity(id, fmodel, valid_type)
    implicit none
    ! Checks (A) if flow model type has been set and (B) optionally if it is the correct type of
    ! input for the model type (valid_type)
    ! Used while reading in options
    class(t_flow_model),pointer  :: fmodel
    character(30)                :: id
    integer,optional             :: valid_type

    ! Is type set?
    if (.not. ASSOCIATED(fmodel)) then
      call error_handler(2,opt_msg="Must specify flow model type before other settings")
    else if (present(valid_type)) then
      ! Is it the right type for this setting?
      if (valid_type /= fmodel%model_type) then
        call error_handler(2,opt_msg="Wrong flow model type for option: " // id)
      end if
    end if

  end subroutine check_id_validity

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! BASE CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize(this)
    implicit none
    class(t_flow_model)       :: this

    this%model_type    = 0
    this%nlayers       = 0
    this%ncol          = 0
    this%preproc_file  = ''
    this%sim_file      = ''
    this%ppzone_file   = ''
    this%hsu_file      = ''
    this%template_file = ''
    this%output_file   = ''
    this%grd_file      = ''
    this%has_aquitards = .false.

  end subroutine initialize

!-------------------------------------------------------------------------------------------------!

  subroutine read_model(this, opt)
    ! Base class method for reading in [nonexistant] model input file(s)
    ! Should not be run
    implicit none
    class(t_flow_model)         :: this
    class(t_options),intent(in) :: opt

  end subroutine read_model

  !-------------------------------------------------------------------------------------------------!

  subroutine build_tree(this)
    ! build KDtree for model grid
    implicit none
    class(t_flow_model)       :: this

    integer                   :: igrid
    do igrid = 1, size(this%grid)
      if (.not. associated(this%grid(igrid)%ptrgrid%tree) .and. this%grid(igrid)%ptrgrid%n>1) call this%grid(igrid)%ptrgrid%build_tree()
    end do
  end subroutine build_tree

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_layers(this, lay2grid)
    implicit none
    class(t_flow_model)       :: this
    integer                   :: lay2grid(this%nlayers)
    integer                   :: i

    ! Allocate grid based on unique grids
    this%ngrids   = maxval(lay2grid)

    allocate(this%nnodes  (0:this%nlayers), &
             this%lay2grid(  this%nlayers), &
             this%grid    (  this%ngrids ), &
             this%lay_aqtard(this%nlayers)  )

    this%lay2grid = lay2grid
    this%lay_aqtard = .false.

  end subroutine alloc_layers

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_nodes(this)
    ! Called by subclasses as well
    implicit none
    class(t_flow_model)       :: this
    integer                   :: i

    allocate(              this%elev(maxval(this%nnodes(:)), 0:this%nlayers), &
             this%active_nodes_bylay(maxval(this%nnodes(:)),   this%nlayers), &
              this%active_nodes_flat(maxval(this%nnodes(:))))

    ! Initialize to active
    this%active_nodes_bylay = .true.
    this%active_nodes_flat  = .true.
    
    ! Loop over layers creating grid object
    do i=1, this%ngrids
      this%grid(i)%ptrgrid => create_grid_object(2, this%nnodes(this%lay2grid(i)))
    end do

  end subroutine alloc_nodes

!-------------------------------------------------------------------------------------------------!

  subroutine write_input_summary(this)
    use m_global, only: log
    implicit none
    class(t_flow_model)     :: this

    select type (this)
      class is (t_iwfm_model)
        call log%write_valueline('Number of Model Nodes',    this%nnodes(1))
        call log%write_valueline('Number of Model Elements', this%nelements)
        call log%write_valueline('Number of Aquifer Layers', count(this%lay_exist .and. .not. this%lay_aqtard))
        call log%write_valueline('Number of Active Aquitard Layers', count(this%lay_exist .and. this%lay_aqtard))
      class is (t_flow_fdmodel)
        call log%write_valueline('Number of Model Cells (All Layers)', sum(this%nnodes))
        call log%write_valueline('Number of Model Layers',      this%nlayers)
    end select

  end subroutine write_input_summary

!-------------------------------------------------------------------------------------------------!

  function find_in_grid(this, coords, max_outside_dist) result(res)
    ! Base class method, should not be run
    implicit none
    class(t_flow_model)     :: this
    real                    :: coords(:)
    real, intent(in)        :: max_outside_dist
    integer                 :: res

    res = 0

  end function find_in_grid

!-------------------------------------------------------------------------------------------------!

  subroutine get_elev_at_point(this, coords, point_elevs, locid)
    ! Returns elevations for each layer given coordinates.
    implicit none
    class(t_flow_model)     :: this
    real    , intent(in)    :: coords(:)
    real    , intent(out)   :: point_elevs(0:this%nlayers)
    integer, optional       :: locid      ! Containing cell id if known
    integer                 :: id

    if (present(locid)) then
      id = locid
    else
      id = this%find_in_grid(coords, max_outside_dist=0.0)
    end if

    point_elevs = this%elev(id,:)

  end subroutine get_elev_at_point

!-------------------------------------------------------------------------------------------------!

  subroutine get_midpoint_depths(this, k, depths)
    ! To be overloaded where necessary (should be the same for all models)...
    implicit none
    class(t_flow_model)                :: this
    integer,intent(in)                 :: k
    integer                            :: i
    real, intent(inout)                :: depths(:)

    !depths = this%elev(:,0) - ((this%elev(:,k-1) + this%elev(:,k)) / 2.0)
    
    ! Loop & compute depths to avoid large temporary arrays
    do i = 1, this%nnodes(k)
      depths(i) = this%elev(i,0) - ((this%elev(i,k-1) + this%elev(i,k)) * 0.5)
    end do

  end subroutine get_midpoint_depths

!-------------------------------------------------------------------------------------------------!

  function get_active_ppzones(this) result(nactive)
    ! To be overloaded where necessary
    implicit none
    class(t_flow_model)     :: this
    integer                 :: nactive
    !nactive = this%ppzones%ncat
    nactive = maxval(this%ppzones%arrncat(1:))
  end function get_active_ppzones

!-------------------------------------------------------------------------------------------------!

  function get_layer_id(this, k) result (id)
    ! To be overloaded where necessary
    implicit none
    class(t_flow_model)     :: this
    integer,intent(in)      :: k
    integer                 :: id
    id = k
    return
  end function get_layer_id

!-------------------------------------------------------------------------------------------------!

  function get_layer_no(this, k) result(no)
    ! To be overloaded where necessary
    implicit none
    class(t_flow_model)     :: this
    integer,intent(in)      :: k
    integer                 :: no
    no = k
    return
  end function get_layer_no

!-------------------------------------------------------------------------------------------------!

  subroutine read_category_files(this)
    ! Called after model read
    ! Assumes all layers have same number of nodes
    ! (Could be modified in the future by passing all this%nnodes)
    implicit none
    class(t_flow_model)     :: this
    logical,allocatable     :: not_used(:)

    ! Read ppzones file
    call this%ppzones%read_file(this%ppzone_file, this%nlayers, this%nnodes(1), 0, this%lay_aqtard, this%pparr_aqtard)

    ! Read hsus file
    call this%hsus%read_file(this%hsu_file, this%nlayers, this%nnodes(1), 1, this%lay_aqtard, not_used)

  end subroutine read_category_files

!-------------------------------------------------------------------------------------------------!

  function is_active_layer(this, k)
    ! To be overloaded where necessary (IWFM has zero-thickness layers, typically aquitards)
    implicit none
    class(t_flow_model)     :: this
    integer,intent(in)      :: k
    logical                 :: is_active_layer

    is_active_layer = .true.

  end function is_active_layer

!-------------------------------------------------------------------------------------------------!

  function is_aquitard_layer(this, k)
    ! To be overloaded where necessary (IWFM has aquitards)
    implicit none
    class(t_flow_model)     :: this
    integer,intent(in)      :: k
    logical                 :: is_aquitard_layer

    is_aquitard_layer = .false.

  end function is_aquitard_layer

!-------------------------------------------------------------------------------------------------!

  subroutine write_fmodel_input(this, par)
    ! Base class method, should not be run
    use m_datasets, only: t_layerdata
    implicit none
    class(t_flow_model)        :: this
    type(t_layerdata),pointer  :: par(:)

  end subroutine write_fmodel_input

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_files(this, datasets, dat_names)
    use m_vstringlist, only: t_vstringlist, vstrlist_length
    ! Calls methods overloaded by subclasses
    use m_datasets, only: t_layerdata
    implicit none
    class(t_flow_model)        :: this
    type(t_layerdata),pointer  :: datasets(:)  ! (nlayers)
    type(t_vstringlist)        :: dat_names
    character(100)             :: fname, temp
    integer                    :: i

    do i=1, vstrlist_length(dat_names)  ! ndat, hopefully
      call item2char(dat_names, i, temp)
      write(fname,'(3a)') trim(this%projectname)//'_', trim(temp), '.csv'
      call this%write_node_file(fname, datasets, i)
    end do

    if (this%has_aquitards) then
      call item2char(dat_names, 2, temp)
      write(fname,'(3a)') trim(this%projectname)//'_', trim(temp), '_aqtard.csv'
      call this%write_node_file_aqtard(fname, datasets, 2)  ! Only implemented for IWFM
    end if

  end subroutine write_node_files

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_file(this, fname, par, par_id)
    ! Base class method, should not be run
    use m_datasets, only: t_layerdata
    implicit none
    class(t_flow_model)        :: this
    character(*)               :: fname
    type(t_layerdata),pointer  :: par(:)
    integer,intent(in)         :: par_id

  end subroutine write_node_file

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_file_aqtard(this, fname, par, par_id)
    ! Base class method, should not be run
    use m_datasets, only: t_layerdata
    implicit none
    class(t_flow_model)        :: this
    character(*)               :: fname
    type(t_layerdata),pointer  :: par(:)
    integer,intent(in)         :: par_id

  end subroutine write_node_file_aqtard

!-------------------------------------------------------------------------------------------------!

  !subroutine add_node2vert2node(this, node, nvert)
  !  ! Not currently used
  !  implicit none
  !  class(t_flow_fdmodel)     :: this
  !  integer                   :: i, j, k
  !  integer, intent(in)       :: node, nvert
  !
  !  do j=1, nvert
  !    ! Increment node-element membership array
  !    this%vertnnode(this%node2vert(j)) = this%vertnnode(this%node2vert(j)) + 1
  !    ! Ensure we're not going over our max
  !    if (this%vertnnode(this%node2vert(j)) > max_node_elem_members) then
  !      write(*,*) 'ERROR - max_vert_node_members exceeded!'     ! a slight lie in variable name
  !      write(*,'(a,i7,a,i3)') 'vert ', this%node2vert(j), 'is in >', max_node_elem_members
  !      stop
  !    end if
  !    ! Otherwise, add to the array
  !    this%vert2node(this%vertnnode(this%node2vert(j)),this%node2vert(j)) = node
  !  end do
  !
  !end subroutine add_node2vert2node

!-------------------------------------------------------------------------------------------------!

  subroutine read_gsf(this, opt)
    implicit none
    class(t_flow_fdmodel)       :: this
    class(t_options),intent(in) :: opt
    type(t_vstring)             :: strtemp
    integer                     :: i, j, total_nodes, ngrids, nvert, node, lay, grid, itemp, &
                                   gridcount, nodevert
    integer,allocatable         :: nnodes(:), grid_count(:), lay2grid(:),temp_node_vert(:)
    real                        :: rtemp(3)
    character(200)              :: ctemp
    logical                     :: found

    ! Standard file reader variables
    integer                     :: eof
    type(t_vstringlist)         :: strings

    reader => open_file_reader(this%grd_file)

    call reader%next_item(eof, strings) ! Line 1 structured or unstructured (unused)
    ! Next couple lines are read if t_mgrid, but not if MODFLOW
    ! Additionally, arrays SHOULD already be allocated for MODFLOW
    select type (this)
      type is (t_mgrid)
        ! Line 2 Nodes for all layers, nlay, iz, ic (used if mgrid, not MF-USG)
        read(reader%unit, *) total_nodes, this%nlayers
        allocate(nnodes(this%nlayers))
        ! Line 3 (Custom T2P) nodes per layer (NODELAY in USG)
        call reader%next_item(eof, strings)
        ngrids = 1
        allocate(grid_count(this%nlayers), lay2grid(this%nlayers))
        if (vstring_match(vstrlist_index(strings,1),'CONSTANT')) then
          nnodes(:) = item2int(strings, 2)              ! same nnodes each layer
          lay2grid = 1
        else                                                 ! The hard way
          do i=1, this%nlayers
            nnodes(i) = item2int(strings, i)
          end do
          grid_count(1) = nnodes(1)
          do i=2, this%nlayers
            found = .false.
            do j=1, ngrids
              if (nnodes(i) == grid_count(j)) then
                found = .true.
                lay2grid(i) = j
                exit
              end if
            end do
            if (.not. found) then
              ngrids = ngrids + 1
              grid_count(ngrids) = nnodes(i)
              lay2grid(i) = ngrids
            end if
          end do
        end if
      type is (t_modflow_model)
        call reader%skip(2, eof)
    end select  ! Back to the easy stuff

    ! Allocate layers, nodes
    this%ngrids  = ngrids
    call this%alloc_layers(lay2grid)
    this%nnodes = nnodes
    call this%alloc_nodes()

    ! Line 4 Number of vertices
    read(reader%unit, *) nvert
    this%nvert = nvert
    allocate(this%vertgrid)
    this%vertgrid = create_grid_object(3, this%nvert)
    write(*,*) this%nnodes

    ! Read vertices
    do i=1, nvert
      read(reader%unit, *) this%vertgrid%coords(1:3,i)  ! xyz
    end do

    ! Read to get number of verticies associated with each node
    allocate(this%nodenvert(total_nodes))
    this%nodenvert = 0
    grid = 1
    gridcount = 0
    do i=1, total_nodes
      read(reader%unit, *) node, rtemp(1:3), lay, nodevert
      this%nodenvert(i) = nodevert
    end do

    ! Allocate vertex lists...
    !allocate(this%vertnnode(nvert), &
    !         this%vert2node(max_node_elem_members,nvert), &
    !         this%node2vert(maxval(this%nodenvert(:))))
    !this%vertnnode = 0
    !this%vert2node = 0
    allocate(temp_node_vert(maxval(this%nodenvert(:))), &
             this%node2vert(maxval(this%nodenvert(:)), total_nodes))
    this%node2vert = 0

    ! Now we backup... and read for vert2node, node x, y, [and a calculated z if grid, not MF-USG]
    call reader%back(total_nodes)
    grid = 1
    gridcount = 0
    do i=1, total_nodes
      temp_node_vert = 0
      read(reader%unit, *) itemp, rtemp(1:3), lay, nodevert, temp_node_vert(1:this%nodenvert(i))
      node = merge(itemp - sum(this%nnodes(2:lay)), itemp, lay > 1)
      this%node2vert(1:this%nodenvert(i), node) = temp_node_vert(1:this%nodenvert(i))
      !call this%add_node2vert2node(i, nodevert/2, this%node2vert)  ! Only connecting to top vertices
      if (lay2grid(lay) == grid) then
        ! Process node
        this%grid(grid)%ptrgrid%coords(1:2,i) = rtemp(1:2)
      end if
      ! Get mean top/bot if grid
      select type (this)
        type is (t_mgrid)
          if (node==1) write(*,*) 'REAL NODE FOUND LAYER', lay, itemp, node
          if (lay==1) then  ! top
            this%elev(node, lay-1) = sum(this%vertgrid%coords(3,temp_node_vert(1:nodevert/2))) / (nodevert/2)
          end if            ! bot
            this%elev(node, lay)   = sum(this%vertgrid%coords(3,temp_node_vert(nodevert/2+1:nodevert  ))) / (nodevert/2)
          ! Make sure top/bot make sense
          if (this%elev(node,lay) > this%elev(node,lay-1)) then
            write(ctemp,*) itemp
            call error_handler(2,opt_msg='Bad vertex elevation or sorting: Layer elevation calc error for node' // ctemp)
          end if
      end select
      if (node==this%nnodes(lay))then
      grid = grid+1                       ! just advance to next grid
      write(*,*) 'GRID FOUND LAYER', lay, grid
      end if
    end do

    call reader%close_file()

  end subroutine

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! GSF GRID CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine init_mgrid(this)
    implicit none
    class(t_mgrid)       :: this

    call initialize(this)
    this%model_type  = 1
    this%name = "GSF"
    this%structured = .false.
  end subroutine init_mgrid

!-------------------------------------------------------------------------------------------------!

  subroutine read_mgrid(this, opt)
    implicit none
    class(t_mgrid)              :: this
    class(t_options),intent(in) :: opt

    ! Potentially multiple types of grid files (although unimplemented so far)
    select case (this%grd_file_type)
      case('gsf')
        call this%read_gsf(opt)
      case DEFAULT
        call error_handler(2,opt_msg='Unknown Grid Type for file: ' // this%grd_file)
    end select

  end subroutine read_mgrid

!-------------------------------------------------------------------------------------------------!

  function find_in_grid_gsf(this, coords, max_outside_dist) result(node)
    use tools, only: pinpol
    ! Assumes coords is in GLOBAL coordinates
    ! Assumes uniform xy layers
    ! Returns 0 if not found
    ! in T2P 1.0, it gave an error within here if the cell was not found. However, seems best to
    ! let the calling routine handle the error - it has more context.
    implicit none
    class(t_mgrid)   :: this
    real                    :: coords(:)
    real, intent(in)        :: max_outside_dist
    integer                 :: i,j,node
    integer                 :: nodeids(5)
    real                    :: in_node
    real                    :: nodedist(5), elemx(max_node_elem_members), elemy(max_node_elem_members)

    node = 0
    call this%grid(this%lay2grid(1))%ptrgrid%get_nnear(coords, 5, nodeids, nodedist)

    ! Loop over elements of closest node
    do i=1, 5
      do j=1, this%nodenvert(nodeids(i))/2  ! Only the top vertices
        elemx(j) = this%vertgrid%coords(1, this%node2vert(j, nodeids(i)))
        elemy(j) = this%vertgrid%coords(2, this%node2vert(j, nodeids(i)))
      end do
      ! Check if the point is in this polygon
      in_node = pinpol(coords(1), coords(2), elemx, elemy, this%nodenvert(nodeids(i))/2)
      if (in_node >= 0.0d0 .or. abs(in_node) <= max_outside_dist**2) then  ! Greedy, treats on line as in
        node = nodeids(i)
        exit
      end if
    end do

  end function find_in_grid_gsf

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! SIMPLE GRID CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine init_simpgrid(this)
    implicit none
    class(t_simpgrid)       :: this

    call initialize(this)
    this%model_type  = 1
    this%name = "SIMPLE"
    this%structured = .false.
  end subroutine init_simpgrid

!-------------------------------------------------------------------------------------------------!

  subroutine read_simpgrid(this, opt)
    implicit none
    class(t_simpgrid)           :: this
    class(t_options),intent(in) :: opt
    integer                     :: i, id, grid_nodes
    integer,allocatable         :: lay2grid(:)

    ! Standard file reader variables
    integer                    :: eof
    type(t_vstringlist)        :: strings

    reader => open_file_reader(this%sim_file)

    ! Read in discretization line: nnodes, nlayers
    call reader%next_item(eof, strings)
    grid_nodes = item2int(strings, 1)
    this%nlayers = item2int(strings, 2)

    ! Allocate
    allocate(lay2grid(this%nlayers))
    lay2grid(:) = 1                           ! all layers use the same xy grid
    call this%alloc_layers(lay2grid)
    this%nnodes(:) = grid_nodes
    call this%alloc_nodes()

    ! One node per line
    call reader%read_to_next_line(eof)
    do i=1,this%nnodes(1)
      read(reader%unit,*) id, this%grid(1)%ptrgrid%coords(1,i), this%grid(1)%ptrgrid%coords(2,i), this%elev(i, :)
    end do

    call reader%close_file()

  end subroutine read_simpgrid

!-------------------------------------------------------------------------------------------------!

  function find_in_grid_simpgrid(this, coords, max_outside_dist) result(node)
    ! Assumes coords is in GLOBAL coordinates
    ! Assumes uniform xy layers
    ! Returns 0 if not found
    implicit none
    class(t_simpgrid)   :: this
    real                :: coords(:)
    real, intent(in)    :: max_outside_dist
    integer             :: i,j,node
    integer             :: nodeids(5)
    real                :: in_node
    real                :: nodedist(5), elemx(max_node_elem_members), elemy(max_node_elem_members)

    node = 0
    call this%grid(this%lay2grid(1))%ptrgrid%get_nnear(coords, 1, nodeids, nodedist)
    if (nodedist(1) <= max_outside_dist**2) then
      node = nodeids(1)
    end if
  end function find_in_grid_simpgrid

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_file_simpgrid(this, fname, par, par_id)
    use m_datasets, only: t_layerdata, write_layerdata
    implicit none
    class(t_simpgrid)        :: this
    character(*)               :: fname
    type(t_layerdata),pointer  :: par(:)
    integer,intent(in)         :: par_id
    integer                    :: i, k, unit
    character(60)              :: fmt(2)
    type(t_file_writer),pointer:: fwrite

    call write_layerdata(fname, par, this%grid(1)%ptrgrid%coords, 1, 1, par_id)

  end subroutine write_node_file_simpgrid

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! IWFM CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine init_iwfm(this)
    implicit none
    class(t_iwfm_model)       :: this

    call initialize(this)
    this%model_type  = 2
    this%name = "IWFM"
    this%structured = .false.
  end subroutine init_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_layers_iwfm(this, lay2grid)
    implicit none
    class(t_iwfm_model)       :: this
    integer                   :: i, lay2grid(this%nlayers)

    allocate(this%lay_exist (this%nlayers))
    this%lay_exist = .true.

    ! Call super
    call alloc_layers(this, lay2grid)

    ! Set lay_aqtard to true for every odd layer
    this%lay_aqtard = mod((/(i, i=1, this%nlayers)/), 2) == 1

  end subroutine alloc_layers_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_nodes_iwfm(this)
    ! Additional arrays required for IWFM due to Finite-element
    implicit none
    class(t_iwfm_model)       :: this

    allocate(this%node2elem(max_node_elem_members,this%nnodes(1)), &
             this%nodenelem(this%nnodes(1)),&
             this%nodeid   (this%nnodes(1)) )

    ! Call super
    call alloc_nodes(this)

    ! Initialize
    this%node2elem = 0
    this%nodenelem = 0

  end subroutine alloc_nodes_iwfm

!-------------------------------------------------------------------------------------------------!

  function find_in_grid_elem(this, coords, max_outside_dist) result(element)
    use tools, only: pinpol
    ! Assumes uniform xy layers
    ! Returns 0 if not found
    ! in T2P 1.0, it gave an error within here if the element was not found. However, seems best to
    ! let the calling routine handle the error - it has more context.
    implicit none
    class(t_iwfm_model)     :: this
    real                    :: coords(:)
    real, intent(in)        :: max_outside_dist
    integer                 :: element
    integer                 :: nodeids(5)
    real                    :: nodedist(5), elemx(4), elemy(4), inelem, mindist
    integer                 :: inode, i, j, count, elem, minelem

    element = 0
    minelem = 0
    mindist = huge(0.0)
    call this%grid(this%lay2grid(1))%ptrgrid%get_nnear(coords, 5, nodeids, nodedist)

    ! Loop over elements of closest node
    outerloop:  do inode=1, 5
      do i=1, this%nodenelem(nodeids(inode))
        elem = this%node2elem(i,nodeids(inode))
        count = 0
        ! Get Element node x,y coordinates
        do j=1, 4
          if (this%elements(j, elem) > 0) then
            elemx(j) = this%grid(this%lay2grid(1))%ptrgrid%coords(1, this%elements(j, elem))
            elemy(j) = this%grid(this%lay2grid(1))%ptrgrid%coords(2, this%elements(j, elem))
            count = count + 1
          end if
        end do
        ! Check if the point is in this polygon
        inelem = pinpol(coords(1), coords(2), elemx, elemy, count)
        if (inelem >= 0.0d0) then  ! Greedy, treats on line as in elem
          element = elem
          exit outerloop
        else if (abs(inelem) < mindist) then
          mindist = abs(inelem)
          minelem = elem
        end if
      end do
    end do outerloop

    ! Didn't find an exact match? is it at least within max_outside_dist?
    if (element == 0) then
      if (mindist <= max_outside_dist**2) element = minelem
    end if

  end function find_in_grid_elem

!-------------------------------------------------------------------------------------------------!

  subroutine get_elev_at_point_elem(this, coords, point_elevs, locid)
    ! Returns elevations for each layer given coordinates.
    implicit none
    class(t_iwfm_model)   :: this
    real    , intent(in)    :: coords(:)
    real    , intent(out)   :: point_elevs(0:this%nlayers)
    integer, optional       :: locid      ! Containing cell id if known
    integer                 :: elemid

    if (present(locid)) then
      elemid = locid
    else
      elemid = this%find_in_grid(coords, 0.0)
    end if

    call this%iwfm_elem_elev_idw(coords,elemid,point_elevs)

  end subroutine get_elev_at_point_elem

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_elems(this)
    implicit none
    class(t_iwfm_model)       :: this
    integer                   :: i

    allocate(this%elements(4,this%nelements), &
             this%elemnnodes(this%nelements), &
             this%elemid    (this%nelements)  )

    this%elements(4,this%nelements) = -1  ! initialize to obvious non-node value

  end subroutine alloc_elems

!-------------------------------------------------------------------------------------------------!

  subroutine init_nodebyid(this, maxnodeid)
    implicit none
    class(t_iwfm_model)       :: this
    integer,intent(in)        :: maxnodeid
    integer                   :: i

    allocate(this%nodebyid(maxnodeid))

    ! Initialize all values to a negative number so we can easily check whether
    ! a nodeid is valid
    this%nodebyid = -1

    ! Fill array
    do i=1, this%nnodes(i)
      this%nodebyid(this%nodeid(i)) = i
    end do

  end subroutine init_nodebyid

!-------------------------------------------------------------------------------------------------!

  subroutine process_iwfm_layers(this)
    implicit none
    class(t_iwfm_model)       :: this
    integer :: i

    ! Calculate maximum thickness (differences) between consecutive layers
    ! Determine if layer exists based on max thickness > 0.0
    this%lay_exist = maxval(abs(this%elev(:, 1:this%nlayers) - this%elev(:, 0:this%nlayers-1)), dim=1) > 0.0

    ! See if any of these are aquitard layers and exist
    this%has_aquitards = any(this%lay_aqtard .and. this%lay_exist)

    ! Correct lay_aqtard flags if layers do not exist (screws up ppzone array reading)
    this%lay_aqtard = this%lay_aqtard .and. this%lay_exist

  end subroutine process_iwfm_layers

!-------------------------------------------------------------------------------------------------!

  function is_active_layer_iwfm(this, k)
    ! To be overloaded where necessary (IWFM has zero-thickness layers, typically aquitards)
    implicit none
    class(t_iwfm_model)     :: this
    integer,intent(in)      :: k
    logical                 :: is_active_layer_iwfm

    is_active_layer_iwfm = this%lay_exist(k)

  end function is_active_layer_iwfm

!-------------------------------------------------------------------------------------------------!

  function is_aquitard_layer_iwfm(this, k)
    ! To be overloaded where necessary (IWFM has aquitards)
    implicit none
    class(t_iwfm_model)     :: this
    integer,intent(in)      :: k
    logical                 :: is_aquitard_layer_iwfm

    is_aquitard_layer_iwfm = this%lay_aqtard(k)

  end function is_aquitard_layer_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine add_elem2nodeelem(this, ielem)
    implicit none
    class(t_iwfm_model)       :: this
    integer                   :: i, j, k, node
    integer, intent(in)       :: ielem

    do j=1, 4
      node = this%elements(j,ielem)
      if (node > 0) then
        ! Increment node-element membership array
        this%nodenelem(node) = this%nodenelem(node) + 1
        ! Ensure we're not going over our max
        if (this%nodenelem(node) > max_node_elem_members) then
          write(*,*) 'ERROR - max_node_elem_members exceeded!'
          write(*,'(a,i7,a,i3)') 'node ', node, 'is in >', max_node_elem_members
          stop
        end if
        ! Otherwise, add to the array
        this%node2elem(this%nodenelem(node),node) = ielem
      end if
    end do

  end subroutine add_elem2nodeelem

!-------------------------------------------------------------------------------------------------!

  subroutine read_iwfm(this, opt)
    use fpath
    use m_vstringlist, only: t_vstringlist
    implicit none
    type(t_file_reader), pointer :: reader2
    class(t_iwfm_model)          :: this
    class(t_options),intent(in)  :: opt
    character(200)               :: path, ctemp
    real                         :: factor, rtemp1, rtemp2
    real    ,allocatable         :: rarray(:)
    integer                      :: i, j, itemp, inode, maxnodeid
    integer, allocatable         :: lay2grid(:)

    ! Standard file reader variables
    integer                    :: status, length, eof
    character(200)             :: value
    type(t_vstringlist)        :: strings

    ! Get relative path to later be used
    call fpath_strip(this%preproc_file, path, value)

    ! Preprocessor file
    reader => open_file_reader(this%preproc_file)
    call reader%read_to_next_iwfm_line(eof)
    call reader%skip(3, eof)                  ! Title
    call reader%read_to_next_iwfm_line(eof)
    call reader%skip(1, eof)                  ! Binary output
    call reader%next_item(eof, strings)       ! Element File
    call item2char(strings, 1, value)
    call fpath_join(path, value, this%elem_file)
    call reader%next_item(eof, strings)       ! Node File
    call item2char(strings, 1, value)
    call fpath_join(path, value, this%node_file)
    call reader%next_item(eof, strings)       ! Stratigraphy File
    call item2char(strings, 1, value)
    call fpath_join(path, value, this%strt_file)
    call reader%close_file()

    ! Stratigraphy file (only to nlayers)
    reader2 => open_file_reader(this%strt_file)
    call reader%read_to_next_iwfm_line(eof)
    call reader%next_item(eof, strings)       ! nlayers
    this%nlayers = item2int(strings, 1) * 2   ! Each layer has overlying aquitard (to be treated as layer)

    ! Allocate layers
    allocate(lay2grid(this%nlayers))
    lay2grid(:) = 1                           ! all layers use the same xy grid in IWFM
    call this%alloc_layers(lay2grid)

    ! Node file
    reader => open_file_reader(this%node_file)
    call reader%read_to_next_iwfm_line(eof)
    call reader%next_item(eof, strings)       ! nnodes
    this%nnodes(:) = item2int(strings, 1)     ! Node count does not vary by layer in IWFM
    call reader%next_item(eof, strings)       ! xy mult factor
    factor = item2dp(strings, 1)

    ! Allocate nodes
    call this%alloc_nodes()

    ! Continue Node file read
    call reader%read_to_next_iwfm_line(eof)
    maxnodeid = 0
    do i=1,this%nnodes(1)
      ! This marks the spot I realized that my reader class is far too slow for reading long datasets
      read(reader%unit,*) this%nodeid(i), rtemp1, rtemp2
      this%grid(1)%ptrgrid%coords(1,i) = rtemp1 * factor
      this%grid(1)%ptrgrid%coords(2,i) = rtemp2 * factor
    end do
    call this%init_nodebyid(maxval(this%nodeid)) ! Setup mapping from nodeid->array index
    call reader%close_file()                     ! Close Node file

    ! Continue stratigraphy file read
    call reader2%next_item(eof, strings)         ! elev, thickness mult factor
    factor = item2dp(strings, 1)
    call reader2%read_to_next_iwfm_line(eof)
    allocate(rarray(this%nlayers+1))
    do i=1,this%nnodes(1)
      read(reader2%unit,*) itemp, rarray
      inode = this%nodebyid(itemp)
      this%elev(inode,:) = rarray(1) * factor
      do j=1, this%nlayers                      ! Loop over layers subtracting thicknesses
        this%elev(inode,j:this%nlayers) = this%elev(inode,j:this%nlayers) - (rarray(1+j) * factor)
      end do
    end do
    call reader2%close_file()                   ! Close Stratigraphy file
    call this%process_iwfm_layers()             ! Find layers of zero thickness

    ! Elements file
    reader => open_file_reader(this%elem_file)
    call reader%read_to_next_iwfm_line(eof)
    call reader2%next_item(eof, strings)       ! Number of elements
    this%nelements = item2int(strings, 1)
    call reader%read_to_next_iwfm_line(eof)
    call reader2%next_item(eof, strings)       ! Number of regions
    itemp = item2int(strings, 1)
    call reader%read_to_next_iwfm_line(eof)
    call reader%skip(itemp, eof)               ! Skip region descriptions

    ! Allocate element arrays
    call this%alloc_elems()

    ! Read elements
    call reader%read_to_next_iwfm_line(eof)
    do i=1, this%nelements
      read(reader2%unit,*) this%elemid(i), this%elements(1:4,i)
      this%elemnnodes(i) = count(this%elements(1:4,i) > 0)

      ! Translate/Validate nodeid to node index for each connection
      do j=1,4
        if (this%elements(j,i) > 0) then
          if (this%nodebyid(this%elements(j,i)) < 1) then
            write(ctemp,'(i0)') this%elemid(i)
            call error_handler(2,opt_msg='Invalid node ID in Element File. Element Number = ' // trim(ctemp))
          end if
          this%elements(j,i) = this%nodebyid(this%elements(j,i))
        end if
      end do
      call this%add_elem2nodeelem(i)
    end do
    call reader%close_file()                   ! Close Element file

    ! Simulation file
    reader => open_file_reader(this%sim_file)
    call fpath_strip(this%sim_file, path, value)

    call reader%read_to_next_iwfm_line(eof)
    call reader%skip(3, eof)                   ! Title
    call reader%read_to_next_iwfm_line(eof)
    call reader%skip(1, eof)                   ! Binary file
    call reader%next_item(eof, strings)        ! GW File
    call item2char(strings, 1, value)
    call fpath_join(path, value, this%output_file)
    call reader%close_file()                   ! Close Simulation file

  end subroutine read_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine iwfm_elem_elev_idw(this,coords,ObsElem,InterpValues)
    use m_interpolator, only: dist
  ! Author: Marinko Karanovic, SSP&A
  ! Reworked for T2P v2 by Leland Scantlebury
  ! p hardcoded to 1, hardcoded to layer elevations
    implicit none
    class(t_iwfm_model)   :: this
    integer               :: j,k,nodeID
    integer               :: ObsElem
    real                  :: coords(2),InterpValues(:)
    real                  :: wgt_tmp,wgt
    InterpValues=0
    wgt=0
    do j=1,4
      if (ObsElem.gt.0) then
        nodeID = this%elements(j,ObsElem)
        if (nodeID.gt.0) then
          wgt_tmp = 1.0 / dist(coords, this%grid(1)%ptrgrid%coords(:2,nodeID)) ! Assumes all layers share xy
	        wgt = wgt + wgt_tmp
          InterpValues(:) =InterpValues(:) + wgt_tmp * this%elev(nodeID,:)
        end if
      end if
    end do
    InterpValues(:) =InterpValues(:) / wgt
  end subroutine iwfm_elem_elev_idw

!-------------------------------------------------------------------------------------------------!

  function get_layer_id_iwfm(this, k) result (id)
    implicit none
    class(t_iwfm_model)     :: this
    integer,intent(in)      :: k
    integer                 :: id
    id = ceiling(real(k)/real(2))
    return
  end function get_layer_id_iwfm

!-------------------------------------------------------------------------------------------------!

  function get_layer_no_iwfm(this, k) result(no)
    implicit none
    class(t_iwfm_model)     :: this
    integer,intent(in)      :: k
    integer                 :: no
    no = k*2
    return
  end function get_layer_no_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine write_fmodel_input_iwfm(this, par)
    use m_write_fmodel, only: writeIWFMgwfile
    use m_datasets, only: t_layerdata
    implicit none
    class(t_iwfm_model)        :: this
    type(t_layerdata),pointer  :: par(:)

    ! External for now
    call writeIWFMgwfile(this%template_file, this%output_file, par, this%nlayers, this%nnodes(1), this%nodeid, this%lay_aqtard, this%lay_exist)

  end subroutine write_fmodel_input_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_file_iwfm(this, fname, par, par_id)
    use m_datasets, only: t_layerdata, write_layerdata
    implicit none
    class(t_iwfm_model)        :: this
    character(*)               :: fname
    type(t_layerdata),pointer  :: par(:)
    integer,intent(in)         :: par_id
    integer                    :: i, k, unit
    character(60)              :: fmt(2)
    type(t_file_writer),pointer:: fwrite

    call write_layerdata(fname, par, this%grid(1)%ptrgrid%coords, 2, 2, par_id)

  end subroutine write_node_file_iwfm

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_file_aqtard_iwfm(this, fname, par, par_id)
    use m_datasets, only: t_layerdata, write_layerdata
    implicit none
    class(t_iwfm_model)        :: this
    character(*)               :: fname
    type(t_layerdata),pointer  :: par(:)
    integer,intent(in)         :: par_id
    integer                    :: i, k, unit
    character(60)              :: fmt(2)
    type(t_file_writer),pointer:: fwrite

    call write_layerdata(fname, par, this%grid(1)%ptrgrid%coords, 1, 2, par_id, lname="Aquitard")

  end subroutine write_node_file_aqtard_iwfm

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! FINITE-DIFFERENCE CLASS TYPE-BOUND PROCEDURES (MODFLOW, GSF)
!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! FINITE-DIFFERENCE GENERIC GRID (FDGRID)
!-------------------------------------------------------------------------------------------------!
  
  subroutine init_fdgrid(this)
    implicit none
    class(t_flow_fdgrid)       :: this

    call initialize(this)
    this%model_type  = 3
    this%structured  = .true.
    this%name = "GRID"
    this%grd_file_type = 'grd'
    this%nlayers = 0
    this%nx = 0
    this%ny = 0
    this%dx = 0.0
    this%dy = 0.0
    this%dz = 0.0

  end subroutine init_fdgrid
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine get_cell_local_fd(this, cell, xmin, xmax, ymin, ymax)
    ! Could consolidate by making generic along with MF version (get_cell_local)
    implicit none
    class(t_flow_fdgrid)   :: this
    integer,intent(in)     :: cell
    real    ,intent(out)   :: xmin,xmax,ymin,ymax
    integer                :: row, col

    call this%cell2rowcol(cell, row, col)
    xmin = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(1,cell) - 0.5 * this%dx
    xmax = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(1,cell) + 0.5 * this%dx
    ymin = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(2,cell) - 0.5 * this%dy
    ymax = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(2,cell) + 0.5 * this%dy

  end subroutine get_cell_local_fd
  
!-------------------------------------------------------------------------------------------------!
  subroutine calc_fdcell_centers(this)
    ! Both local and global (formerly was just global, causing some issues in get_cell_local!)
    use tools, only: loc2glo
    implicit none
    ! Calculates cell centers in global coordinates
    class(t_flow_fdgrid)   :: this
    integer                :: i, j, n
    real                   :: xbase, ybase

    ybase = this%dy * this%ny
    do i=1, this%ny
      xbase = 0.0
      do j=1, this%nx
        n = (i-1)*this%nx+j                            ! node number
        this%loc_grid(1)%ptrgrid%coords(1,n) = xbase + 0.5 * this%dx  ! x local
        this%loc_grid(1)%ptrgrid%coords(2,n) = ybase - 0.5 * this%dy  ! y loca
        ! get global
        call loc2glo(                           &
          this%loc_grid(1)%ptrgrid%coords(1,n), &
          this%loc_grid(1)%ptrgrid%coords(2,n), &
          this%xoff,                            &
          this%yoff,                            &
          this%rot,                             &
          this%grid(1)%ptrgrid%coords(1,n),             &
          this%grid(1)%ptrgrid%coords(2,n))
        xbase = xbase + this%dx
      end do
      ybase = ybase - this%dy
    end do

  end subroutine calc_fdcell_centers
!-------------------------------------------------------------------------------------------------!
  
  subroutine read_fdgrid(this, opt)
    use m_file_io, only: log_unit
    implicit none
    class(t_flow_fdgrid)         :: this
    class(t_options),intent(in)  :: opt
    integer                      :: ierr, i, j, k, n, offset
    integer,allocatable          :: lay2grid(:), ibound(:)
    real,allocatable             :: top(:), bot(:), thick(:)
    character(256)               :: topfile, botfile, iboundfile
    logical                      :: uniform_ibound
    type(t_file_reader), pointer :: reader2

    ! Standard file reader variables
    integer                    :: status, length, eof
    character(200)             :: id, value
    type(t_vstringlist)        :: strings
    
    topfile = ''
    botfile = ''
    iboundfile = ''
    uniform_ibound = .false.
    
    reader => open_file_reader(this%grd_file)
    
    ! Pretend we're in a block...
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("DX")
          this%dx = item2dp(strings, 2)
        case("DY")
          this%dy = item2dp(strings, 2)
        case("DZ")
          this%dz = item2dp(strings, 2)
        case("NX")
          this%nx = item2int(strings, 2)
        case("NY")
          this%ny = item2int(strings, 2)
        case("NZ")
          this%nlayers = item2int(strings, 2)
        case("TOP")
          if (this%nx==0.or.this%ny==0) then
            call error_handler(1,reader%file,"Must set NX & NY before TOP/BOT arrays")
          else ! allocate
            allocate(top(this%nx*this%ny))
            top = 0.0
          end if
          if (length > 2) then
            if (vstring_equals ( vstrlist_index(strings, 2), "constant", nocase=.true.)) then
              top = item2dp(strings, 3)
            else if (vstring_equals ( vstrlist_index(strings, 2), "external", nocase=.true.)) then
              call item2char(strings, 3, topfile)
            end if
          else ! Read in array
            read(reader%unit, *) top
          end if
        case("BOT")
          if (this%nx==0.or.this%ny==0) then
            call error_handler(1,reader%file,"Must set NX & NY before TOP/BOT arrays")
          else ! allocate
            allocate(bot(this%nx*this%ny))
          end if
          if (length > 2) then
            if (vstring_equals ( vstrlist_index(strings, 2), "constant", nocase=.true.)) then
              bot = item2dp(strings, 3)
            else if (vstring_equals ( vstrlist_index(strings, 2), "external", nocase=.true.)) then
              call item2char(strings, 3, botfile)
            end if
          else ! Read in array
            read(reader%unit, *) bot
          end if
        case("IBOUND","ACTIVE")
          if (this%nx==0.or.this%ny==0.or.this%nlayers==0) then
            call error_handler(1,reader%file,"Must set NX, NY, NZ before IBOUND array")
          end if
          if (length > 2) then
            if (vstring_equals ( vstrlist_index(strings, 2), "constant", nocase=.true.)) then
              allocate(ibound(this%nx*this%ny))
              uniform_ibound = .true.
              ibound = item2dp(strings, 3)
            else if (vstring_equals ( vstrlist_index(strings, 2), "external", nocase=.true.)) then
              call item2char(strings, 3, iboundfile)
            end if
          else ! Read in array
            if (uniform_ibound) then
              allocate(ibound(this%nx*this%ny))
            else
              allocate(ibound(this%nx*this%ny*this%nlayers))
            end if
            read(reader%unit, *) ibound
          end if
        case("USE_UNIFORM_IBOUND","USE_UNIFORM_ACTIVE")
          uniform_ibound = .true.
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Grid option: " // trim(id))
      end select
    end do
    call reader%close_file()
    
    ! Handle other input cases
    if (trim(topfile)/='') then             ! top as external file
      reader => open_file_reader(trim(topfile))
      read(reader%unit, *) top
    else if (.not.allocated(top)) then      ! no top entered, default 0
      allocate(top(this%nx*this%ny))
      top = 0.0      
    end if
    
    if (trim(botfile)/='') then             ! bot as external file
      reader => open_file_reader(trim(botfile))
      read(reader%unit, *) bot
    else if (.not.allocated(bot)) then      ! no bot entered, calc from dz
      if (this%dz==0) call error_handler(1,reader%file,"Must set DZ if not passing BOT array")
      allocate(bot(this%nx*this%ny))
      bot = top - this%dz * this%nlayers
    end if
    
    if (trim(iboundfile)/='') then          ! ibound as external file
      if (uniform_ibound) then
        allocate(ibound(this%nx*this%ny))
      else
        allocate(ibound(this%nx*this%ny*this%nlayers))
      end if
      reader => open_file_reader(trim(iboundfile))
      read(reader%unit, *) ibound
    else if (.not.allocated(ibound)) then   ! no ibound entered, default 1
      allocate(ibound(this%nx*this%ny))
      uniform_ibound = .true.
      ibound = 1
    end if

    ! Setup Layers
    allocate(lay2grid(this%nlayers))
    lay2grid = 1
    this%ngrids = 1
    call this%alloc_layers(lay2grid)

    ! Setup Nodes
    this%ncol = this%nx  ! for cell2rowcol
    this%nnodes = (/ (this%nx * this%ny, k=1, this%nlayers) /)
    call this%alloc_nodes()
    
    ! Handle inactive cells
    offset = 0
    if (.not.opt%PREDICT_INACTIVE) then
      this%active_nodes_flat = .false.
      do k = 1, this%nlayers
        if (k>1) offset = sum(this%nnodes(1:k-1))
        do i = 1, this%nnodes(k)
          if (uniform_ibound) then
            this%active_nodes_bylay(i,k) = ibound(i) /= 0
            this%active_nodes_flat(i) = ibound(i) /= 0
          else
            this%active_nodes_bylay(i,k) = ibound(offset+i) /= 0
            if (.not.this%active_nodes_flat(i).and.ibound(offset+i)==1) this%active_nodes_flat(i) = .true.
          end if
        end do
      end do
    end if

    ! Move elevation arrays
    allocate(thick(this%nx*this%ny))
    if (allocated(top).and.allocated(bot)) then
      thick = (top - bot) / this%nlayers
    else if (this%dz>0) then
      thick = this%dz
    else
      ! Error caught above
    end if
    this%elev(:,0) = top
    do k=1, this%nlayers
      this%elev(1:this%nnodes(k),k) = this%elev(1:this%nnodes(k),k-1) - thick
    end do

    ! Get cell/node centers
    call this%calc_fdcell_centers()

    ! Clean up
    if (allocated(top)) deallocate(top)
    if (allocated(bot)) deallocate(bot)
    if (allocated(thick)) deallocate(thick)

  end subroutine read_fdgrid

!-------------------------------------------------------------------------------------------------!
  
 subroutine write_fdgrid_out(this, par)
    use m_datasets, only: t_layerdata
    use m_file_io, only: t_file_reader, t_file_writer, open_file_reader, open_file_writer
    use m_write_fmodel
    implicit none
    class(t_flow_fdgrid)         :: this
    type(t_layerdata),pointer    :: par(:)
    type(t_file_writer),pointer  :: fwrite
    integer                      :: ierr
    character(500)               :: fname
    
    ! Nothing, for now

  end subroutine write_fdgrid_out
  
!-------------------------------------------------------------------------------------------------!
! MODFLOW 2000 CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine init_modflow2000(this)
    implicit none
    class(t_modflow_model)       :: this

    call initialize(this)
    this%model_type  = 3
    this%structured  = .true.
    this%name = "MODFLOW"

  end subroutine init_modflow2000

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_layers_mf(this, lay2grid)
    implicit none
    class(t_modflow_model)       :: this
    integer                      :: lay2grid(this%nlayers)

    ! Call super
    call alloc_layers(this, lay2grid)

    allocate(this%loc_grid(1))  ! Would be this%ngrids if layers did not have same xy grid

  end subroutine alloc_layers_mf

!-------------------------------------------------------------------------------------------------!

  subroutine alloc_nodes_mf(this)
    implicit none
    class(t_modflow_model)       :: this

    ! Create local grid object (see above layer caveat in alloc_layers_mf)
    this%loc_grid(1)%ptrgrid => create_grid_object(2, this%nnodes(1))

    ! Call super
    call alloc_nodes(this)

  end subroutine alloc_nodes_mf

!-------------------------------------------------------------------------------------------------!
  subroutine calc_mfcell_centers(this)
    ! Both local and global (formerly was just global, causing some issues in get_cell_local!)
    use tools, only: loc2glo
    use GLOBAL, ONLY : NROW, NCOL, DELR, DELC
    implicit none
    ! Calculates cell centers in global coordinates
    class(t_modflow_model)       :: this
    integer                :: i, j, n
    real                   :: xbase, ybase

    ybase = sum(delc)
    do i=1, nrow
      xbase = 0.0
      do j=1, ncol
        n = (i-1)*ncol+j                            ! node number
        this%loc_grid(1)%ptrgrid%coords(1,n) = xbase + 0.5 * delr(j)  ! x local
        this%loc_grid(1)%ptrgrid%coords(2,n) = ybase - 0.5 * delc(i)  ! y loca
        ! get global
        call loc2glo(                           &
          this%loc_grid(1)%ptrgrid%coords(1,n), &
          this%loc_grid(1)%ptrgrid%coords(2,n), &
          this%xoff,                            &
          this%yoff,                            &
          this%rot,                             &
          this%grid(1)%ptrgrid%coords(1,n),             &
          this%grid(1)%ptrgrid%coords(2,n))
        xbase = xbase + delr(j)
      end do
      ybase = ybase - delc(i)
    end do

  end subroutine calc_mfcell_centers
!-------------------------------------------------------------------------------------------------!

  function find_in_grid_cell(this, coords, max_outside_dist) result(cell)
    use tools, only: glo2loc
    ! Assumes coords is in GLOBAL coordinates
    ! Assumes uniform xy layers
    ! Returns 0 if not found
    ! in T2P 1.0, it gave an error within here if the cell was not found. However, seems best to
    ! let the calling routine handle the error - it has more context.
    implicit none
    class(t_modflow_model)  :: this
    real                    :: coords(:)
    real, intent(in)        :: max_outside_dist
    integer                 :: cellids(4)
    real                    :: celldist(4),xmin,xmax,ymin,ymax,xl,yl,min_dist,dx,dy,dist
    integer                 :: cell, i, maxcell

    cell = 0
    maxcell = min(4, this%grid(this%lay2grid(1))%ptrgrid%n)
    call this%grid(this%lay2grid(1))%ptrgrid%get_nnear(coords, maxcell, cellids, celldist, mask=this%active_nodes_flat)  ! do we need more than 4 for USG?

    if (this%structured) then
      ! Check each cell center to see if well belongs
      do i=1, maxcell
        call this%get_cell_local(cellids(i), xmin, xmax, ymin, ymax)
        call glo2loc(coords(1), coords(2), this%xoff, this%yoff, this%rot, xl, yl)
        if ((xl >= xmin).and.(xl < xmax).and.(yl >= ymin).and.(yl < ymax)) then
          cell = cellids(i)
          return       ! Early Return !
        end if
      end do
    else  ! USG
      !cell = find_in_grid_gsf(coord, max_outside_dist)
    end if

    ! If we didn't find it in a cell, was it at least within max_outside_dist of a cell edge?
    min_dist = HUGE(1.0)
    do i=1, maxcell
      call this%get_cell_local(cellids(i), xmin, xmax, ymin, ymax)
      dx = max(0.0, max(xmin - xl, xl - xmax))  ! Distance to x edges
      dy = max(0.0, max(ymin - yl, yl - ymax))  ! Distance to y edges
      dist = sqrt(dx**2 + dy**2)
      if (dist < max_outside_dist .and. dist < min_dist) then
        min_dist = dist
        cell = cellids(i)
      end if
    end do

  end function find_in_grid_cell

  !-------------------------------------------------------------------------------------------------!

  subroutine cell2rowcol(this, cell, row, col)
    implicit none
    class(t_flow_model)    :: this
    integer,intent(in)     :: cell
    integer,intent(out)    :: row, col

    row = int((cell-1)/this%ncol) + 1
    col = cell - (row-1) * this%ncol

  end subroutine cell2rowcol

!-------------------------------------------------------------------------------------------------!

  subroutine get_cell_local(this, cell, xmin, xmax, ymin, ymax)
    ! Assumes uniform xy layers
    use GLOBAL, ONLY : DELR, DELC
    implicit none
    class(t_modflow_model)  :: this
    integer,intent(in)     :: cell
    real    ,intent(out)       :: xmin,xmax,ymin,ymax
    integer                :: row, col

    if (this%structured) then
      call this%cell2rowcol(cell, row, col)
      xmin = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(1,cell) - 0.5 * delr(col)
      xmax = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(1,cell) + 0.5 * delr(col)
      ymin = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(2,cell) - 0.5 * delc(row)
      ymax = this%loc_grid(this%lay2grid(1))%ptrgrid%coords(2,cell) + 0.5 * delc(row)
    else
      ! TODO
    end if

  end subroutine get_cell_local

!-------------------------------------------------------------------------------------------------!

  subroutine read_modflow2000(this, opt)
    use m_file_io, only: log_unit
    ! MODFLOW globals to import
    use GLOBAL, ONLY : IUNSTR,NLAY,NROW,NCOL,DELR,DELC,TOP,BOT,LAYCBD,IUNIT,NODLAY,IBOUND
    use PARAMMODULE, ONLY: NMLTAR, NZONAR
    use GWFBCFMODULE, ONLY: VKA, SC1, SC2
    implicit none
    class(t_modflow_model)       :: this
    class(t_options),intent(in)  :: opt
    integer                      :: ierr, ngrids, i, j, k, n, mfmaxunit
    integer,allocatable          :: grid_count(:), lay2grid(:)
    character(256)               :: iomsg
    logical                      :: found

    ! MODFLOW-USG does the heavy lifting
    call init_modflow_usg(this%sim_file,ierr,iomsg,mfmaxunit)
    this%ncol = NCOL
    ! Error handling for external reader...
    if (ierr /= 0) then
      ! Todo replace with actual log writing function
      write(*,'(a)') trim(iomsg)
      write(log_unit,'(3a)') 'IO Error - ', trim(iomsg)
      error stop
    end if

    ! Handle MODFLOW settings
    ! UPW or LPF (or none)
    this%layfile = 0
    if (IUNIT(23) > 0) then
      this%layfile = 23
      this%gwftyp = 'LPF'
    end if
    if (IUNIT(45) > 0) then
      this%layfile = 45
      this%gwftyp = 'UPW'
    end if
    if (this%layfile==0) call error_handler(2,'MODFLOW NAM file must have LPF or UPW package present',this%sim_file)

    allocate(lay2grid(nlay))
    lay2grid = 1

    ! Check out if/how unstructured the model is
    ngrids = 1
    if (IUNSTR/=0) then
      this%structured = .false.
      ! Equal nodes each layer?
      allocate(grid_count(nlay))
      grid_count(1) = nodlay(1)
      do i=2, nlay
        found = .false.
        n = nodlay(i)-nodlay(i-1)
        do j=1, ngrids
          if (n == grid_count(j)) then
            found = .true.
            lay2grid(i) = j
            exit
          end if
        end do
        if (.not. found) then
          ngrids = ngrids + 1
          grid_count(ngrids) = n
          lay2grid(i) = ngrids
        end if
      end do
    end if

    ! Set Layers
    this%nlayers = nlay
    this%ngrids  = ngrids
    call this%alloc_layers(lay2grid)

    ! Count nodes
    this%nnodes = (/ (nodlay(k) - nodlay(k-1), k=1, nlay) /)
    call this%alloc_nodes()
    
    ! Handle inactive cells
    if (.not.opt%PREDICT_INACTIVE) then
      do k = 1, nlay
        do i = 1, this%nnodes(k)
          this%active_nodes_bylay(i,k) = ibound(nodlay(k-1) + i) /= 0
        end do
      end do
      ! Flatten
      do i = 1, this%nnodes(1)
        this%active_nodes_flat(i) = any(this%active_nodes_bylay(i,1:nlay))
      end do
    end if

    ! Move elevation arrays
    this%elev(:,0) = top(1:this%nnodes(1))
    do k=1, nlay
      this%elev(1:this%nnodes(k),k) = bot(nodlay(k-1)+1: nodlay(k))
    end do

    ! Get cell/node centers
    if (IUNSTR/=0) then
      ! TODO
    else
      call this%calc_mfcell_centers()
    end if

    ! Clean up
    if (allocated(grid_count)) deallocate(grid_count)

  end subroutine read_modflow2000

!-------------------------------------------------------------------------------------------------!

  subroutine write_node_file_mf(this, fname, par, par_id)
    ! Base class method, should not be run
    use m_datasets, only: t_layerdata, write_layerdata
    implicit none
    class(t_modflow_model)     :: this
    character(*)               :: fname
    type(t_layerdata),pointer  :: par(:)
    integer,intent(in)         :: par_id
    integer                    :: i, k, row, col, unit
    character(60)              :: fmt(2)
    type(t_file_writer),pointer:: fwrite

    call write_layerdata(fname, par, this%grid(1)%ptrgrid%coords, 1, 1, par_id, gridncol=this%ncol)

  end subroutine write_node_file_mf
!-------------------------------------------------------------------------------------------------!
  subroutine write_fmodel_input_mf(this, par)
    use GLOBAL, ONLY:IUNIT
    use m_datasets, only: t_layerdata
    use m_file_io, only: t_file_reader, t_file_writer, open_file_reader, open_file_writer
    use m_write_fmodel
    implicit none
    class(t_modflow_model)     :: this
    type(t_layerdata),pointer  :: par(:)
  type(t_file_reader),pointer  :: fread
  type(t_file_writer),pointer  :: fwrite
    integer                    :: ierr, upwflag
    character(500)             :: fname
    logical                    :: lop

    !write(*,'(2x,2(a,3a))') 'Reading temp ', this%gwftyp, ', writing new ', this%gwftyp

    ! LPF/UPW [should be] open - close it. We're going to do a bait-and-switch on MODFLOW
    inquire(unit=IUNIT(this%layfile), opened=lop, name=fname)
    if (lop) close(IUNIT(this%layfile))

    ! Initialize MODFLOW LPF values with template LPF file (user provided)
    fread => open_file_reader(this%template_file)
    upwflag = 0
    if (this%gwftyp=='UPW') upwflag = 1
    CALL GWF2BCFU1AR(IUNIT(1),IUNIT(22),fread%unit,IUNIT(64), upwflag)
    call fread%close_file()

    ! Re-open actual LPF file and OVERWRITE
    fwrite => open_file_writer(fname)

    call writeGWFP(fwrite%unit, par, this%gwftyp)
    call fwrite%close_file()

  end subroutine write_fmodel_input_mf
!-------------------------------------------------------------------------------------------------!

end module m_flow_model