module m_datasets
  use m_global, only: NODATA
  use m_grid, only: t_grid, create_grid_object
  implicit none
!-------------------------------------------------------------------------------------------------!
  type  :: t_dataset  ! Abstract Base Class

    integer,allocatable            :: category(:)      ! Zones, Hydrostratigraphic Units (HSUs)
    type(t_grid),pointer           :: grid             ! Grid
    ! Doesn't even have any data! Because array dimensions differ, subclasses are controversially
    ! allowed to have their own data storage

    contains
      procedure,nopass             :: initialize_abc
      generic,public               :: initialize => initialize_abc
      procedure,public             :: build_tree_by_category
      procedure,public             :: get_data_by_category
      procedure,public             :: finalize
  end type t_dataset
!-------------------------------------------------------------------------------------------------!
!  type, extends(t_dataset) :: t_intervaldata
!
!    character(30)                  :: name             ! Texture class
!    integer                        :: class_id         ! Class ID
!    integer,allocatable            :: nearest_node(:)  ! Nearest model node (cell) to data point
!    real    ,allocatable           :: values(:,:)      ! Data values associated with intervals at each point (interval, point)
!
!    contains
!
!      procedure,public             :: initialize => init_interval
!
!  end type t_intervaldata
!-------------------------------------------------------------------------------------------------!
  type, extends(t_dataset) :: t_layerdata
    ! Intended for input data
    character(30)                  :: name             ! Class Name  (UNUSED? TODO)
    integer                        :: id               ! Index within t2p%datasets
    real    ,allocatable           :: values(:,:)      ! Data values associated with each grid point for each class (nclasses, npoints)
    integer,allocatable            :: fmodel_loc(:)    ! Containing model element/cell
    real    ,allocatable           :: mean(:)          ! Mean of values, used for cokriging standardization (generally globally, not by layer)

    contains
      procedure                    :: init_layerdata
      generic                      :: initialize => init_layerdata
      procedure,public             :: finalize => finalize_layerdata
  end type t_layerdata
!-------------------------------------------------------------------------------------------------!
!  type, extends(t_dataset) :: t_layermultidata
!    ! Intended for intermediate data
!    character(30)                  :: name             ! Class Name
!    integer                        :: class_id         ! Class ID
!    real    ,allocatable           :: values(:,:)      ! Data values associated with each grid point for each class (nclasses, npoints)
!    integer,allocatable            :: fmodel_loc(:)    ! Containing model element/cell
!
!    contains
!      procedure                    :: init_layermultidata
!      generic                      :: initialize => init_layermultidata
!
!  end type t_layermultidata
!-------------------------------------------------------------------------------------------------!
! See also: t_pilotpoint derived class in PilotPoints.f90
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine write_layerdata(&
    fname,         & ! output filename
    layerdata,     & ! array of layerdata
    coords,        & ! coordinates
    kstart,        & ! starting layer for ourput
    kintrvl,       & ! layer step for ourput (used for IWFM)
    classid,       & ! index of values
    gridncol,      & ! optional; number of columns for structured grid; default is 0 for unstructured grid
    lname,         & ! optional; prefix of layer name
    rownames,      & ! optional; row names written in the first column in the output file
    modelnodes,    & ! optional; index of the node numbers of the output, used when output corresponding cell of the wells
    mdim,          & ! optional; number of dimensions; default is 2
    nodecol        & ! optional; column name of the node number
    
  )
    implicit none
    character(*)               :: fname
    type(t_layerdata),pointer  :: layerdata(:)  ! (nlayers)
    integer,intent(in)         :: kstart, kintrvl, classid
    real   ,intent(in)         :: coords(:,:)
    integer,intent(in),optional:: gridncol, mdim, modelnodes(:)
    character(*),intent(in),optional :: lname, rownames(0:), nodecol
    ! local
    integer                    :: inode, igrid, i, k, row, col, unit, ncol, ndim, nlayers
    integer, allocatable       :: inodes(:)
    integer                    :: nnodes
    character(:),allocatable   :: header, fmt, nodename
    character(20)              :: tmpstr

    nlayers = size(layerdata)
    nodename = "Node"
    if (present(nodecol)) nodename = trim(nodecol)
    if (present(modelnodes)) then
      inodes = modelnodes
    else
      nnodes = size(layerdata(1)%values, dim=2)
      allocate(inodes(nnodes))
      !inodes = [(inode, inode=1,size(layerdata(1)%values, dim=2))]
      do i = 1, nnodes
        inodes(i) = i
      end do
    end if
    ndim = 2
    if (present(mdim)) ndim = mdim
    nnodes = size(inodes)
    ncol = 0
    if (present(gridncol)) then
      ncol = gridncol
    end if
    if (ncol>0) then
      header="Row,Column,X,Y"
      fmt="I0,',',I0,2(',',G0.10)"
    else
      header=nodename//",X,Y"
      fmt="I0,2(',',G0.10)"
    end if
    if (present(rownames)) then
      header = trim(rownames(0))//","//header
      fmt = "A,',',"//fmt
    end if
    if (ndim==3) then
      header = header//",Z"
      fmt = fmt//",',',G0.6"
    end if

    do k=1, int(nlayers/kintrvl)
      if (present(lname)) then
        write(tmpstr, "(2A,I0)") ",", trim(lname), k
      else
        write(tmpstr, "(2A,I0)") ",","Layer", k
      end if
      header = header//trim(tmpstr)
    end do
    fmt = "("//fmt//",*(:',',G0.6))"

    open(newunit=unit, file=trim(fname), status='replace')

    ! Header
    write(unit,"(A)") header

    ! Values
    do inode = 1, nnodes
      igrid = inodes(inode)
      if (ncol>0) then
        row = int((igrid-1)/ncol)+1
        col = igrid-(row-1)*ncol
        if (present(rownames)) then
          write(unit,fmt) trim(rownames(inode)), row, col, coords(1:ndim,inode), (layerdata(k)%values(classid,inode), k=kstart,nlayers,kintrvl)
        else
          write(unit,fmt) row, col, coords(1:ndim,inode), (layerdata(k)%values(classid,inode), k=kstart,nlayers,kintrvl)
        end if
      else
        if (present(rownames)) then
          write(unit,fmt) trim(rownames(inode)), igrid, coords(1:ndim,inode), (layerdata(k)%values(classid,inode), k=kstart,nlayers,kintrvl)
        else
          write(unit,fmt) igrid, coords(1:ndim,inode), (layerdata(k)%values(classid,inode), k=kstart,nlayers,kintrvl)
        end if
      end if
    end do
    close(unit)
  end subroutine write_layerdata

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! BASE CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize_abc(this)
    implicit none
    class(t_dataset),intent(inout)  :: this

  end subroutine initialize_abc

!-------------------------------------------------------------------------------------------------!

  subroutine build_tree_by_category(this, cat, class_idx, rotmat, has_data)
    implicit none
    class(t_dataset),intent(inout)  :: this
    integer, intent(in)             :: cat, class_idx
    !logical,allocatable             :: catmask(:), na_mask(:)
    logical, allocatable            :: combined_mask(:)
    real    ,optional               :: rotmat(:,:) ! Rotation Matrix
    integer                         :: i
    logical,intent(out)             :: has_data

    has_data = .true.
    
    call this%grid%burn_tree()  ! Clear out previous tree
    !allocate(catmask(this%grid%n), na_mask(this%grid%n))
    allocate(combined_mask(this%grid%n))
    
    ! Compute combined mask in a single loop - in category & not NA
    ! Should consider parallelization
    select type(this)
      class is (t_layerdata)
        do i = 1, this%grid%n
          combined_mask(i) = (this%category(i) == cat) .and. (this%values(class_idx, i) /= NODATA)
        end do
      class default
        combined_mask = (this%category == cat)
    end select
    
    if (count(combined_mask) > 0) then  ! Should min_points be a setting??
      call this%grid%build_tree(combined_mask, rotmat=rotmat)
    else
      has_data = .false.
    end if
    deallocate(combined_mask)

  end subroutine build_tree_by_category

!-------------------------------------------------------------------------------------------------!

  subroutine get_data_by_category(this, cat, class_idx, cat_idx, nidx, maxsize)
    ! To get (1) how many items are in that category (nidx) and (2) what their indices are (cat_idx)
    implicit none
    class(t_dataset),intent(inout)  :: this
    integer, intent(in)             :: cat, class_idx, maxsize
    integer, intent(inout)          :: nidx
    integer, intent(inout)          :: cat_idx(maxsize)
    integer                         :: i

    ! For dataset, ignore all NA values
    cat_idx = 0
    nidx = 1
    select type(this)
      class is (t_layerdata)
        do i=1, this%grid%n
          if ((this%values(class_idx,i) /= NODATA).and.(this%category(i)==cat)) then
            cat_idx(nidx) = i
            nidx = nidx + 1
          end if
        end do
    end select
    ! Adjust for starting count at 1
    nidx = nidx - 1

  end subroutine get_data_by_category

!-------------------------------------------------------------------------------------------------!

  subroutine finalize(this)
    implicit none
    class(t_dataset),intent(inout) :: this

    if (allocated(this%category)) deallocate(this%category)
    if (associated(this%grid)) nullify(this%grid)

  end subroutine finalize

!-------------------------------------------------------------------------------------------------!
! LAYER CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine init_layerdata(this, npoints, nclasses, default_value)
    implicit none
    class(t_layerdata),intent(inout)  :: this
    integer,intent(in)                :: npoints, nclasses
    real    ,optional                 :: default_value

    allocate(this%category  (npoints), &
             this%fmodel_loc(npoints), &
             this%values    (nclasses, npoints), &
             this%mean      (nclasses))
    allocate(this%grid)

    ! By default
    this%category = 1

    if (present(default_value)) this%values = default_value

    !this%grid = create_grid_object(2, npoints)  ! X,Y,Z coords by n

  end subroutine init_layerdata

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! LAYER MULTI-DATA CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!
!
!  subroutine init_layermultidata(this, npoints, nclasses, grid)
!    implicit none
!    class(t_layermultidata),intent(inout)  :: this
!    integer,intent(in)                     :: npoints, nclasses
!    class(t_grid),pointer                  :: grid
!
!    allocate(this%category  (npoints), &
!             this%values    (nclasses, npoints))
!    !allocate(this%grid)  ! Already should exist, avoid copying
!    this%grid => grid
!
!    !this%grid = create_grid_object(2, npoints)  ! X,Y,Z coords by n
!
!  end subroutine init_layermultidata
!
!-------------------------------------------------------------------------------------------------!

  subroutine finalize_layerdata(this)
    implicit none
    class(t_layerdata),intent(inout) :: this

    if (allocated(this%category  )) deallocate(this%category  )
    if (allocated(this%values    )) deallocate(this%values    )
    if (allocated(this%fmodel_loc)) deallocate(this%fmodel_loc)
    if (allocated(this%mean      )) deallocate(this%mean      )
    if (associated(this%grid     )) nullify(this%grid)

  end subroutine finalize_layerdata

!-------------------------------------------------------------------------------------------------!

end module m_datasets