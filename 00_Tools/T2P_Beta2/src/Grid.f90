module m_grid
  use kdtree2_module
  implicit none

!-------------------------------------------------------------------------------------------------!
  type t_Grid

    integer                   :: n
    integer                   :: dim
    real    , allocatable     :: coords(:,:)         ! xy[z] by n points
    real    , allocatable     :: masked_coords(:,:)  ! xy[z] by n points, subject to a mask (for kdtree)
    type(kdtree2),pointer     :: tree => null()      ! Tree for locating model nodes
    integer, allocatable      :: true_idx(:)         ! Mapping of indices from masked to full dataset

    contains
      procedure,private        :: initialize
      procedure,private        :: finalize
      procedure,public         :: build_tree
      procedure,public         :: get_nnear
      procedure,public         :: get_r2near
      procedure,public         :: burn_tree
  end type t_Grid
!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  function create_grid_object(dim, n) result(object)
    implicit none

    integer,intent(in)            :: dim, n
    class(t_Grid),pointer         :: object

    allocate(object)
    call object%initialize(dim, n)

  end function

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! GRID CLASS FILE TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize(this, dim, n)
    implicit none

    class(t_Grid)        :: this
    integer, intent(in)  :: dim, n

    this%n   = n
    this%dim = dim

    allocate(this%coords(dim,n))

    this%coords = 0.0

  end subroutine initialize

!-------------------------------------------------------------------------------------------------!

  subroutine build_tree(this, mask, rotmat)
    use m_error_handler, only: error_handler
    use tools, only: indices_from_mask
    implicit none
    class(t_Grid), intent(inout) :: this
    logical,intent(in),optional  :: mask(this%n)
    real    ,optional            :: rotmat(:,:) ! Rotation Matrix
    integer                      :: j, n_points, success

    allocate(this%tree)

    if (present(mask)) then
      n_points = count(mask)
      allocate(this%true_idx(n_points))
      allocate(this%masked_coords(this%dim, n_points))
      this%true_idx = indices_from_mask(mask)
      !this%masked_coords = this%coords(1:,this%true_idx)  ! Copy subset to prevent pointer to temporary array
      do j = 1, n_points
        this%masked_coords(:, j) = this%coords(:, this%true_idx(j))
      end do
      this%tree => kdtree2_create(this%masked_coords, success, dim=this%dim, rotmat=rotmat)
    else
      this%tree => kdtree2_create(this%coords, success, dim=this%dim, rotmat=rotmat)
      allocate(this%true_idx(this%n))
      this%true_idx = [(j, j = 1, this%n)]
    end if

    if (success==0) call error_handler(9, opt_msg='Could not allocate kdtree - usually caused by not enough data')

  end subroutine build_tree

!-------------------------------------------------------------------------------------------------!

  subroutine get_nnear(this, qpoint, nnear, idx, dis, mask, rotmat)
    implicit none
    ! Uses tree to return n nearest grid indeces and distances
    class(t_Grid), intent(inout) :: this
    real    ,intent(in)          :: qpoint(this%dim)
    integer, intent(in)          :: nnear
    integer, intent(out)         :: idx(nnear)
    real    ,intent(out)         :: dis(nnear)
    type(kdtree2_result)         :: res(nnear)
    logical,intent(in),optional  :: mask(this%n)
    real(kdkind), target, optional  :: rotmat(:,:) ! Rotation Matrix
    integer :: i
    
    if (nnear<this%n) then
      if (.not. associated(this%tree)) call this%build_tree(mask=mask)
      call kdtree2_n_nearest(this%tree, qpoint, nnear, res, rotmat)

      ! Worth getting fancier than copying? Pointers?
      if (allocated(this%true_idx)) then
        ! Map indices back to original dataset
        idx = this%true_idx(res(1:)%idx)
      else
        idx = res(:)%idx
      end if
      dis = res(:)%dis
    else
      idx(1:this%n) = [(i, i=1,this%n)]
      dis(1:this%n) = [(sum((qpoint(1:this%dim)-this%coords(1:this%dim, i))**2), i=1,this%n)]
    end if
  end subroutine get_nnear

!-------------------------------------------------------------------------------------------------!

  subroutine get_r2near(this, qpoint, r2, idx, dis, mask)
    implicit none
    class(t_Grid), intent(inout)     :: this
    real    ,intent(in)              :: qpoint(this%dim)
    real    ,intent(in)              :: r2
    integer, pointer                 :: idx(:)
    real    ,pointer                 :: dis(:)
    integer                          :: nfound
    type(kdtree2_result),allocatable :: res(:)
    logical,intent(in),optional      :: mask(this%n)

    if (.not. associated(this%tree)) call this%build_tree(mask=mask)
    nfound = kdtree2_r_count(this%tree,qpoint,r2)
    if (associated(idx)) deallocate(idx)
    if (associated(dis)) deallocate(dis)
    allocate(idx(nfound), dis(nfound), res(nfound))
    call kdtree2_r_nearest(this%tree, qpoint, r2, nfound, nfound, res)

    if (allocated(this%true_idx)) then
      ! Map indices back to original dataset
      idx = this%true_idx(res(:)%idx)
    else
      idx = res(:)%idx
    end if
    dis = res(:)%dis

  end subroutine get_r2near

!-------------------------------------------------------------------------------------------------!

  subroutine burn_tree(this)
    ! sometimes you have to have a little fun with names
    implicit none
    class(t_Grid), intent(inout)     :: this
    if (associated(this%tree)) deallocate(this%tree)
    if (allocated(this%true_idx)) deallocate(this%true_idx)
    if (allocated(this%masked_coords)) deallocate(this%masked_coords)
  end subroutine burn_tree

!-------------------------------------------------------------------------------------------------!

  subroutine finalize(this)
    implicit none
    class(t_Grid), intent(inout) :: this

    if (allocated(this%coords))        deallocate(this%coords)
    if (allocated(this%masked_coords)) deallocate(this%masked_coords)
    if (allocated(this%true_idx))      deallocate(this%true_idx)
    if (associated(this%tree))         deallocate(this%tree)

    ! Include any other cleanup tasks here
  end subroutine finalize

!-------------------------------------------------------------------------------------------------!
end module m_grid