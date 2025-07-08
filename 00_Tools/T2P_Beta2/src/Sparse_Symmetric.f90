module m_sparse_symmetric
  
  use m_error_handler, only: error_handler
!-------------------------------------------------------------------------------------------------!
type :: t_sparse_sym
  ! For storing smartly, sparsely, a symmetric matrix (internally, upper triangular)
  ! Not dynamically allocated, size must be known in advance
  integer               :: n                        ! Size of the original symmetric matrix
  integer               :: sparse_size              ! Size of the internal subset matrix
  integer               :: max_non_zeros            ! Number of feasible pairs in the subset
  integer               :: num_values = 0           ! Current count of non-zero pairs
  integer               :: next_internal_index = 1  ! Counter for assigning new internal indices
  integer, allocatable  :: idxs(:,:)                ! Internal index mapping to position in values
  integer, allocatable  :: translation(:)           ! Array for translating original to internal indices
  real    , allocatable :: values(:)                ! Array storing values for feasible pairs


  contains
    procedure, private :: translate_and_add
    procedure, public  :: initialize
    procedure, public  :: add_value
    procedure, public  :: get_value
  
end type t_sparse_sym
!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!
! CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize(this, orig_size, sparse_size)
    implicit none
    class(t_sparse_sym), intent(inout) :: this
    integer, intent(in)                :: orig_size, sparse_size

    ! Store
    this%n = orig_size
    this%sparse_size = sparse_size
    this%max_non_zeros = (sparse_size * (sparse_size + 1)) / 2
    
    ! Allocate
    allocate(this%idxs(sparse_size, sparse_size))
    allocate(this%values(this%max_non_zeros))
    allocate(this%translation(orig_size))

    ! Initialize
    this%idxs = 0          ! Zero indicates no mapping yet
    this%values = 0.0   ! Should this be editable??
    this%translation = -1
  end subroutine initialize

!-------------------------------------------------------------------------------------------------!
  
  function translate_and_add(this, orig_idx) result(internal_idx)
    implicit none
    class(t_sparse_sym), intent(inout) :: this
    integer, intent(in)                :: orig_idx
    integer                            :: internal_idx

    internal_idx = this%translation(orig_idx)

    ! Assign a new internal index if it's not already mapped
    if (internal_idx == -1) then
      if (this%next_internal_index > this%sparse_size) then
        print *, "Error: Exceeded subset size in internal index mapping"
        stop 1
      end if
      internal_idx = this%next_internal_index
      this%translation(orig_idx) = internal_idx
      this%next_internal_index = this%next_internal_index + 1
    end if
  end function translate_and_add
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine add_value(this, orig_idx1, orig_idx2, value)
    implicit none
    class(t_sparse_sym), intent(inout) :: this
    integer, intent(in)                :: orig_idx1, orig_idx2
    real    , intent(in)               :: value
    integer                            :: internal_idx1, internal_idx2, row, col, position

    internal_idx1 = translate_and_add(this, orig_idx1)
    internal_idx2 = translate_and_add(this, orig_idx2)

    ! Ensure indices are in ascending order (symmetry)
    if (internal_idx1 > internal_idx2) then
      row = internal_idx2
      col = internal_idx1
    else
      row = internal_idx1
      col = internal_idx2
    end if

    position = this%idxs(row, col)

    if (position == 0) then
      ! New feasible value
      this%num_values = this%num_values + 1
      if (this%num_values > this%max_non_zeros) call error_handler(9,opt_msg="Sparse Symmetric Matrix values exceeded")
      position = this%num_values
      this%idxs(row, col) = position
    end if

    this%values(position) = value
  end subroutine add_value

!-------------------------------------------------------------------------------------------------!
  
  function get_value(this, orig_idx1, orig_idx2) result(v)
    implicit none
    class(t_sparse_sym), intent(in) :: this
    integer, intent(in)             :: orig_idx1, orig_idx2
    real                            :: v
    integer                         :: internal_idx1, internal_idx2, row, col

    internal_idx1 = this%translation(orig_idx1)
    internal_idx2 = this%translation(orig_idx2)

    ! Ensure valid internal indices are found
    if (internal_idx1 == -1 .or. internal_idx2 == -1) call error_handler(9,opt_msg="Sparse Symmetric Matrix invalid index")

    ! Ensure indices are in ascending order (symmetry)
    if (internal_idx1 > internal_idx2) then
      row = internal_idx2
      col = internal_idx1
    else
      row = internal_idx1
      col = internal_idx2
    end if
    
    v = this%values(this%idxs(row, col))
  end function get_value

!-------------------------------------------------------------------------------------------------!
end module m_sparse_symmetric