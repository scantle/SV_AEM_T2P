module list_module
  implicit none
!-------------------------------------------------------------------------------------------------!
! Adapted/Inspired by https://www.physicstom.com/fortranlinkedlist/
!-------------------------------------------------------------------------------------------------!
  
!-------------------------------------------------------------------------------------------------!
! Module Types
!-------------------------------------------------------------------------------------------------!
  type,public :: list_node
    integer                      :: value
    type(list_node), pointer     :: next  => null()
    type(list_node), pointer     :: prev  => null()
  end type list_node
!-------------------------------------------------------------------------------------------------!
  type :: stack
    integer                      :: size = 0
    type(list_node), pointer     :: head => null()
    type(list_node), pointer     :: tail => null()
  contains
    procedure :: push
    procedure :: remove_tail
    procedure :: pop
  end type stack
!-------------------------------------------------------------------------------------------------!  
  contains
!-------------------------------------------------------------------------------------------------!
! Stack Procedures
!-------------------------------------------------------------------------------------------------!
  subroutine push(this, value)
    implicit none
    class(stack)                 :: this
    integer, intent(in)          :: value
    type(list_node), pointer     :: curr_ptr
    type(list_node), pointer     :: node_ptr
    
    ! Create node, set value
    allocate(node_ptr)
    node_ptr%value =  value
    node_ptr%next  => null()
    
    ! Increment size
    this%size = this%size + 1
    
    ! If this is the first item, it is also the head and tail
    if(.not. associated(this%head)) then
      this%head => node_ptr
      this%tail => node_ptr
      node_ptr%prev  => null()
    else
      ! Update previous tail node
      this%tail%next => node_ptr
      ! Previous tail is prev to this node
      node_ptr%prev => this%tail
      ! Set tail to newest node
      this%tail => node_ptr
    end if
  end subroutine push
!-------------------------------------------------------------------------------------------------!
  subroutine remove_tail(this)
    implicit none
    class(stack)              :: this
    type(list_node), pointer  :: node_ptr
    
    ! Deincrement size
    this%size = this%size - 1
    ! Get pointer to new tail
    node_ptr => this%tail%prev
    ! Remove old tail
    deallocate(this%tail)
    ! If nodes remain, reassign tail
    if (this%size > 0) then
      this%tail => node_ptr
      this%tail%next => null()
    else
      this%tail => null()
      this%head => null()  ! Otherwise will point to deallocated node
    end if
    
  end subroutine remove_tail
!-------------------------------------------------------------------------------------------------!
  function pop(this) result(lastnode)
    implicit none
    class(stack)              :: this
    type(list_node), pointer  :: lastnode
    
    if (this%size > 0) then
      ! Set the output node
      allocate(lastnode)
      lastnode = this%tail
      call this%remove_tail()
    else
      ! Return a null if list is empty
      lastnode => null()
    end if
  end function pop
!-------------------------------------------------------------------------------------------------!
end module list_module