module tools
  
  contains
!-----------------------------------------------------------------------------!
  subroutine loc2glo(locx, locy, xoff, yoff, rot, glox, gloy)
    implicit none

    real    ,intent(in)        :: locx, locy, xoff, yoff, rot
    real    ,intent(out)       :: glox, gloy
    real    , parameter        :: DEG2RAD = 3.141592654/180.0

    glox = xoff + locx * cos(rot*DEG2RAD) - locy * sin(rot*DEG2RAD)
    gloy = yoff + locy * cos(rot*DEG2RAD) + locx * sin(rot*DEG2RAD)

  end subroutine loc2glo
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
  subroutine glo2loc(glox, gloy, xoff, yoff, rot, locx, locy)
    implicit none

    real    ,intent(in)        :: glox, gloy, xoff, yoff, rot
    real    ,intent(out)       :: locx, locy
    real    , parameter        :: DEG2RAD = 3.141592654/180.0

    locx = (glox-xoff) * cos(rot*DEG2RAD) + (gloy-yoff) * sin(rot*DEG2RAD)
    locy = (gloy-yoff) * cos(rot*DEG2RAD) - (glox-xoff) * sin(rot*DEG2RAD)

  end subroutine glo2loc
!-----------------------------------------------------------------------------!
  
!-----------------------------------------------------------------------------!
subroutine expand_int_array(array, new_len, init)
    implicit none
    integer, pointer, intent(inout) :: array(:)
    integer, intent(in)             :: new_len
    integer, intent(in), optional   :: init
    integer, pointer                :: temp(:)

    allocate(temp(new_len))
    if (present(init)) temp = init
    temp(1:size(array)) = array
    deallocate(array)
    nullify(array)
    array => temp
end subroutine expand_int_array
  
!-----------------------------------------------------------------------------!
  
!-----------------------------------------------------------------------------!
  subroutine random_seed_initialize (key)
    !*****************************************************************************
    !
    !! random_seed_initialize() initializes the FORTRAN90 random number generator.
    !
    !  Discussion:
    !
    !    This is the stupidest, most awkward procedure I have seen!
    !
    !  Modified:
    !
    !    27 October 2021
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Input:
    !
    !    integer KEY: an initial seed for the random number generator.
    !
    implicit none

    integer key
    integer, allocatable :: seed(:)
    integer seed_size

    if (key<=0) key = huge(seed_size) / 17
    call random_seed ( size = seed_size )
    allocate ( seed(seed_size) )
    seed(1:seed_size) = key
    call random_seed ( put = seed )
    deallocate ( seed )

    return
  end subroutine random_seed_initialize
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
function pinpol(px, py, polyx, polyy, npoly) result(inpoly)
  implicit none
  !---------------------------------------------------------------------------!
  !                                 PINPOL
  !                               *********
  !                 Checks if a point is inside a polygon.
  !      "An improved version of the algorithm of Nordbeck and Rydstedt"
  !
  ! Arguments:
  !   -- px is the x-coordinate of the point to be tested
  !   -- py is the y-coordinate of the point to be tested
  !   -- polyx is a vector of nodal x-coordinates in counter-clockwise order
  !   -- polyy is a vector of nodal y-coordinates in counter-clockwise order
  !   -- npoly is the number of polygon verticies
  !
  ! Requires:
  !   -- Function tridet to calculate triangle determinant
  !
  ! Returns:
  !   -- `inpoly` is the square distance from the point to the nearest point 
  !      on the polygon. The sign of `inpoly` indicates the point's position 
  !      relative to the polygon:
  !      - Exact Zero (0.0): The point lies exactly on the boundary of the 
  !        polygon.
  !      - Positive Value: The point is inside the polygon.
  !      - Negative Value: The point is outside the polygon.
  !
  ! Adapted from Sloan, S. W., 1985 A point-in-polygon program, Adv. Eng.
  !                  Software, Vol 7., No. 1
  !
  !               Implemented in f90 by Leland Scantlebury
  !
  ! Copyright 2018-2025 Leland Scantlebury
  !---------------------------------------------------------------------------!

  integer, intent(in)      :: npoly
  integer                  :: i, next, prev, j
  real    , intent(in)     :: px, py, polyx(npoly), polyy(npoly)
  real                     :: x1, y1, x21, y21, x1p, y1p, t, d, dx, dy, area
  real    , parameter      :: smalld = 1e-6
  logical                  :: snear
  real                     :: inpoly

  inpoly = HUGE(1.0)

  ! Loop over each side defining the polygon
  do i=1, npoly
    ! Side coordinates, length, distance from x,y
    next = i + 1
    if (i == npoly) next = 1
    x1 = polyx(i)
    y1 = polyy(i)
    x21 = polyx(next)-x1
    y21 = polyy(next)-y1
    x1p = x1-px
    y1p = y1-py

    ! Find where normal of px,py intersects infinite line
    t = -(x1p*x21 + y1p*y21)/(x21**2 + y21**2)
    if (t < 0.0d0) then
      ! Normal does not intersect side, point is closer to (x1, y1)
      ! Compute square distance to vertex
      d = x1p**2 + y1p**2
      if (d < inpoly) then
        ! Smallest distance yet
        snear = .false.
        inpoly = d
        j = i
      end if
    else if (t < 1.0d0) then
      ! Normal intersects the side
      dx = x1p + t * x21
      dy = y1p + t * y21
      d = dx**2 + dy**2
      if (d < inpoly) then
        ! Smallest distance yet
        snear = .true.
        inpoly = d
        j = i
      end if
    else
      ! Point is closer to the next vertex, continue on to next side
      cycle
    end if
  end do

  if (inpoly < smalld) then
    ! Point lies on the side of the polygon
    inpoly = 0.0d0
  else
    next = j + 1
    prev = j - 1
    if (j == 1) then
      prev = npoly
    else if (j == npoly) then
      next = 1
    else
      continue
    end if
    if (snear) then
      ! Point is closer to side. Use determinant to determine if the triangle
      ! formed by the vertices and point is positive or negative. Positive indicates
      ! the point is inside (form a counter-clockwise triangle)
      area = tridet(polyx(j),polyy(j),polyx(next),polyy(next),px,py)
      inpoly = sign(inpoly, area)
    else
      ! Point is closer to node. Check if nearest vertex is concave
      ! If concave, point is inside the polygon
      !if (j == 1) j = npoly + 1
      area = tridet(polyx(next),polyy(next),polyx(j),polyy(j),polyx(prev),polyy(prev))
      inpoly = sign(inpoly, area)
    end if
  end if

  return
  end function pinpol

!-----------------------------------------------------------------------------!

function tridet(x1, y1, x2, y2, x3, y3) result(det)
  implicit none
!-----------------------------------------------------------------------------!
! Computes twice the area of the triangle defined by coordinates
! (x1,y1) (x2,y2) (x3,y3) using determinate formula
!
! If the area is positive, the points are counter-clockwise
! If the area is negative, the points are clockwise
! If the area is zero, two or more of the points are co-located or all three
!    points are collinear
!
! Useful for determining what side of a line a point (x3, y3) is on.
!-----------------------------------------------------------------------------!
  real    , intent(in)     :: x1, y1, x2, y2, x3, y3
  real                     :: det

  det = (x1-x3)*(y2-y3)-(x2-x3)*(y1-y3)

  return
  end function tridet
  
!-----------------------------------------------------------------------------!
  
function indices_from_mask(mask) result(idx_array)
    implicit none
    ! Returns array of length mask==.true. with the indices of true values from
    ! 1:length(mask) - can be used to subset an array with only the true values
    logical, intent(in)     :: mask(:)
    integer, allocatable    :: idx_array(:)
    integer                 :: count_true, i, idx_counter

    allocate(idx_array(count(mask)))

    ! Fill the result array with indices where mask is true
    idx_counter = 0
    do i = 1, size(mask)
      if (mask(i)) then
        idx_counter = idx_counter + 1
        idx_array(idx_counter) = i
      end if
    end do
    
    return
end function indices_from_mask

!-----------------------------------------------------------------------------!
  
end module tools 