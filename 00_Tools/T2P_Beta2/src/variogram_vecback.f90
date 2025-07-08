module m_vario
  use m_global, only: dp
!-------------------------------------------------------------------------------------------------!

type :: t_vgm
! Variogram or cross variogram structure
  integer                 :: itype       ! Variogram model (0-linear, 1-spherical, 2-exponential)
  real(dp)                :: sill        ! Contribution of each structure, when summed (with nugget) the sill
  real(dp)                :: a_hmax      ! Range: distance where data are no longer correlated
  real(dp)                :: a_hmin      ! Range min (equal to amax when anis1/anis2 ==1)
  real(dp)                :: a_vert      ! Vertical range
  real(dp)                :: nugget      ! Nugget: sill immediately at distances > 0
  real(dp)                :: ang1        ! Angle 1 (azimuth correction)
  real(dp)                :: ang2        ! Angle 2 (dip correction)
  real(dp)                :: ang3        ! Angle 3 (plunge correction)
  real(dp)                :: anis1       ! Anisotropy 1 (horizontal)
  real(dp)                :: anis2       ! Anisotropy 2 (vertical)
  real(dp)                :: rotmat(3,3) ! Rotation Matrix

  contains
    procedure,public        :: initialize
    procedure,private       :: setrot
    procedure,private       :: sqdist
    procedure,public        :: sqdist_vec
    procedure,public        :: cov
end type t_vgm

!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  function get_vgm_from_line(strings, struct_id,interp_dim) result(vgm)
    use m_file_io
    use m_vstringlist, only: t_vstringlist
    use m_error_handler, only: error_handler
    implicit none

    type(t_vstringlist),intent(in)   :: strings
    integer, intent(in)              :: struct_id, interp_dim
    class(t_vgm),pointer             :: vgm

    allocate(vgm)

    vgm%itype  = find_string_index_in_list(strings, 2, "SPH,EXP,HOL,GAU,POW,CIR,LIN", toupper=.true.)
    if (vgm%itype==0) then
      call error_handler(1,"Invalid variogram type - must be one of Lin, Sph, Exp")
    else
      ! Convert to zero index (see itype definiton in t_vgm type)
      vgm%itype = vgm%itype - 1
    end if

    ! TODO implement different reading for nonstandard variograms (e.g. power)
    if (interp_dim==2) then
      vgm%nugget = item2real(strings, 3)
      vgm%sill   = item2real(strings, 4)
      vgm%a_hmin = item2real(strings, 5)
      vgm%a_hmax = item2real(strings, 6)
      vgm%ang1   = item2real(strings, 7)
      ! Sanitize unused variables
      vgm%a_vert = vgm%a_hmin
      vgm%ang2   = 0.0
      vgm%ang3   = 0.0
      !nnear read outside of routine
    else if (interp_dim==3) then
      vgm%nugget = item2real(strings, 3)
      vgm%sill   = item2real(strings, 4)
      vgm%a_hmin = item2real(strings, 5)
      vgm%a_hmax = item2real(strings, 6)
      vgm%a_vert = item2real(strings, 7)
      vgm%ang1   = item2real(strings, 8)
      vgm%ang2   = item2real(strings, 9)
      vgm%ang3   = item2real(strings,10)
      !nnear read outside of routine
    end if

    ! TODO - more parameters for search ellipsoid? Read elsewhere?

    ! initialize
    call vgm%initialize(struct_id)

  end function get_vgm_from_line

!-------------------------------------------------------------------------------------------------!
! BASE CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize(this, sid)
    implicit none
    class(t_vgm)         :: this
    integer, intent(in)  :: sid
    integer              :: i,j

    ! Sets anis1, anis2 ratios based on ranges aa1, aa2
    this%anis1 = this%a_hmin/this%a_hmax
    this%anis2 = this%a_vert/this%a_hmax

    ! Correct for zero a_vert (may not be using 3D kriging)
    if (this%anis2 < tiny(this%anis2)) this%anis2 = 1.0

    ! Zero out nuggets on structures above 1
    if (sid > 1) this%nugget = 0.0

    ! Now the rotation matrices get initialized
    call this%setrot()

  end subroutine initialize

!-------------------------------------------------------------------------------------------------!

  subroutine setrot(this)
    implicit none
    class(t_vgm)         :: this
!
!              Sets up an Anisotropic Rotation Matrix
!              **************************************
!
! Sets up the matrix to transform cartesian coordinates to coordinates
! accounting for angles and anisotropy (see manual for a detailed
! definition):
!
!
! INPUT PARAMETERS: (moved to module definition)
!
!   ang1             Azimuth angle for principal direction
!   ang2             Dip angle for principal direction
!   ang3             Third rotation angle
!   anis1            First anisotropy ratio
!   anis2            Second anisotropy ratio
!   ind              The matrix indicator to initialize
!   MAXROT           The maximum number of rotation matrices dimensioned
!   rotmat           The rotation matrices
!
! Author: C. Deutsch                                Date: September 1989
! Modified for T2P by Leland Scantlebury
!-------------------------------------------------------------------------------------------------!
      real*8, parameter         :: DEG2RAD = 3.141592654/180.0, &
                                   EPSLON = 1.e-10
      real*8                    :: alpha, beta, theta,&
                                   sina, sinb, sint, &
                                   cosa, cosb, cost, &
                                   afac1, afac2
!
! Converts the input angles to three angles which make more
!  mathematical sense:
!
!         alpha   angle between the major axis of anisotropy and the
!                 E-W axis. Note: Counter clockwise is positive.
!         beta    angle between major axis and the horizontal plane.
!                 (The dip of the ellipsoid measured positive down)
!         theta   Angle of rotation of minor axis about the major axis
!                 of the ellipsoid.
!

      if(this%ang1.ge.0.0d0.and.this%ang1.lt.270.0d0) then
          alpha = (90.0d0   - this%ang1) * DEG2RAD
      else
          alpha = (450.0d0  - this%ang1) * DEG2RAD
      endif
      beta  = -1.0d0 * this%ang2 * DEG2RAD
      theta =          this%ang3 * DEG2RAD
!
! Get the required sines and cosines:
!
      sina = sin(alpha)
      sinb = sin(beta)
      sint = sin(theta)
      cosa = cos(alpha)
      cosb = cos(beta)
      cost = cos(theta)
!
! Construct the rotation matrix in the required memory:
!
      afac1 = 1.0d0 / max(this%anis1,EPSLON)
      afac2 = 1.0d0 / max(this%anis2,EPSLON)
      this%rotmat(1,1) =       (cosb * cosa)
      this%rotmat(1,2) =       (cosb * sina)
      this%rotmat(1,3) =       (-sinb)
      this%rotmat(2,1) = afac1*(-cost*sina + sint*sinb*cosa)
      this%rotmat(2,2) = afac1*(cost*cosa + sint*sinb*sina)
      this%rotmat(2,3) = afac1*( sint * cosb)
      this%rotmat(3,1) = afac2*(sint*sina + cost*sinb*cosa)
      this%rotmat(3,2) = afac2*(-sint*cosa + cost*sinb*sina)
      this%rotmat(3,3) = afac2*(cost * cosb)
!
! Return to calling program:
!
      return
    end subroutine setrot

!-------------------------------------------------------------------------------------------------!

  real function sqdist(this,x1,y1,z1,x2,y2,z2)
    implicit none
    class(t_vgm)         :: this
  !
  !    Squared Anisotropic Distance Calculation Given Matrix Indicator
  !    ***************************************************************
  !
  ! This routine calculates the anisotropic distance between two points
  !  given the coordinates of each point and a definition of the
  !  anisotropy.
  !
  ! INPUT VARIABLES:
  !   x1,y1,z1         Coordinates of first point
  !   x2,y2,z2         Coordinates of second point
  !   ind              The matrix indicator to initialize
  !   MAXROT           The maximum number of rotation matrices dimensioned
  !   rotmat           The rotation matrices
  !
  !
  ! OUTPUT VARIABLES:
  !   sqdist           The squared distance accounting for the anisotropy
  !                      and the rotation of coordinates (if any).
  !
  !
  ! Author: C. Deutsch                                Date: September 1989
        integer       :: i
        real(dp)      :: dx,dy,dz,x1,y1,x2,y2,z1,z2,cont
  !
  ! Compute component distance vectors and the squared distance:
  !
        dx = x1 - x2
        dy = y1 - y2
        dz = z1 - z2
        sqdist = 0.0d0
        do i=1,3
          cont   = this%rotmat(i,1) * dx &
                 + this%rotmat(i,2) * dy &
                 + this%rotmat(i,3) * dz
          sqdist = sqdist + cont * cont
        end do
        return

  end function sqdist

!-------------------------------------------------------------------------------------------------!

function sqdist_vec(this, dim, p, points, n)
  implicit none
  class(t_vgm)            :: this
  integer, intent(in)     :: dim, n
  real(dp), intent(in)    :: p(dim)
  real(dp)                :: sqdist_vec(n)
  real(dp), intent(in)    :: points(dim, n)
  real(dp)                :: dd(dim, n)

  dd = points - spread(p, 2, n)
  sqdist_vec = sum(matmul(this%rotmat(1:dim, 1:dim), dd)**2, dim=1)

end function sqdist_vec

!-------------------------------------------------------------------------------------------------!

  function cov(this, dist, n)
    implicit none
    real(dp), parameter   :: pi = 3.141592653589793d0
    real(dp),parameter       :: epsilon=1.e-10
    class(t_vgm)          :: this
    integer               :: n
    real(dp)              :: dist(n)
    real(dp)              :: cov(n)
    real(dp)              :: hr(n)

    cov = 0.0
    hr = dist / this%a_hmax

    select case (this%itype)
      ! Spherical model
      case(1); where(dist < this%a_hmax) cov = 1.0 - 1.5 * hr + 0.5 * hr**3

      ! Exponential model
      case(2); cov = exp(-3.0 * hr)

      ! Hole effect model
      case(3); cov = cos(pi * hr)

      ! Gaussian model
      case(4); cov = exp(-49.0 / 16.0 * hr**2)

      ! Power model
      case(5); cov = dist**this%a_hmax

      ! Circular model
      case(6); where(dist < this%a_hmax) cov = (2 * hr * sqrt(1.0 - hr**2) + 2 * asin(hr)) / pi

      ! Linear model
      case(7); cov = this%sill - hr

      ! Default case for unknown model type
    case default
      print*, 'Unknown variogram model.'
      stop
    end select

    if (this%itype /= 7) cov = this%sill * cov
    if (this%nugget > 0.0_dp) then
      where (dist < epsilon) cov = cov + this%nugget
    end if

  end function cov
!-------------------------------------------------------------------------------------------------!

  !function cov(this, x1, y1, z1, x2, y2, z2) result(value)
  !  implicit none
  !  class(t_vgm)         :: this
! !     common block/vario/slope,sill,a
  !  real(dp)                 :: x1,y1,z1,x2,y2,z2,h,tmp
  !  real(dp),parameter       :: epsilon=1.e-10
  !  real(dp)                 :: value
  !
  !  h = this%sqdist(x1,y1,z1,x2,y2,z2)
  !
  !  ! Check for "zero" distance, return with maximum value
  !  if (h < epsilon) then
  !    value = this%nugget + this%sill
  !    return  ! EARLY RETURN
  !  ! linear variogram
  !  else if(this%itype.eq.1) then
  !    h=sqrt(h)*this%sill
  !    !write(11,*) 'Linear', h
  !  ! spherical variogram
  !  else if(this%itype.eq.2) then
  !    H=SQRT(h)
  !    tmp=h/this%a_hmax
  !    if(tmp.ge.1.) then
  !      h=this%sill
  !    else
  !      h=this%sill*(1.5*tmp-0.5*tmp*tmp*tmp)
  !    endif
  !  ! exponential variogram
  !  else if (this%itype.eq.3) then
  !    H=SQRT(h)
  !    h=this%sill*(1.d0-exp(-3.d0*h/this%a_hmax))
  !  endif
  !  value=this%sill-h
  !
  !end function cov

!-------------------------------------------------------------------------------------------------!
end module m_vario