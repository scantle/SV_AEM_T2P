module m_vario

!-------------------------------------------------------------------------------------------------!

  real    , parameter   :: pi = 3.141592653589793d0

type :: t_vgm
! Variogram or cross variogram structure
  integer                 :: itype       ! Variogram model (0-linear, 1-spherical, 2-exponential)
  integer                 :: ndim        ! Number of dimensions
  real                    :: sill        ! Contribution of each structure, when summed (with nugget) the sill
  real                    :: a_hmax      ! Range: distance where data are no longer correlated
  real                    :: a_hmin      ! Range min (equal to amax when anis1/anis2 ==1)
  real                    :: a_vert      ! Vertical range
  real                    :: nugget      ! Nugget: sill immediately at distances > 0
  real                    :: ang1        ! Angle 1 (azimuth correction)
  real                    :: ang2        ! Angle 2 (dip correction)
  real                    :: ang3        ! Angle 3 (plunge correction)
  real                    :: anis1       ! Anisotropy 1 (horizontal)
  real                    :: anis2       ! Anisotropy 2 (vertical)
  real                    :: rotmat(3,3) ! Rotation Matrix

  contains
    procedure,public        :: initialize
    procedure,private       :: setrot
    procedure,private       :: sqdist
    procedure,public        :: sqdist_vec
    procedure,public        :: cov
    procedure,public        :: cov_vec
    procedure,public        :: write_variogram
end type t_vgm

!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine get_vgm_from_line(vgm, strings, struct_id,interp_dim,maxread)
    use m_file_io
    use m_vstringlist, only: t_vstringlist
    use m_error_handler, only: error_handler
    implicit none

    type(t_vstringlist),intent(in)   :: strings
    integer, intent(in)              :: struct_id, interp_dim
    integer, intent(out)             :: maxread
    class(t_vgm), intent(inout)      :: vgm

    ! allocate(vgm)
    maxread = 0

    vgm%itype  = find_string_index_in_list(strings, 2, "SPH,EXP,HOL,GAU,POW,CIR,LIN", toupper=.true.)
    if (vgm%itype==0) then
      call error_handler(1,"Invalid variogram type - must be one of Exp, Sph, Hol, Gau, Cir, Lin")
    end if

    ! TODO implement different reading for nonstandard variograms (e.g. power)
    if (interp_dim==2) then
      vgm%nugget = item2dp(strings, 3)
      vgm%sill   = item2dp(strings, 4)
      vgm%a_hmin = item2dp(strings, 5)
      vgm%a_hmax = item2dp(strings, 6)
      vgm%ang1   = item2dp(strings, 7)
      maxread = 7
      ! Sanitize unused variables
      vgm%a_vert = vgm%a_hmax
      vgm%ang2   = 0.0
      vgm%ang3   = 0.0
      !nnear read outside of routine
    else if (interp_dim==3) then
      vgm%nugget = item2dp(strings, 3)
      vgm%sill   = item2dp(strings, 4)
      vgm%a_hmin = item2dp(strings, 5)
      vgm%a_hmax = item2dp(strings, 6)
      vgm%a_vert = item2dp(strings, 7)
      vgm%ang1   = item2dp(strings, 8)
      vgm%ang2   = item2dp(strings, 9)
      vgm%ang3   = item2dp(strings,10)
      maxread = 10
      !nnear read outside of routine
    end if

    vgm%ndim = interp_dim
    ! TODO - more parameters for search ellipsoid? Read elsewhere?

    ! initialize
    call vgm%initialize(struct_id)
  end subroutine get_vgm_from_line

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

  real function sqdist(this,dim,p1,p2)
    implicit none
    class(t_vgm)            :: this
    integer, intent(in)     :: dim
    real    , intent(in)    :: p1(dim), p2(dim)
  !
  ! Compute component distance vectors and the squared distance:
    !dd = p1 - p2

    ! Perform the matrix-vector multiplication using the rotation matrix:
    ! Calculate the squared distance:
    sqdist = sum(matmul(this%rotmat(1:dim, 1:dim), (p1 - p2))**2)
    return
  end function sqdist

!-------------------------------------------------------------------------------------------------!

  function sqdist_vec(this, p, points, n)
    implicit none
    class(t_vgm)            :: this
    integer, intent(in)     :: n
    real    , intent(in)    :: p(this%ndim)
    real                    :: sqdist_vec(n)
    real    , intent(in)    :: points(this%ndim, n)
    real                    :: dd(this%ndim, n)

    dd = points - spread(p, 2, n)
    sqdist_vec = sum(matmul(this%rotmat(1:this%ndim, 1:this%ndim), dd)**2, dim=1)

  end function sqdist_vec

!-------------------------------------------------------------------------------------------------!

  function cov(this, p1,p2) result(cov_value)
    implicit none

    real    ,parameter       :: epsilon=1.e-10
    class(t_vgm)          :: this
    real    , intent(in)  :: p1(:), p2(:)
    real                  :: dist
    real                  :: cov_value
    real                  :: hr

    dist = sqrt(this%sqdist(size(p1,1),p1,p2))
    cov_value = 0.0
    hr = dist / this%a_hmax

    select case (this%itype)
      case(1)  ! Spherical model
        if (dist < this%a_hmax) then
          cov_value = 1.0 - 1.5 * hr + 0.5 * hr**3.0
        end if

      case(2)  ! Exponential model
        cov_value = exp(-3.0 * hr)

      case(3)  ! Hole effect model
        cov_value = cos(pi * hr)

      case(4)  ! Gaussian model
        cov_value = exp(-49.0 / 16.0 * hr**2)

      case(5)  ! Power model
        cov_value = dist**this%a_hmax

      case(6)  ! Circular model
        if (dist < this%a_hmax) cov_value = (2 * hr * sqrt(1.0 - hr**2) + 2 * asin(hr)) / pi

      case(7)  ! Linear model
        cov_value = this%sill - hr
    end select

    if (this%itype /= 7) cov_value = this%sill * cov_value
    if (this%nugget > 0.0) then
      if (dist < epsilon) cov_value = cov_value + this%nugget
    end if

  end function cov

!-------------------------------------------------------------------------------------------------!

  function cov_vec(this, dist, n) result(cov)
    implicit none
    real    ,parameter    :: epsilon=1.e-10
    class(t_vgm)          :: this
    integer               :: n
    real                  :: dist(n)
    real                  :: cov(n)
    real                  :: hr(n)

    cov = 0.0
    hr = dist / this%a_hmax

    select case (this%itype)
      ! Spherical model
      case(1); where (dist < this%a_hmax) cov = 1.0 - 1.5 * hr + 0.5 * hr**3.0

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
    if (this%nugget > 0.0) then
      where (dist < epsilon) cov = cov + this%nugget
    end if

  end function cov_vec
!-------------------------------------------------------------------------------------------------!
  
  function write_variogram(this) result(line)
    implicit none
    class(t_vgm), intent(in) :: this
    character(len=256)       :: line

    character(len=3), dimension(7) :: model_names
    character(len=3) :: model
    integer :: id

    ! Model name lookup
    model_names = ["SPH", "EXP", "HOL", "GAU", "POW", "CIR", "LIN"]
    id = this%itype

    if (id >= 1 .and. id <= size(model_names)) then
      model = model_names(id)
    else
      model = "UNK"
    end if

    if (this%ndim == 2) then
      write(line, '(A,1X,F7.4,1X,F7.4,1X,ES12.4,1X,ES12.4,1X,F8.2)') &
          trim(model), this%nugget, this%sill, this%a_hmin, this%a_hmax, this%ang1
    else if (this%ndim == 3) then
      write(line, '(A,1X,F7.4,1X,F7.4,1X,ES12.4,1X,ES12.4,1X,ES12.4,1X,F8.2,1X,F5.2,1X,F5.2)') &
          trim(model), this%nugget, this%sill, this%a_hmin, this%a_hmax, this%a_vert, this%ang1, this%ang2, this%ang3
    end if
  end function write_variogram

!-------------------------------------------------------------------------------------------------!

  !function cov(this, x1, y1, z1, x2, y2, z2) result(value)
  !  implicit none
  !  class(t_vgm)         :: this
! !     common block/vario/slope,sill,a
  !  real                     :: x1,y1,z1,x2,y2,z2,h,tmp
  !  real    ,parameter       :: epsilon=1.e-10
  !  real                     :: value
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