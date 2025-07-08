module m_kriging
  use m_global, only: dp
  use m_interpolator
  use m_weights, only: t_weights, create_weight_object
  use m_grid, only: t_grid, create_grid_object
  use m_vario, only: t_vgm
  use m_datasets, only: t_dataset, t_layerdata
  use m_error_handler, only: error_handler
  use m_options, only: t_options
  use m_sparse_symmetric, only: t_sparse_sym
  implicit none
!-------------------------------------------------------------------------------------------------!

  type, extends(t_interpolator) :: t_krige
    integer                          :: nvario            ! Number of variograms stored for interpolator
    integer,pointer                  :: nnear(:)          ! Number of nearest points used for interpolation
    integer,pointer                  :: nstruct(:)        ! Number of structures for each variogram
    type(t_vgm),pointer              :: variograms(:,:)   ! (stuctures, variogram)
    integer,allocatable              :: vario_idx(:,:)    ! Variogram index given (class_id, class_id)
    integer                          :: nsets             ! Number of datasets involved
    real(dp),allocatable             :: w(:)              ! Internally calculated weights
    ! Variables from PPSGS (slightly adjusted)
    integer                          :: ndim, ngrid, ndrift, seed, unbias, nsim
    integer,allocatable              :: nobs(:), nmax(:), near_idx(:)
    real(dp),allocatable             :: matA(:,:), rhsB(:)
    ! Attributes for KD-tree and search parameters could be added here
  contains
    procedure               :: intialize => initialize_kriging
    procedure               :: setup => setup_krige
    procedure               :: pset_extreme
  end type t_krige
!-------------------------------------------------------------------------------------------------!
  
  type, extends(t_krige) :: t_simpkrige
  contains
    procedure               :: setup => setup_sk
    procedure               :: predict_point => predict_point_sk
  end type t_simpkrige
!-------------------------------------------------------------------------------------------------!
  
  type, extends(t_krige) :: t_ordkrige
    integer                 :: maxnear
    integer,allocatable     :: usenear(:)
  contains
    procedure               :: setup => setup_ok
    procedure               :: predict_point => predict_point_ok
  end type t_ordkrige
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!
  
  subroutine initialize_kriging(this)
    implicit none
    class(t_krige)                :: this
    
    
  end subroutine initialize_kriging
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine setup_krige(this, obs_data, to_grid, cat, to_cats, weights, opt)
    ! Does everything common to all kriging
    ! Intended to be called by subclasses during their setup
    implicit none
    class(t_krige)              :: this
    class(t_dataset)            :: obs_data(:)
    type(t_grid),intent(in)     :: to_grid
    real(dp)                    :: weights(:,:)
    integer,intent(in)          :: cat
    integer,intent(in)          :: to_cats(:)
    type(t_options),intent(in)  :: opt
    integer                     :: i
    
    ! Defaults
    this%ndim   = opt%INTERP_DIM
    this%ndrift = 0  ! Hardcoded out for now
    this%unbias = 0 
    this%nsets  = size(obs_data)
    this%ngrid  = to_grid%n
    
    !call random_seed_initialize(seed)
    !call set_samples()
    
    !if (nsim>0) then
    !  call set_randpath()
    !  grid = grid(:, irandpath)
    !else
    !  irandpath=[(ig, ig=1, ngrid)]
    !end if
    
    
  end subroutine setup_krige
  
!-------------------------------------------------------------------------------------------------!
  
  function total_cov(variograms, nstruct, dist, npoints)
    implicit none

    integer,intent(in)      :: nstruct, npoints
    type(t_vgm), intent(in) :: variograms(:)
    real(dp), intent(in)    :: dist(nstruct)
    integer                 :: i, j
    real(dp)                :: total_cov(npoints)

    ! Initialize total covariance to zero
    total_cov = 0.0_dp

    ! Loop over each structure within the given variogram
    do i = 1, nstruct
      ! Sum covariances from each structure
      total_cov = total_cov + variograms(i)%cov(dist, npoints)
    end do

    return
  end function total_cov
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine pset_extreme(this, obs_data, ntot, p, id, maxpt)
    implicit none
    ! Rewrite of PSET routine for cokriging
    ! Author Leland Scantlebury
    !
    ! Copyright 2022 S.S. Papadopulos & Associates. All rights reserved.
    
    ! Inputs
    class(t_krige)         :: this
    type(t_dataset)        :: obs_data(:)
    integer,intent(in)     :: maxpt, ntot
    integer,intent(inout)  :: id(ntot)
    real,intent(inout)     :: p(maxpt)
    
    ! Subroutine variables
    integer             :: i,ii,j,jj,n,ind,JS, rowcount
    integer,allocatable :: colcount(:)
    real                :: x1,x2,y1,y2,value,value_sum
    
    allocate(colcount(this%nsets))
    
    colcount = 0
    do i=1, (this%nsets-1)
      colcount(i+1) = colcount(i) + obs_data(i)%grid%n
    end do
    
!***  ZERO OUT ELEMENTS OF P-MATRIX NEEDED FOR KRIGING
    P=0.0
!***  CALCULATE THE POINTERS FOR ID
    ID(1)=1
    DO I=2,NTOT
      ID(I)=ID(I-1)+NTOT+2-I
    END DO
    ! Outer Loop over datasets (columns)
    do j=1, this%nsets
      ! Inner loop over datasets (rows)
      rowcount = 0
      do i=j, this%nsets
        ! Variogram index for section
        ind = this%vario_idx(j,i)
        ! Outer loop over column points
        do jj=1, obs_data(j)%grid%n
          JS=ID(colcount(j)+jj)-(colcount(j)+jj)
          x2=obs_data(j)%grid%coords(1,jj)
          y2=obs_data(j)%grid%coords(2,jj)
          ! Inner loop over row points
          ii = jj
          if (i/=j) ii=1
          do while (ii <= obs_data(i)%grid%n)
            x1=obs_data(i)%grid%coords(1,ii)
            y1=obs_data(i)%grid%coords(2,ii)
            value_sum=0.0d0
            !do n=1, nstruct(ind)
            !  call variogram(ind,n,x1,y1,0.0d0,x2,y2,0.0d0,value)
            !  value_sum = value_sum + value
            !end do
            P(JS+rowcount+colcount(j)+ii)=value_sum
            ii = ii + 1
          end do
        end do
        rowcount = rowcount + (ii-1)
      end do
    end do
    ! Fill last row with unbias
    if (this%unbias > 1) then
      DO I=1,ntot-1
        P(ID(I)+ntot-I)=1
      END DO
    end if
      
    deallocate(colcount)
    RETURN

  end subroutine pset_extreme
  
!-------------------------------------------------------------------------------------------------!
    
!-------------------------------------------------------------------------------------------------!
! SIMPLE KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine setup_sk(this, obs_data, to_grid, cat, to_cats, weights, opt)
    implicit none
    class(t_simpkrige)          :: this
    class(t_dataset)            :: obs_data(:)
    type(t_grid),intent(in)     :: to_grid
    real(dp)                    :: weights(:,:)
    integer,intent(in)          :: cat
    integer,intent(in)          :: to_cats(:)
    type(t_options),intent(in)  :: opt
    ! Local
    integer                     :: i,matsize
    
    ! Call super setup
    call setup_krige(this, obs_data, to_grid, cat, to_cats, weights, opt)
    
    matsize = 0
    do i=1, this%nsets
      matsize = obs_data(i)%grid%n
    end do
    
    matsize = to_grid%n+matsize+this%unbias+this%ndrift
    allocate(this%w(matsize))
    allocate(this%matA(matsize, matsize), this%rhsB(matsize))
    
    this%matA = 0.0
    this%rhsB = 0.0
    
    !pre-calculate distances
    !call setdist()
    
  end subroutine setup_sk
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine predict_point_sk(this, idx, p, obs_data, cat, weights)
    implicit none
    class(t_simpkrige), intent(in)    :: this
    integer,intent(in)                :: idx, cat
    real(dp),intent(in)               :: p(2)
    type(t_dataset)                   :: obs_data(:)
    real(dp)                          :: weights(:)
  
  end subroutine predict_point_sk
  
!-------------------------------------------------------------------------------------------------!
  
!-------------------------------------------------------------------------------------------------!
! ORDINARY KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine setup_ok(this, obs_data, to_grid, cat, to_cats, weights, opt)
    implicit none
    class(t_ordkrige)          :: this
    class(t_dataset)            :: obs_data(:)
    type(t_grid),intent(in)     :: to_grid
    real(dp)                    :: weights(:,:)
    integer,intent(in)          :: cat
    integer,intent(in)          :: to_cats(:)
    type(t_options),intent(in)  :: opt
    ! Local
    integer                     :: i,matsize,vidx
    
    ! Call super setup
    call setup_krige(this, obs_data, to_grid, cat, to_cats, weights, opt)
    
    ! Setup nnearest tracker (usenear) (sometimes there is not this%nnear(i) values in the dataset)
    allocate(this%usenear(this%nsets))
    
    ! Build search trees in observed grid objects
    ! Also, figure out number of datasets in each category
    do i=1, this%nsets
      call obs_data(i)%build_tree_by_category(cat, this%variograms(1,i)%rotmat(1:this%ndim,1:this%ndim))
      this%usenear(i) = min(this%nnear(i), obs_data(i)%grid%tree%n)
      ! TODO Warn that there is so little data? Do we even have enough information to warn?
      ! Warnings only get printed before the calculations
      ! In this scope we don't if it's pp or data...
    end do
    
    this%maxnear = this%nnear(1)
    if (this%nsets > 1) this%maxnear = this%maxnear + maxval(this%nnear)
    
    matsize = sum(this%nnear)+this%unbias+this%ndrift
    allocate(this%w(matsize))
    allocate(this%matA(matsize, matsize), this%rhsB(matsize))
    
  end subroutine setup_ok
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine predict_point_ok(this, idx, p, obs_data, cat, weights)
    implicit none
    class(t_ordkrige), intent(in)     :: this
    integer,intent(in)                :: idx, cat
    real(dp),intent(in)               :: p(2)
    type(t_dataset)                   :: obs_data(:)
    real(dp)                          :: weights(:)
    ! local
    integer                           :: i, j, vidx, cidx
    integer                           :: inear(this%maxnear,this%nvario)
    real(dp)                          :: dist(this%maxnear,this%nvario)
    
    ! Get nearest for each obs dataset
    do i=1, this%nsets
      vidx = this%vario_idx(i,i)              ! Should always equal i, but to be safe
      call obs_data(i)%grid%get_nnear(p, this%usenear(vidx), inear(:,vidx), dist(:,vidx))
    end do
    
    ! Get distances for obs crossvariograms
    do i=1, this%nsets
      do j=i+1, this%nsets
        vidx = this%vario_idx(i,i)
        cidx = this%vario_idx(j,i)
        if (cidx < 1) continue
        dist(1:this%nnear(i),vidx) = dist_2D_vector(p, obs_data(i)%grid%coords(:,inear(:,vidx)))
        dist(this%nnear(i)+1:this%nnear(j),vidx) = dist_2D_vector(p, obs_data(i)%grid%coords(:,inear(:,cidx)))
      end do
    end do
    
    ! Build Matrix
    ! Loop over columns (all datasets)
    j = 1 ! Tracks column
    do dout=1, this%nsets
      ! Loop over points in outer dataset
      do jj=1, this%nnear(dout)
        !x2=dsets(dout)%xy(1,jj)
        !y2=dsets(dout)%xy(2,jj)
        i = j  ! Start on the diagonal
        ! Loop over rows (starts from dout dataset, b/c diag)
        do din=dout, this%nsets
          !ind = din
          ind = this%vario_idx(din,dout)
          !if (din/=dout) ind = this%nsets + (din-1) + (dout-1)
          ! Loop over points in inner dataset
          do ii=1, dsets(din)%n
            if ((din==dout).and.(ii<jj)) cycle  ! Stay on diagonal
            !x1=dsets(din)%xy(1,ii)
            !y1=dsets(din)%xy(2,ii)
            ! Loop over variogram structures
            value_sum=0.0d0
            do n=1, nstruct(ind)
              !call variogram(ind,n,x1,y1,0.0d0,x2,y2,0.0d0,value)
              !value_sum = value_sum + value
            end do
            ! Store in matrix
            a(i,j) = value_sum
            i = i + 1
          end do ! End of ii loop over din points
        end do  ! End of din row loop
      j = j+1
      end do  ! End of jj loop over dout points
    end do  ! End of dout column loop
  
  end subroutine predict_point_ok
  
!-------------------------------------------------------------------------------------------------!
  
end module m_kriging