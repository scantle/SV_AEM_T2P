module m_kriging
  use m_global, only: log
  use m_interpolator, only: t_interpolator
  use m_weights, only: t_weights
  use m_grid, only: t_grid, create_grid_object
  use m_vario, only: t_vgm
  use m_datasets, only: t_dataset, t_layerdata
  use m_error_handler, only: error_handler
  use m_options, only: t_options
  use m_vstringlist
!  use m_sparse_symmetric, only: t_sparse_sym
  implicit none
!-------------------------------------------------------------------------------------------------!

  type, extends(t_interpolator) :: t_krige
    integer                   :: nvario                  ! Number of variograms stored for interpolator
    integer                   :: maxnear                 ! Maximum number of nearest points used for interpolation for all classes
    integer                   :: nsets                   ! Number of datasets involved
    integer                   :: ndim                    ! Number of dimensions being kriged
    integer                   :: unbias                  ! Unbias term (0 - SK, 1 - OK)
    integer                   :: ndrift                  ! Unimplemented
    integer                   :: nzones                  ! Tracks length of zones list (see below)
    real                      :: max_sdist               ! Maximum inclusion distance for secondary data
    real   ,allocatable       :: matA(:)                 ! LHS obs-obs covariance matrix (lower triangle, stored as an array)
    real   ,allocatable       :: rhsB(:)                 ! RHS grid-obs covariance array
    integer,pointer           :: nstruct(:)              ! Number of structures for each variogram
    integer,pointer           :: nnear(:)                ! Number of nearest points used for interpolation, for each dataset/class (nvario) (0 for cross)
    integer,pointer           :: obs_idx(:)              ! Indices of datasets
    type(t_vstringlist)       :: zones                   ! List of HSUs, needed to track the order of HSUs in global_vario_idx (may not match global HSU id!)
    integer,allocatable       :: cat2zone(:)             ! Map from global category id (hsu_id) to internal zone number (read at different times)
    type(t_vgm),pointer       :: variograms(:,:)         ! (stuctures, variogram)
    integer,allocatable       :: global_vario_idx(:,:,:) ! Variogram index given (hsu, class_id, class_id) for ALL classes
    integer,allocatable       :: vario_idx(:,:)          ! Variogram index given (class_id, class_id) for local classes passed to setup/point
    real   ,allocatable       :: w(:)                    ! Internally calculated weights
    integer,allocatable       :: pset_id(:)              ! ID vector used for kriging matrix
    ! Some kriging-specific settings
    logical                   :: no_negative_weights     ! Flag to zero negative weights and rebalance (True = zero, rebalance)
    logical                   :: write_matrix            ! Flag to write AMAT and RHS to log file
    ! Attributes for KD-tree and search parameters could be added here
  contains
    procedure               :: intialize => initialize_kriging
    procedure               :: add_zonal_variogram
    procedure               :: init_zones
    procedure               :: setup => setup_krige
    procedure               :: pset_cok
    procedure               :: filter_near_points
    procedure               :: write_amat, write_rhs_array, write_inear
    procedure               :: write_summary => write_summary_krige
    procedure               :: finalize => finalize_krige
  end type t_krige
!-------------------------------------------------------------------------------------------------!

  type, extends(t_krige) :: t_globkrige
    integer,allocatable       :: npoints(:)            ! Number of points (in category) to be used for simple kriging
    integer,allocatable       :: useidx(:,:)           ! Indices of points to use for simple kriging, within category, by (npoints, nset)
  contains
    procedure               :: setup => setup_sk
    procedure               :: predict_point => predict_point_sk
  end type t_globkrige
!-------------------------------------------------------------------------------------------------!

  type, extends(t_krige) :: t_treekrige
    integer,allocatable       :: usenear(:)            ! NNear but local to problem (nsets) and limited by tree size
  contains
    procedure               :: setup => setup_ok
    procedure               :: predict_point => predict_point_ok
    procedure               :: wsize_estimate => wsize_estimate_ok
    procedure               :: finalize => finalize_ok
  end type t_treekrige
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

subroutine decomp(p,id,ntot,ier)
    implicit none
!*** DECOMPOSITION TO GET LOWER TRIANGULAR S MATRIX
    integer                :: id,ier,ks,jm1,k,js,is,is1,j,i,i1,ntot
    real                   :: p,sum,p1,p2
    dimension              :: p(1),id(1)

          IER=0
          P(1)=1./P(1)
          DO 65 J=2,NTOT
          DO 65 I=J,NTOT
            KS=ID(I)
            JM1=J-1
            SUM=0.
            DO 55 K=1,JM1
              JS=ID(K)
              IS=JS+I-K
              IS1=JS+J-K
              P1=P(IS)
              P2=P(IS1)
              IF (P1) 39,55,39
   39         IF (P2) 50,55,50
   50         SUM=SUM+P1*P2*P(JS)
   55         CONTINUE
            I1=ID(J)+I-J
            P(I1)=P(I1)-SUM
            IF (I-J) 65,70,65
   70       IF(P(KS)) 90,91,90
   90       P(KS)=1./P(KS)
   65       CONTINUE
          GOTO 92
   91     IER=1
   92     CONTINUE
      return
end subroutine decomp

!-------------------------------------------------------------------------------------------------!

subroutine doolittle(r,p,id,w,ntot)

    implicit none
          integer   :: id,ntot,i,is,js,im1,j,ii,iip1,iis
          real      :: r,p,w,sum
      dimension     :: r(1),p(1),w(1),id(1)
!***  START OF DOOLITTLE ***
!***  FORWARD SUBSTITUTION FOR SYSTEM S*W = R
          W(1)=R(1)*P(1)
          DO 130 I=2,NTOT
            IS=ID(I)
            SUM=0.
            IM1=I-1
            DO 120 J=1,IM1
              JS=ID(J)+I-J
  120       SUM=SUM+W(J)*P(JS)
  130     W(I)=(R(I)-SUM)*P(IS)
!***  BACKWARD SUBSTITUTION FOR SYSTEM T*V = W
!***  WHERE V IS STORED IN W
          DO 150 I=2,NTOT
            SUM=0.
            II=NTOT-I+1
            JS=ID(II)
            IIP1=II+1
            DO 140 J=IIP1,NTOT
              IIS=JS+J-II
  140       SUM=SUM+W(J)*P(IIS)
  150     W(II)=W(II)-SUM*P(JS)
!*** END OF DOOLITTLE ****
      return

end subroutine doolittle

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize_kriging(this)
    implicit none
    class(t_krige)                :: this

  end subroutine initialize_kriging

!-------------------------------------------------------------------------------------------------!

  function add_zonal_variogram(this, zone_name, class1, class2, opt, glo_nvario) result(zone_id)
    use m_vstring, only: t_vstring
    implicit none

    class(t_krige), intent(inout) :: this
    type(t_vstring), intent(in)   :: zone_name
    integer, intent(in)           :: class1, class2
    integer, intent(inout)        :: glo_nvario
    type(t_options),intent(in)    :: opt
    integer                       :: zone_id
    integer                       :: i, j
    integer                       :: vgm_idx, old_nzone
    integer, allocatable          :: tmp(:,:,:)

    if (.not.vstrlist_exists(this%zones)) then
      call vstrlist_new(this%zones)
      this%nzones = 0
    end if

    ! Enforce symmetry
    i = min(class1, class2)
    j = max(class1, class2)

    ! Check if zone name exists in zones
    zone_id = vstrlist_search(this%zones,zone_name)

    if (zone_id == VSTRINGLIST_INDEX_UNKNOWN) then
      call vstrlist_append(this%zones, zone_name)
      zone_id = vstrlist_length(this%zones)

      old_nzone = this%nzones
      this%nzones = zone_id

      allocate(tmp(0:this%nzones, size(this%global_vario_idx,2), size(this%global_vario_idx,3)))
      tmp = -1

      ! Copy existing data
      tmp(0:old_nzone,:,:) = this%global_vario_idx(0:old_nzone,:,:)
      tmp(zone_id,:,:) = tmp(0,:,:)

      call move_alloc(tmp, this%global_vario_idx)
    end if

    ! Increment GLOBAL T2P nvario
    glo_nvario = glo_nvario + 1

    ! Calculate new variogram index
    if (opt%ONLY_TEXTURE) then
      vgm_idx = glo_nvario
    else
      vgm_idx = glo_nvario - 1  ! Pilot Point is last
    end if

    ! Assign variogram index symmetrically
    this%global_vario_idx(zone_id, i, j) = vgm_idx
    this%global_vario_idx(zone_id, j, i) = vgm_idx
  end function add_zonal_variogram

!-------------------------------------------------------------------------------------------------!

  subroutine init_zones(this, glo_zone_names)
    use m_file_io, only: item2char
    use m_vstring, only: t_vstring, vstring_toupper
    implicit none
    class(t_krige),intent(inout)   :: this
    type(t_vstringlist),intent(in) :: glo_zone_names
    integer                        :: i, cat_id
    character(32)                  :: badzone

    ! Goal is to fill cat2zone so that the global HSUs can be mapped to the kriging object zones (HSUs)
    ! (and check that all the kriging zones passed exist!)
    do i=1, vstrlist_length(this%zones)
      cat_id = vstrlist_search(glo_zone_names, vstring_toupper(vstrlist_index(this%zones,i)))
      if (cat_id==VSTRINGLIST_INDEX_UNKNOWN) then
        call item2char(this%zones, i, badzone)
        call error_handler(1,opt_msg="Invalid HSU Variogram Used: "//badzone)
      else
        this%cat2zone(cat_id) = i
      end if
    end do

  end subroutine init_zones

!-------------------------------------------------------------------------------------------------!

  subroutine setup_krige(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
  !  ! Does everything common to all kriging
  !  ! Intended to be called by subclasses during their setup
    implicit none
    class(t_krige),intent(inout)   :: this
    class(t_dataset)               :: obs_data(:)
    integer,intent(in)             :: obs_idx(:)
    type(t_grid),intent(in)        :: to_grid
    real                           :: weights(:,:)
    integer,intent(in)             :: cat
    integer,intent(in)             :: to_cats(:)
    type(t_options),intent(in)     :: opt
    logical,intent(inout)          :: has_data
    integer                        :: i, zone_id

    ! Defaults
    this%ndim   = opt%INTERP_DIM
    this%ndrift = 0  ! Hardcoded out for now
    this%unbias = 0
    this%nsets  = size(obs_data)
    this%no_negative_weights = opt%CORRECT_WEIGHTS
    this%write_matrix = opt%WRITE_MATRIX
    has_data = .true.

    ! If MAX_SECONDARY_DIST is zero, will use 3x the largest structure range
    this%max_sdist = opt%MAX_SECONDARY_DIST

    if (allocated(this%vario_idx)) deallocate(this%vario_idx)
    allocate(this%vario_idx(this%nsets,this%nsets))
    zone_id = this%cat2zone(cat)
    this%vario_idx(:,:) = this%global_vario_idx(zone_id, obs_idx(:),obs_idx(:))  ! subset to local datasets

  end subroutine setup_krige

!-------------------------------------------------------------------------------------------------!

  subroutine filter_near_points(this, usenear, inear, dist, vidx)
    implicit none
    class(t_krige),intent(inout)  :: this
    integer, intent(inout)        :: usenear
    integer, intent(inout)        :: inear(:)
    real    , intent(inout)       :: dist(:)
    integer, intent(in)           :: vidx
    real                          :: max_sdist

    integer :: count, j

    max_sdist = this%max_sdist
    if (this%max_sdist < 0.1) max_sdist = maxval(this%variograms(:,vidx)%a_hmax) * 3.0

    count = 0
    do j = 1, usenear
      if (sqrt(dist(j)) <= max_sdist) then
        count = count + 1
        ! Move valid entries to the beginning of the array
        if (count /= j) then
          inear(count) = inear(j)
          dist(count) = dist(j)
        end if
      end if
    end do

    ! Update usenear to the new count of valid points
    usenear = count
  end subroutine filter_near_points

!-------------------------------------------------------------------------------------------------!

  subroutine write_amat(this, log_unit)
    implicit none
    class(t_krige), intent(inout) :: this
    integer, intent(in)           :: log_unit
    integer                       :: i, j, d, matsize

    matsize = size(this%rhsB, 1)
    call log%write_line("* COVARIANCE A MATRIX:")

    do i = 1, matsize
      d = i
      do j = 1, i
        if (j == 1) then
          write(log_unit, "(/,1ES16.8)", advance = 'no') this%matA(d)
        else
          write(log_unit, "(1ES16.8)", advance = 'no') this%matA(d + matsize - (j - 1))
          d = d + matsize - (j - 1)
        end if
      end do
    end do
    write(log_unit,*)
  end subroutine write_amat

!-------------------------------------------------------------------------------------------------!

  subroutine write_rhs_array(this, log_unit)
    implicit none
    class(t_krige),intent(inout)  :: this
    integer,intent(in)            :: log_unit
    integer                       :: i,matsize
    matsize = size(this%rhsB,1)

    call log%write_line("* RHS ARRAY:")
    do i =1, matsize
      write(log_unit, "(*(ES15.7))") this%rhsB(i)
    end do

  end subroutine write_rhs_array

  !-------------------------------------------------------------------------------------------------!

    subroutine write_inear(this, log_unit, obs_data, usenear, inear)
      implicit none
      class(t_krige),intent(inout)  :: this
      type(t_dataset),intent(in)    :: obs_data(:)
      integer,intent(in)            :: log_unit, usenear(:), inear(:,:)
      integer                       :: iset, i, wi
      character(len=45)             :: xyz='              x              y              z'

      wi = 0
      write(log_unit, '(A)') '   dataset     index'//xyz(1:this%ndim*15)//'         weight'
      do iset=1, this%nsets
        do i=1, usenear(iset)
          write(log_unit, "(2I10,99ES15.7)") iset, inear(i, iset), &
            obs_data(iset)%grid%coords(1:this%ndim, inear(i, iset)), &
            this%w(wi+i)
        end do
        wi = wi + usenear(iset)
      end do
    end subroutine write_inear

!-------------------------------------------------------------------------------------------------!

subroutine write_summary_krige(this)
  implicit none
  class(t_krige),intent(inout)  :: this
  integer                       :: i,j
  character(len=256)            :: line, struct
  integer, dimension(3)         :: idx

  do i=1, this%nvario
    idx = findloc(this%global_vario_idx, i)

    write(line, '(A9,I2,A6,I2,A13,I2,A1,I2,A1)') " * Vario ", i, ": HSU=", idx(1), " Class Pair=(", idx(2), ",", idx(3), ")"
    do j=1, this%nstruct(i)
      if (j==1) then
        write(struct, '(i5,2x,a)') j, trim(adjustl(this%variograms(j,i)%write_variogram()))
        call log%write_line(trim(line) // ' - ' // trim(struct))
      else
        write(struct, '(38x,i5,2x,a)') j, this%variograms(j,i)%write_variogram()
        call log%write_line(trim(struct))
      end if
    end do
  end do
end subroutine write_summary_krige

!-------------------------------------------------------------------------------------------------!

  subroutine finalize_krige(this)
    implicit none
    class(t_krige),intent(inout)  :: this

    if (associated(this%nnear)) deallocate(this%nnear)
    if (allocated(this%matA))    deallocate(this%matA)
    if (allocated(this%rhsB))    deallocate(this%rhsB)
    if (associated(this%nstruct)) deallocate(this%nstruct)
    if (associated(this%obs_idx)) deallocate(this%obs_idx)
    if (associated(this%variograms)) deallocate(this%variograms)
    if (allocated(this%global_vario_idx)) deallocate(this%global_vario_idx)
    if (allocated(this%vario_idx)) deallocate(this%vario_idx)
    if (allocated(this%w)) deallocate(this%w)
    if (allocated(this%pset_id)) deallocate(this%pset_id)

  end subroutine finalize_krige

!-------------------------------------------------------------------------------------------------!
! GLOBAL KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine setup_sk(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
    implicit none
    class(t_globkrige),intent(inout)      :: this
    class(t_dataset)                      :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:)
    type(t_grid),intent(in)               :: to_grid
    real                                  :: weights(:,:)
    integer,intent(in)                    :: cat
    integer,intent(in)                    :: to_cats(:)
    type(t_options),intent(in)            :: opt
    logical,intent(inout)                 :: has_data
    !integer,pointer           :: npoints(i)            ! Number of points (in category) to be used for simple kriging
    !integer,allocatable       :: useidx(:,:)           ! Indices of points to use for simple kriging, within category, by (npoints, nset)
    ! Local
    integer                               :: i,ier,class_id,matsize,mat_tot

    ! Call super setup
    call setup_krige(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)

    ! TODO: Could these instead be local?
    if (allocated(this%npoints)) deallocate(this%npoints)
    if (allocated(this%useidx)) deallocate(this%useidx)
    allocate(this%npoints(this%nsets))
    do i=1, this%nsets
      this%npoints = obs_data(i)%grid%n
    end do
    allocate(this%useidx(maxval(this%npoints),this%nsets))

    ! Build search trees in observed grid objects
    ! Also, figure out number of datasets in each category
    do i=1, this%nsets
      class_id = obs_idx(i)
      if (i>1) class_id = 1
      !call obs_data(i)%build_tree_by_category(cat,class_id) !this%variograms(1,i)%rotmat(1:this%ndim,1:this%ndim))
      call obs_data(i)%get_data_by_category(cat, class_id, this%useidx(:,i), this%npoints(i), obs_data(i)%grid%n)
    end do

    ! Get problem dimensions
    matsize = sum(this%npoints) + this%unbias + this%ndrift
    mat_tot = matsize * (matsize + 1) / 2

    if (allocated(this%matA)) deallocate(this%matA)
    if (allocated(this%rhsB)) deallocate(this%rhsB)
    if (allocated(this%pset_id)) deallocate(this%pset_id)
    if (allocated(this%w)) deallocate(this%w)
    allocate(this%matA(mat_tot), this%rhsB(matsize), this%pset_id(matsize), this%w(matsize))

    this%matA = 0.0
    this%w = 0.0

    call this%pset_cok(obs_data, matsize, this%matA, this%pset_id, mat_tot, this%useidx, this%npoints, this%ndim)
    if (this%write_matrix) then
      write(log%unit, '(/,A)') 'DEBUG - STARTING SIMPLE (CO)KRIGING'
      call this%write_amat(log%unit)
    end if
    call decomp(this%matA, this%pset_id, matsize, ier)
    if (ier == 1) call error_handler(4,opt_msg="Kriging Matrix Decomposition Failure. This is often caused by co-located wells")

  end subroutine setup_sk

!-------------------------------------------------------------------------------------------------!

  subroutine predict_point_sk(this, idx, p, obs_data, cat, weights, widx, ppd)
    implicit none
    class(t_globkrige),intent(inout)  :: this
    integer,intent(in)                :: idx, cat
    real    ,intent(in)               :: p(:)
    class(t_dataset)                  :: obs_data(:)
    real                              :: weights(:)
    integer, optional                 :: widx(:)              ! idx associated with weights
    integer, optional                 :: ppd(size(obs_data))  ! points per dataset
    ! Local
    integer                           :: i, j, d, rcount, start_idx, end_idx
    real                              :: value
    character(20)                     :: cidx

    this%rhsB = 0.0
    weights = 0.0
    if (this%unbias==1) this%rhsB(size(this%rhsB)) = 1.0d0

    rcount = 1
    do d = 1, this%nsets
      do i = 1, this%npoints(d)
        do j = 1, this%nstruct(this%vario_idx(1, d))
          value = this%variograms(j, this%vario_idx(1, d))%cov(p, obs_data(d)%grid%coords(:, this%useidx(i,d)))
          this%rhsB(rcount) = this%rhsB(rcount) + value
        end do
        rcount = rcount + 1
      end do
    end do

    ! Solve weights using the precomputed A matrix
    call doolittle(this%rhsB, this%matA, this%pset_id, weights, size(this%rhsB))

    ! If CORRECT_WEIGHTS, zero negative weights and rebalance
    if (this%no_negative_weights .and. minval(weights) < 0.0) then
      where (weights < 0.0) weights = 0.0
      weights = weights / sum(weights)
    end if

    ! Optional: Return observation indices
    if (present(widx)) then
        rcount = 1
        do d = 1, this%nsets
            do i = 1, this%npoints(d)
                widx(rcount) = this%useidx(i, d)
                rcount = rcount + 1
            end do
        end do
    end if

    if (present(ppd)) then
      ppd(1:) = this%npoints(1:)
    end if

    if (this%write_matrix) then
      write(log%unit, '(/,A,i0)') 'DEBUG - POINT:',idx
      write(log%unit, '(  A,9F12.3)') '  Coordinates:',p
      call this%write_rhs_array(log%unit)
    end if

  end subroutine predict_point_sk

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! TREE-BASED KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine setup_ok(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
    implicit none
    class(t_treekrige),intent(inout)       :: this
    class(t_dataset)                      :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:)
    type(t_grid),intent(in)               :: to_grid
    real                                  :: weights(:,:)
    integer,intent(in)                    :: cat
    integer,intent(in)                    :: to_cats(:)
    type(t_options),intent(in)            :: opt
    logical,intent(inout)                 :: has_data
    ! Local
    integer                     :: i,class_id,matsize,vidx, mat_tot

    ! Call super setup
    call setup_krige(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
    ! OK uses unbias
    this%unbias = 1

    ! Setup usenear for problem
    if (allocated(this%usenear)) deallocate(this%usenear)
    allocate(this%usenear(this%nsets))
    this%usenear = this%nnear(obs_idx)

    ! Build search trees in observed grid objects
    ! Also, figure out number of datasets in each category
    do i=1, this%nsets
      class_id = obs_idx(i)
      if (i>1) class_id = 1
      if (opt%ANISOTROPIC_SEARCH) then
        vidx = this%vario_idx(1,i)
        call obs_data(i)%build_tree_by_category(cat,class_id, rotmat=this%variograms(1,vidx)%rotmat(1:this%ndim,1:this%ndim), has_data=has_data)
      else
        call obs_data(i)%build_tree_by_category(cat,class_id, has_data=has_data)
      end if
      if (has_data) then
        this%usenear(i) = min(this%usenear(i), obs_data(i)%grid%tree%n)
      else
        this%usenear(i) = 0
      end if
    end do

    this%maxnear =  maxval(this%usenear)  !this%nnear(1)
    !if (this%nsets > 1) this%maxnear = this%maxnear + maxval(this%nnear)
    matsize = sum(this%usenear)+this%unbias+this%ndrift
    mat_tot = matsize * (matsize + 1) / 2

    if (allocated(this%matA)) deallocate(this%matA)
    if (allocated(this%rhsB)) deallocate(this%rhsB)
    if (allocated(this%pset_id)) deallocate(this%pset_id)
    if (allocated(this%w)) deallocate(this%w)
    allocate(this%matA(mat_tot), this%rhsB(matsize), this%pset_id(matsize), this%w(matsize))

  end subroutine setup_ok

!-------------------------------------------------------------------------------------------------!

  subroutine predict_point_ok(this, idx, p, obs_data, cat, weights, widx, ppd)
    implicit none
    class(t_treekrige),intent(inout)  :: this
    integer,intent(in)               :: idx, cat
    real    ,intent(in)              :: p(:)
    class(t_dataset)                 :: obs_data(:)
    real                             :: weights(:)
    integer, optional                :: widx(:)              ! idx associated with weights
    integer, optional                :: ppd(size(obs_data))  ! points per dataset
    ! local
    integer                          :: i, j, d, vidx, wi, osize, matsize, mat_tot, ier, rcount, woffset
    integer                          :: inear(this%maxnear,this%nsets), loc_usenear(this%nsets)
    real                             :: value
    real                             :: dist(this%maxnear,this%nsets)

    ! Get nearest for each obs dataset
    loc_usenear = this%usenear  ! LOCAL VERSION
    do i=1, this%nsets
      vidx = this%vario_idx(i,i)
      call obs_data(i)%grid%get_nnear(p, loc_usenear(i), inear(:,i), dist(:,i))
      ! Filter out points beyond max_sdist for covariate datasets
      if (i > 1) call this%filter_near_points(loc_usenear(i), inear(:,i), dist(:,i), vidx)
    end do

    if (present(ppd)) ppd = loc_usenear

    osize   = sum(loc_usenear)
    matsize = osize+this%unbias+this%ndrift
    mat_tot = matsize * (matsize + 1) / 2

    ! Create obs-obs matrix
    call this%pset_cok(obs_data, matsize, this%matA, this%pset_id, mat_tot, inear, loc_usenear, this%ndim)
    if (this%write_matrix) then
      write(log%unit, '(/,A,i0)') 'DEBUG - POINT:',idx
      write(log%unit, '(  A,9F12.3)') '  Coordinates:',p
      call this%write_amat(log%unit)
    end if
    call decomp(this%matA, this%pset_id, matsize, ier)

    if (ier == 1) call error_handler(4,opt_msg="Kriging Matrix Decomposition Failure. This is often caused by co-located wells")

    ! Create RHS
    this%rhsB = 0.d0
    this%w = 0
    if (this%unbias==1) this%rhsB(matsize) = 1.0d0
    rcount = 1
    ! Loop over datasets
    do d = 1, this%nsets
      vidx = this%vario_idx(1,d)
      ! loop over data points
      do i=1, loc_usenear(d)
        do j=1, this%nstruct(vidx)
          value = this%variograms(j,vidx)%cov(p,obs_data(d)%grid%coords(1:,inear(i,d)))
          this%rhsB(rcount) = this%rhsB(rcount) + value
        end do
        rcount = rcount + 1
      end do
    end do

    ! Solve & Out
    call doolittle(this%rhsB,this%matA,this%pset_id,this%w,matsize)

    ! If CORRECT_WEIGHTS, zero negative weights and rebalance
    if (this%no_negative_weights.and.minval(this%w(1:osize))<0.0) then
      where (this%w(1:osize) < 0.0) this%w(1:osize) = 0.0
      this%w(1:osize) = this%w(1:osize)/sum(this%w(1:osize))
    end if

    wi = 0
    woffset = 0
    if (present(widx)) then
      widx = 0
      ! just ids
      do i=1, this%nsets
        widx(woffset+1:woffset+loc_usenear(i)) = inear(1:loc_usenear(i),i)
        woffset = woffset + loc_usenear(i)
      end do
      weights(1:sum(loc_usenear)) = this%w(1:sum(loc_usenear))
    else
      do i=1, this%nsets
        weights((woffset+inear(1:loc_usenear(i),i))) = this%w(wi+1:wi+loc_usenear(i))
        wi = wi + loc_usenear(i)
        woffset = woffset + obs_data(i)%grid%n
      end do
    end if

    if (this%write_matrix) then
      call this%write_inear(log%unit, obs_data, loc_usenear, inear)
      call this%write_rhs_array(log%unit)
    end if

  end subroutine predict_point_ok


!-------------------------------------------------------------------------------------------------!

  subroutine pset_cok(this, obs_data, ntot, p, id, maxpt, inear, nnear,  dim)
    implicit none
    ! Rewrite of PSET routine for ordinary cokriging
    ! Author Leland Scantlebury
    !
    ! Copyright 2022 S.S. Papadopulos & Associates. All rights reserved.

    ! Inputs
    class(t_krige),intent(inout) :: this
    type(t_dataset)        :: obs_data(:)
    integer,intent(in)     :: maxpt, ntot, dim
    integer,intent(in)     :: inear(this%maxnear,this%nsets), nnear(this%nsets)
    integer,intent(inout)  :: id(ntot)
    real    ,intent(inout) :: p(maxpt)

    ! Subroutine variables
    integer             :: i,ii,j,jj,n,ind,JS, rowcount, nobs
    integer,allocatable :: colcount(:)
    real                :: coordi(dim),coordj(dim),value,value_sum

    nobs = this%nsets
    allocate(colcount(nobs))

    colcount = 0
    do i=1, (nobs-1)
      colcount(i+1) = colcount(i) + nnear(i)  !obs_data(i)%grid%n
    end do

!***  ZERO OUT ELEMENTS OF P-MATRIX NEEDED FOR KRIGING
    P=0.0
!***  CALCULATE THE POINTERS FOR ID
    ID(1)=1
    DO I=2,NTOT
      ID(I)=ID(I-1)+NTOT+2-I
    END DO
    ! Outer Loop over datasets (columns)
    do j=1, nobs
      ! Inner loop over datasets (rows)
      rowcount = 0
      do i=j, nobs
        ! Variogram index for section
        ind = this%vario_idx(j,i)
        ! Outer loop over column points
        do jj=1, nnear(j)
          JS=ID(colcount(j)+jj)-(colcount(j)+jj)
          coordj=obs_data(j)%grid%coords(1:,inear(jj,j))
          ! Inner loop over row points
          ii = jj
          if (i/=j) ii=1
          do while (ii <= nnear(i))
            coordi=obs_data(i)%grid%coords(1:,inear(ii,i))
            value_sum=0.0d0
            do n=1, this%nstruct(ind)
              value = this%variograms(n,ind)%cov(coordi,coordj)
              value_sum = value_sum + value
            end do
            P(JS+rowcount+colcount(j)+ii)=value_sum
            ii = ii + 1
          end do
        end do
        rowcount = rowcount + (ii-1)
      end do
    end do
    ! Fill last row with unbias
    if (this%unbias > 0) then
      DO I=1,ntot-1
        P(ID(I)+ntot-I)=1
      END DO
    end if

    deallocate(colcount)
    RETURN

  end subroutine pset_cok

!-------------------------------------------------------------------------------------------------!

  function wsize_estimate_ok(this, obs_data, obs_idx, opt)
    implicit none
    class(t_treekrige),intent(inout)  :: this
    class(t_layerdata)                   :: obs_data(:)
    integer, intent(in)                  :: obs_idx(:)
    type(t_options),intent(in)           :: opt
    integer                              :: wsize_estimate_ok
    wsize_estimate_ok = sum(this%nnear(obs_idx))
    return
  end function wsize_estimate_ok

!-------------------------------------------------------------------------------------------------!

  subroutine finalize_ok(this)
    implicit none
    class(t_treekrige),intent(inout)  :: this

    if (allocated(this%usenear)) deallocate(this%usenear)

    ! call super
    call finalize_krige(this)

  end subroutine finalize_ok

!-------------------------------------------------------------------------------------------------!

end module m_kriging