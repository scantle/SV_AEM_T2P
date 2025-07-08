module m_sgsim
  use m_global, only: t2plog=>log
  use m_interpolator, only: t_interpolator
  use m_weights, only: t_weights
  use m_grid, only: t_grid, create_grid_object
  use m_vario, only: t_vgm
  use m_datasets, only: t_dataset, t_layerdata
  use m_error_handler, only: error_handler
  use m_options, only: t_options
  use m_sparse_symmetric, only: t_sparse_sym
  use m_kriging, only: t_treekrige, decomp, doolittle, setup_krige
  use kdtree2_module, only: kdtree2_create, kdtree2, kdtree2_result, kdtree2_n_nearest
  implicit none
!-------------------------------------------------------------------------------------------------!
  type, extends(t_grid)     :: t_simgrid
    integer , pointer       :: order(:)    =>null()     ! cell visit order for SGSIM
    integer , pointer       :: order_inv(:)=>null()     ! inverse SGSIM cell visit order; result_of_random_cell(order_inv) will put back in order of the original grid
    real    , pointer       :: sample(:)   =>null()     ! random sample used in SGSIM
    real    , pointer       :: values(:)   =>null()     ! values on the random grid
    logical , pointer       :: mask(:)     =>null()     ! logical array mask the data points used for kriging
  contains
    procedure                  :: finalize => finalize_simgrid
  end type

  type, extends(t_treekrige):: t_sgsim
    ! Attributes for KD-tree and search parameters could be added here
    integer                 :: matsize
  contains
    procedure               :: setup_sgsim
    procedure               :: pset_cok_sgsim
    procedure               :: pset_rhs
    procedure               :: predict_point_sgsim
    procedure               :: write_weights
    procedure               :: calc => calc_sgsim
  end type t_sgsim


  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! ORDINARY KRIGING CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine setup_sgsim(this, obs_data, obs_idx, val_idx, to_grid, cat, to_cats, weights, opt, obsxy, gridxy)
    implicit none
    class(t_sgsim),intent(inout)          :: this
    class(t_layerdata)                    :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:), val_idx(:)
    type(t_grid),intent(in)               :: to_grid
    real                                  :: weights(:,:)
    integer,intent(in)                    :: cat
    integer,intent(in)                    :: to_cats(:)
    type(t_options),intent(in)            :: opt

    type(t_simgrid),intent(inout)         :: obsxy(:)
    type(t_simgrid),intent(inout)         :: gridxy

    ! Local
    integer                     :: i,class_id, mat_tot, success, k, addsim, vidx
    integer, allocatable        :: igrid(:)

    igrid = pack([(i,i=1,to_grid%n)], to_cats==cat)
    gridxy%n = size(igrid)
    gridxy%dim = to_grid%dim

    ! SGSIM allocation and preparation

      ! Step 4: clean up
    if (associated(gridxy%order))     deallocate(gridxy%order)
    if (associated(gridxy%order_inv)) deallocate(gridxy%order_inv)
    if (associated(gridxy%sample))    deallocate(gridxy%sample)
    if (allocated (gridxy%coords))    deallocate(gridxy%coords)

    allocate(gridxy%order    (gridxy%n),           &
             gridxy%order_inv(gridxy%n),           &
             gridxy%sample   (gridxy%n),           &
             gridxy%coords   (gridxy%dim, gridxy%n))

    ! set up the random grid
    if (opt%NSIM>0) then
      gridxy%order  = igrid(scramble(gridxy%n))
      gridxy%sample = rnorm_box_muller_vec(gridxy%n)
    else
      gridxy%order  = igrid
      gridxy%sample = 0.0
    end if
    gridxy%coords = to_grid%coords(:, gridxy%order)

    ! set up the obs
    do k = 1, this%nsets
      if (k==1 .and. opt%NSIM>0) then
        addsim=1
      else
        addsim=0
      end if
      if (allocated (obsxy(k)%coords)) deallocate(obsxy(k)%coords)
      if (associated(obsxy(k)%values)) deallocate(obsxy(k)%values)
      if (associated(obsxy(k)%mask))   deallocate(obsxy(k)%mask)

      block
        integer, allocatable     :: iobs(:)
        integer                  :: nobs
        iobs = pack([(i,i=1,obs_data(k)%grid%n)], &
                     (obs_data(k)%category==cat) .and. (obs_data(k)%values(val_idx(k),:)/=opt%NO_DATA_VALUE))
        nobs = size(iobs)
        obsxy(k)%dim = to_grid%dim
        obsxy(k)%n = nobs + gridxy%n * addsim
        allocate(obsxy(k)%values(              obsxy(k)%n))
        allocate(obsxy(k)%mask  (              obsxy(k)%n))
        allocate(obsxy(k)%coords(obsxy(k)%dim, obsxy(k)%n))
        obsxy(k)%coords(:, (gridxy%n*addsim+1):) = obs_data(k)%grid%coords(:, iobs)
        obsxy(k)%values(   (gridxy%n*addsim+1):) = obs_data(k)%values(val_idx(k), iobs)
        obsxy(k)%mask              = .true.
        if (addsim==1) then
          obsxy(k)%coords(:, :gridxy%n) = gridxy%coords
          obsxy(k)%mask  (   :gridxy%n) = .false.
        end if
        if (opt%ANISOTROPIC_SEARCH) then
          vidx = this%vario_idx(1,k)
          obsxy(k)%tree => kdtree2_create(obsxy(k)%coords, success, dim=obsxy(k)%dim, sort=.false., rearrange=.false., &
                                          rotmat=this%variograms(1,vidx)%rotmat(1:this%ndim,1:this%ndim))
        else
          obsxy(k)%tree => kdtree2_create(obsxy(k)%coords, success, dim=obsxy(k)%dim, sort=.false., rearrange=.false.)
        end if

      end block
    end do

    ! Setup usenear for problem
    if (allocated(this%usenear)) deallocate(this%usenear)
    allocate(this%usenear(this%nsets))
    this%usenear = this%nnear(obs_idx)

    this%maxnear =  maxval(this%usenear)  !this%nnear(1)
    !if (this%nsets > 1) this%maxnear = this%maxnear + maxval(this%nnear)
    this%matsize = sum(this%usenear)+this%unbias+this%ndrift

    mat_tot = this%matsize * (this%matsize + 1) / 2

    if (allocated(this%matA)) deallocate(this%matA)
    if (allocated(this%rhsB)) deallocate(this%rhsB)
    if (allocated(this%pset_id)) deallocate(this%pset_id)
    if (allocated(this%w)) deallocate(this%w)
    allocate(this%matA(mat_tot), this%rhsB(this%matsize), this%pset_id(this%matsize), this%w(this%matsize))

  end subroutine setup_sgsim

!-------------------------------------------------------------------------------------------------!

  function predict_point_sgsim(this, idx, obsxy, gridxy, nsim) result(res)
    use kdtree2_module
    implicit none
    class(t_sgsim),intent(inout)    :: this
    type(t_simgrid)                 :: obsxy(:)
    type(t_simgrid)                 :: gridxy
    integer                         :: idx, nsim

    ! local
    real                            :: res, std             ! store kriged estimate and standard deviation

    ! local
    integer                         :: iset
    integer                         :: inear(this%maxnear,this%nsets), loc_usenear(this%nsets)
    real                            :: dist(this%maxnear,this%nsets), maxdist
    type(kdtree2_result)            :: kdnearest(this%maxnear)
    real                            :: newloc(2), var0, var_reduce

    ! local
    integer                         :: j, d, vidx, wi, osize, mat_tot, ier

    ! print*, "debug predict_point_sgsim 00", idx, gridxy%order(idx)
    ! Get nearest for each obs dataset
    newloc = gridxy%coords(:, idx)
    loc_usenear = this%usenear  ! LOCAL VERSION
    maxdist = this%max_sdist**2

    do iset=1, this%nsets
      loc_usenear(iset) = min(loc_usenear(iset), count(obsxy(iset)%mask))
      call kdtree2_n_nearest(obsxy(iset)%tree, newloc, loc_usenear(iset), kdnearest(:loc_usenear(iset)), mask=obsxy(iset)%mask)
      inear(:loc_usenear(iset),iset) = kdnearest(:loc_usenear(iset))%idx
      dist (:loc_usenear(iset),iset) = kdnearest(:loc_usenear(iset))%dis

      block
        logical :: valid(loc_usenear(iset))
        integer :: nvalid
        valid = inear(:loc_usenear(iset),iset) > 0
        if (iset>1) valid = valid .and. (dist(:loc_usenear(iset),iset) < maxdist)
        nvalid = count(valid)
        inear(:nvalid,iset) = pack(inear(:loc_usenear(iset),iset), valid)
        loc_usenear(iset) = nvalid
        ! dist (:nvalid,iset) = pack(dist (:loc_usenear(iset),iset), valid)
      end block
    end do

    osize   = sum(loc_usenear)
    if (osize==0) then
      write(*,*) "No data point is found for Node", idx
      stop 1
    end if
    this%matsize = osize+this%unbias+this%ndrift
    mat_tot = this%matsize * (this%matsize + 1) / 2

    ! Create obs-obs matrix
    call this%pset_cok_sgsim(obsxy, this%matsize, this%matA, this%pset_id, mat_tot, inear, loc_usenear, this%ndim)
    if (this%write_matrix) then
      write(t2plog%unit, '(/,A,i0)') 'DEBUG - POINT:',idx
      call this%write_amat(t2plog%unit)
    end if
    call decomp(this%matA, this%pset_id, this%matsize, ier)

    if (ier == 1) then
      call error_handler(4,opt_msg="Kriging Matrix Decomposition Failure. This is often caused by co-located wells")
    end if

    call this%pset_rhs(this%matsize, obsxy, newloc, inear, loc_usenear)

    ! Solve & Out
    this%w = 0.0
    call doolittle(this%rhsB,this%matA,this%pset_id,this%w,this%matsize)

    ! calculate kriging result and standard deviation
    res = 0.0
    osize = 0
    do iset=1, this%nsets
      res = res + sum(obsxy(iset)%values(inear(1:loc_usenear(iset), iset)) * this%w(osize+1:osize+loc_usenear(iset)))
      osize = osize + loc_usenear(iset)
    end do
    var0 = 0.0
    vidx = this%vario_idx(1,1)
    do j=1, this%nstruct(vidx)
      var0 = var0 + this%variograms(j,vidx)%cov(newloc,newloc)
    end do
    var_reduce = sum(this%w(1:this%matsize) * this%rhsB(1:this%matsize))
    std = sqrt(max(var0 - var_reduce, 0.0))
    res = res + gridxy%sample(idx) * std
    res = max(0.0, min(1.0, res))
    obsxy(1)%mask(idx) = .true.

    if (this%write_matrix) then
      !call this%write_inear(t2plog%unit, obsxy, loc_usenear, inear)
      call this%write_rhs_array(t2plog%unit)
      call this%write_weights(t2plog%unit)
    end if
  end function predict_point_sgsim

  subroutine pset_cok_sgsim(this, obsxy, ntot, p, id, maxpt, inear, nnear,  dim)
    implicit none
    ! Rewrite of PSET routine for ordinary cokriging
    ! Author Leland Scantlebury
    !
    ! Copyright 2022 S.S. Papadopulos & Associates. All rights reserved.

    ! Inputs
    class(t_sgsim),intent(inout) :: this
    type(t_simgrid)       :: obsxy(:)
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
          coordj=obsxy(j)%coords(1:,inear(jj,j))
          ! Inner loop over row points
          ii = jj
          if (i/=j) ii=1
          do while (ii <= nnear(i))
            coordi=obsxy(i)%coords(1:,inear(ii,i))
            value_sum=0.0
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

  end subroutine pset_cok_sgsim

!-------------------------------------------------------------------------------------------------!

  subroutine pset_rhs(this, matsize, obsxy, newloc, inear, nnear)
    class(t_sgsim),intent(inout) :: this
    type(t_simgrid)       :: obsxy(:)
    integer,intent(in)     :: inear(this%maxnear,this%nsets), nnear(this%nsets), matsize
    real    ,intent(in)    :: newloc(:)
    ! local
    integer                :: i, j, rcount, d, vidx
    ! Create RHS
    this%rhsB = 0.0
    if (this%unbias==1) this%rhsB(matsize) = 1.0

    rcount = 1
    ! Loop over datasets
    do d = 1, this%nsets
      vidx = this%vario_idx(1,d)
      ! loop over data points
      do i=1, nnear(d)
        do j=1, this%nstruct(vidx)
          this%rhsB(rcount) = this%rhsB(rcount) + this%variograms(j,vidx)%cov(newloc, obsxy(d)%coords(:,inear(i,d)))
        end do
        rcount = rcount + 1
      end do
    end do
  end subroutine

  !-------------------------------------------------------------------------------------------------!

  subroutine calc_sgsim(this, obs_data, obs_idx, val_idx, to_grid, active, results, catlist, to_cats, opt, layer)
    use m_weights, only: t_sosa_weights
    use kdtree2_module
    implicit none
    class(t_sgsim),intent(inout)          :: this
    class(t_layerdata)                    :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:), val_idx(:)
    type(t_grid),intent(in)               :: to_grid
    logical,intent(in)                    :: active(:)    !1:to_grid%n
    real                                  :: results(:)
    integer,intent(in)                    :: catlist(:), to_cats(:), layer
    type(t_options),intent(in)            :: opt
    logical                               :: has_data

    ! local
    integer                               :: i, j, iset, cat,  nobs1
    real    , allocatable                 :: weights(:,:)

    ! Call super setup
    call setup_krige(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)

    ! OK uses unbias
    this%unbias = 1

    do i=1, size(catlist)
      ! Setup for each category
      cat = catlist(i)
      block
        type(t_simgrid)  :: obsxy(this%nsets), gridxy
        ! Step 1: filter obs and grid to
        call this%setup_sgsim(obs_data, obs_idx, val_idx, to_grid, cat, to_cats, weights, opt, obsxy, gridxy)
        ! Step 2: estimate values
        do j=1, gridxy%n
          if (.not.active(j)) cycle
          ! Interpolate for each point
          obsxy(1)%values(j) = this%predict_point_sgsim(j, obsxy, gridxy, opt%NSIM)
        end do
        ! Step 3: put back the values
        results(gridxy%order) = obsxy(1)%values(:gridxy%n)
        ! Step 4: clean up
        !call gridxy%finalize()
        !do iset=1, this%nsets; call obsxy(iset)%finalize(); end do
      end block
    end do

  end subroutine calc_sgsim

  !-------------------------------------------------------------------------------------------------!

    subroutine write_weights(this, log_unit)
      implicit none
      class(t_sgsim),intent(inout)  :: this
      integer,intent(in)            :: log_unit
      integer                       :: i

      call t2plog%write_line("* WEIGHTS:")
      do i =1, this%matsize
        write(log_unit, "(*(ES15.7))") this%w(i)
      end do

    end subroutine write_weights

!-------------------------------------------------------------------------------------------------!
  subroutine finalize_simgrid(this)
    class(t_simgrid),intent(inout)  :: this
    if (associated(this%order))      deallocate(this%order)
    if (associated(this%order_inv))  deallocate(this%order_inv)
    if (associated(this%sample))     deallocate(this%sample)
    if (associated(this%mask))       deallocate(this%mask)
    if (associated(this%values))     deallocate(this%values)
    if (allocated (this%coords))     deallocate(this%coords)
    if (allocated (this%true_idx))   deallocate(this%true_idx)
    if (associated(this%tree))       deallocate(this%tree)
  end subroutine

  function scramble( number_of_values ) result(array)
    integer,intent(in)    :: number_of_values
    integer,allocatable   :: array(:)
    integer               :: l, j, k, m, n
    integer               :: temp
    real                  :: u

    array=[(l,l=1,number_of_values)]

    ! The intrinsic RANDOM_NUMBER(3f) returns a real number (or an array
    ! of such) from the uniform distribution over the interval [0,1). (ie.
    ! it includes 0 but not 1.).
    !
    ! To have a discrete uniform distribution on
    ! the integers {n, n+1, ..., m-1, m} carve the continuous distribution
    ! up into m+1-n equal sized chunks, mapping each chunk to an integer.
    !
    ! One way is:
    !   call random_number(u)
    !   j = n + FLOOR((m+1-n)*u)  ! choose one from m-n+1 integers

    n=1
    m=number_of_values
    do k=1,2
        do l=1,m
          call random_number(u)
          j = n + FLOOR((m+1-n)*u)
          ! switch values
          temp=array(j)
          array(j)=array(l)
          array(l)=temp
        enddo
    enddo
  end function scramble

  function rnorm_box_muller_vec(n,mu,sigma) result(variates)
    ! https://fortran-lang.discourse.group/t/normal-random-number-generator/3724/7
    integer      , intent(in)           :: n     ! # of normal variates
    real         , intent(in), optional :: mu    ! target mean
    real         , intent(in), optional :: sigma ! target standard deviation
    real                                :: variates(n) ! normal variates
    integer                             :: i,j
    logical                             :: n_odd
    n_odd = mod(n,2) /= 0
    do i=1,n/2
        j = 2*i - 1
        variates(j:j+1) = rnorm_box_muller()
    end do
    if (n_odd) variates(n) = rnorm_box_muller_single_variate()
    if (present(sigma)) variates = sigma*variates
    if (present(mu)) variates = variates + mu
  end function rnorm_box_muller_vec

    !
  function rnorm_box_muller() result(variates) ! coded formulas from https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
    ! return two uncorrelated standard normal variates
    real          :: variates(2)
    real          :: u(2), factor, arg
    real         , parameter  :: two_pi=8.0*atan(1.0)
    do
        call random_number(u)
        if (u(1) > 0.0) exit
    end do
    factor = sqrt(-2 * log(u(1)))
    arg = two_pi*u(2)
    variates = factor * [cos(arg),sin(arg)]
  end function rnorm_box_muller
    !
  function rnorm_box_muller_single_variate() result(variate)
    ! return a standard normal variate
    real          :: variate
    real          :: u(2), factor, arg
    real         , parameter  :: two_pi=8.0*atan(1.0)
    call random_number(u)
    factor = sqrt(-2 * log(u(1)))
    arg = two_pi*u(2)
    variate = factor * cos(arg)
  end function rnorm_box_muller_single_variate

end module m_sgsim