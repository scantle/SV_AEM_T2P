module m_interpolator
  use m_global, only: log
  use m_weights, only: t_weights
  use m_grid, only: t_grid, create_grid_object
  use m_datasets, only: t_dataset, t_layerdata
  use m_error_handler, only: error_handler
  use m_options, only: t_options
  use m_vario, only: t_vgm
  use m_categories, only: t_category
  implicit none
!-------------------------------------------------------------------------------------------------!
! ADDING A NEW INTERPOLATOR
!
! Interpolators must override these methods:
!  - setup(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt)
!  - predict_point(this, idx, p, obs_data, cat, weights, widx, ppd)
!
! Optionally, they can also override these:
!  - wsize_estimate(this, obs_data, obs_idx)
!  - finalize(this)
!
! `setup` is called before each loop over a set of points in a given category (pp zone, HSU).
! The intent is for it to be used to allocate arrays, etc.
!
! `predict_point` is called for each point on the output grid (MF, IWFM, GSF...). The intent
! is to return a series of a weights for each input observation (pp or class data). In the
! interpolate subroutine `calc_weights` it returns weights for the entire observation set.
! In `calc` however (currently used for class interpolation) a sparse weight representation
! is used; the predict_point() optional arguments ppd and widx are used to return the number
! of observations and the observation indices, respectively.
!
! `wsize_estimator` returns the maximum size of the weight array (max obs points used)
! `finalize` deallocates arrays and nullifies pointers
!
! The arguments of these methods are documented in their abstract base class placeholders
! below.
!
! To add the new interpolator as a valid option in the input file, add a new case
! statement to T2P.f90 in the `process_options` method. Their the interpolator can be setup
! to be allocated as the correct subclass.
!
! If your interpolator has additional options that need to be included via the input file,
! their are a couple possible routes. New options can be added to the options class in
! options.f90, and then when the interpolator `setup()` method is called it will have access
! to the new options. If the interpolator requires it's own block in the input file (as
! kriging did with variograms) you'll need to:
!  1. Add a new block in the main input file with a new reader in the Read_Main_Input.f90
!     file
!  2. Setup the OPTIONS block to be required before your new block in `pre_block_read_check()`
!  3. Write a subroutine in T2P to be called after your block is read. Pass the necessary
!     data or options to t2p%interp_pp and t2p%interp_data - see setup_variograms in T2P.f90
! 	for an example
! Other less drastic options include:
!  1. Adding an input file to the options block your `setup()` routine can read
!     Downside: `setup()` can't distinguish between pp and data interpolation easily
!  2. Modify the interpolator `initialize()` routine to accept your parameter as an input.
!     Override `initialize()` for your interpolator so it properly handles the parameter
! 	The `initialize()` calls are currently in `process_options()` but could be moved
! 	to run after everything is read - they currently do nothing!
!-------------------------------------------------------------------------------------------------!
  type, abstract:: t_interpolator

  ! Abstract Base Class
  contains
    procedure,private      :: initialize_abc
    generic                :: initialize => initialize_abc       ! placeholder
    procedure,public       :: calc_weights
    procedure,public       :: calc
    procedure,private      :: single_point_full_weight
    procedure              :: setup
    procedure              :: predict_point
    procedure              :: wsize_estimate
    procedure              :: write_summary
    procedure              :: finalize
  end type t_interpolator
!-------------------------------------------------------------------------------------------------!

  type, extends(t_interpolator) :: t_idw
    real                   :: power
    integer                :: nobs
    logical,allocatable    :: catmask(:)

  contains
    procedure              :: setup => setup_idw
    procedure              :: predict_point => predict_point_idw
    procedure              :: finalize => finalize_idw
  end type t_idw

!-------------------------------------------------------------------------------------------------!
! Kriging class/routines are located in kriging.f90. They inheret from Interpolator
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!

  function dist(point1, point2) result(distance)
    real    , intent(in) :: point1(:), point2(:)  ! Input: Coordinates of the two points
    real                 :: distance              ! Output: Euclidean distance

    distance = sqrt(sum((point2 - point1)**2))
  end function dist

!-------------------------------------------------------------------------------------------------!

  function dist_vector(p, point_array) result(distances)
    real    , intent(in)                      :: p(:)
    real    , dimension(:, :), intent(in)     :: point_array
    real    , dimension(size(point_array, 2)) :: distances
    integer                                   :: ii
    distances = 0.0
    do ii=1, size(p)
      distances = distances + (p(ii)-point_array(ii,:))**2
    end do
    distances = sqrt(distances)
  end function dist_vector

!-------------------------------------------------------------------------------------------------!
! BASE CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

  subroutine initialize_abc(this)
    implicit none
    class(t_interpolator), intent(in)     :: this

  end subroutine initialize_abc

!-------------------------------------------------------------------------------------------------!

  subroutine calc_weights(this, obs_data, obs_idx, to_grid, active, weights, catlist, to_cats, opt)
    implicit none
    class(t_interpolator),intent(inout)   :: this
    class(t_dataset)                      :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:)
    type(t_grid),intent(in)               :: to_grid
    logical,intent(in)                    :: active(:)    !1:to_grid%n
    real                                  :: weights(:,:) !(output_points, input_points)
    integer,intent(in)                    :: catlist(:), to_cats(:)
    type(t_options),intent(in)            :: opt
    integer                               :: i, j, cat, count_cat
    logical                               :: has_data

    do i=1, size(catlist)
      ! Setup for each category
      cat = catlist(i)
      count_cat = 0
      ! Count occurrences across datasets
      do j = 1, size(obs_data)
        count_cat = count_cat + count(obs_data(j)%category == cat)
        if (count_cat > 1) exit
      end do

      if (count_cat>1) then ! more than one obs point
        call this%setup(obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
        if (.not.has_data) then
          ! TODO - setup runtime warnings through Error Handler
          write(*,'(a,i0)') 'Warning - no data for pp in this array for category ', cat
          write(log%unit,'(a,i0)') 'Warning - no data for pp in this array for category ', cat
          cycle
        end if
        do j=1, to_grid%n
          if (.not.active(j)) cycle
          ! Interpolate for each point
          if (to_cats(j)==cat) call this%predict_point(j, to_grid%coords(:,j), obs_data, cat, weights(j,:))
        end do
      else  ! only one point!
        call this%single_point_full_weight(obs_data, cat, to_cats, weights)
      end if
    end do

  end subroutine calc_weights

!-------------------------------------------------------------------------------------------------!

  subroutine calc(this, obs_data, obs_idx, val_idx, to_grid, active, results, catlist, to_cats, opt, layer)
    use m_weights, only: t_sosa_weights
    implicit none
    class(t_interpolator),intent(inout)   :: this
    class(t_layerdata)                    :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:), val_idx(:)
    type(t_grid),intent(in)               :: to_grid
    real                                  :: results(:)
    logical,intent(in)                    :: active(:)    !1:to_grid%n
    integer,intent(in)                    :: catlist(:), to_cats(:)
    type(t_options),intent(in)            :: opt
    integer,intent(in)                    :: layer
    type(t_sosa_weights)                  :: weights
    integer                               :: i, j, k, woffset, cat, wlen
    real                                  :: fake_weights(1,1), v_adjust
    !integer                               :: ppd(size(obs_data))  !LS moved to sosa weights
    character(100)                        :: wtfile
    logical                               :: has_data
    
    ! TODO need to move all weight-related operations to separate routines. This is too messy.

    results = 0.0  ! necessary?
    call weights%initialize(this%wsize_estimate(obs_data, obs_idx, opt), size(obs_data))
    
    ! Open weights file if using (call does nothing if options are false)
    write(wtfile, '(A,I0,A,I0,A)') 't2p_grdweights_arr', layer, '_obs', obs_idx(1), '.wts'
    call weights%open_file(trim(wtfile), opt%WRITE_GRID_WEIGHTS, opt%READ_GRID_WEIGHTS, opt%GRID_WEIGHTS_TEXT)
    
    if (opt%READ_GRID_WEIGHTS) then
      ! Read in weights by grid point
      do i=1, to_grid%n  ! tracks progress through file, may not match grid id! Use j, as below!
        if (.not.active(i)) cycle ! makes sure it only reads for the # of active cells
        ! Read in one grid point of data
        call weights%load_from_file(j)
        ! Apply weight
        woffset = 0
        do k=1, size(obs_data)
          v_adjust = 0.0
          if (opt%INTERP_METHOD=="SK") then
            v_adjust = -1 * obs_data(k)%mean(val_idx(k))                              ! Subtract out mean
          else if (k>1) then
            v_adjust = obs_data(1)%mean(val_idx(1)) - obs_data(k)%mean(1)             ! Primary - Secondary Means
          end if
          results(j) = results(j) + dot_product(weights%values(woffset+1:woffset+weights%ppd(k)), obs_data(k)%values(val_idx(k), weights%idx(woffset+1:woffset+weights%ppd(k)))+v_adjust)
          woffset = woffset + weights%ppd(k)
        end do
        
      end do
      
    else  ! Calculating, not reading
      do i=1, size(catlist)
        ! Setup for each category
        cat = catlist(i)
        call this%setup(obs_data, obs_idx, to_grid, cat, to_cats, fake_weights, opt, has_data)
        
        if (.not.has_data) then
          ! TODO - setup runtime warnings through Error Handler
          write(*,'(2(a,i0))') 'Warning - no data for layer ', layer, ' in category ', cat
          write(log%unit,'(2(a,i0))') 'Warning - no data for layer ', layer, ' in category ', cat
          cycle
        end if

        ! Calculate weights
        do j=1, to_grid%n
          if (.not.active(j)) cycle  ! does not predict or write weights for inactive cells
          ! Interpolate for each point
          if (to_cats(j)==cat) then
            call this%predict_point(j, to_grid%coords(:,j), obs_data, cat, weights%values, weights%idx, weights%ppd)
            if (opt%WRITE_GRID_WEIGHTS) then
              call weights%save_to_file(j)
            end if
            ! Calculate for each point
            woffset = 0
            do k=1, size(obs_data)
              v_adjust = 0.0
              if (opt%INTERP_METHOD=="SK") then
                v_adjust = -1 * obs_data(k)%mean(val_idx(k))                              ! Subtract out mean
              else if (k>1) then
                v_adjust = obs_data(1)%mean(val_idx(1)) - obs_data(k)%mean(1)             ! Primary - Secondary Means
              end if
              results(j) = results(j) + dot_product(weights%values(woffset+1:woffset+weights%ppd(k)), obs_data(k)%values(val_idx(k), weights%idx(woffset+1:woffset+weights%ppd(k)))+v_adjust)
              woffset = woffset + weights%ppd(k)
            end do
            ! For SK, add back in mean
            if (opt%INTERP_METHOD == "SK") results(j) = results(j) + obs_data(1)%mean(val_idx(1))     ! Add back in primary mean
          end if
        end do
      end do
     end if
      
    if (opt%WRITE_GRID_WEIGHTS.or.opt%READ_GRID_WEIGHTS) call weights%close_file()
   
  end subroutine calc

!-------------------------------------------------------------------------------------------------!
subroutine single_point_full_weight(this, obs_data, cat, to_cats, weights)
  implicit none
  class(t_interpolator), intent(inout) :: this
  class(t_dataset)                     :: obs_data(:)
  real                                 :: weights(:,:)
  integer, intent(in)                  :: cat, to_cats(:)
  integer                              :: i, j, k, obs_idx, w_offset

  w_offset = 0

  ! Loop over datasets to find the index of the ASSUMED SINGLE observation that matches cat
  do i = 1, size(obs_data)
    obs_idx = findloc(obs_data(i)%category, cat, dim=1)
    if (obs_idx > 0) then  ! If found in dataset 'i'
      obs_idx = obs_idx + w_offset
      ! set weights for the found observation
      do k = 1, size(to_cats)
        if (to_cats(k) == cat) weights(k, obs_idx) = 1.0
      end do
      exit
    endif
    w_offset = w_offset + obs_data(i)%grid%n
  end do
end subroutine single_point_full_weight

!-------------------------------------------------------------------------------------------------!
!> Initializes the interpolator before each loop over a set of points in a given category.
!! This is typically used to allocate necessary arrays and set up the environment.
!!
!! @param this The interpolator object.
!! @param obs_data Observed datasets to be used for interpolation.
!! @param obs_idx Indices of the observed data points.
!! @param to_grid Grid object where the interpolated values will be placed.
!! @param cat Category data.
!! @param to_cats Categories for the target grid points.
!! @param weights Weights to be calculated during interpolation.
!! @param opt T2P Options object.
!! @param has_data Logical, was there no data for this category/layer/data combo??
  subroutine setup(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
    ! Abstract Base Class method - should not be called
    implicit none
    class(t_interpolator),intent(inout)   :: this
    class(t_dataset)                      :: obs_data(:)
    integer,intent(in)                    :: obs_idx(:)
    type(t_grid),intent(in)               :: to_grid
    real                                  :: weights(:,:)
    integer,intent(in)                    :: cat
    integer,intent(in)                    :: to_cats(:)
    type(t_options),intent(in)            :: opt
    logical,intent(inout)                 :: has_data

  end subroutine setup

!-------------------------------------------------------------------------------------------------!
!> Predicts the value at a specific point on the output grid based on the observed data and weights.
!! This is called for each point on the output grid to compute the interpolation weights.
!!
!! @param this The interpolator object.
!! @param idx Index of the point to predict.
!! @param p Coordinates of the point to predict.
!! @param obs_data Observed data used for prediction.
!! @param cat Category data.
!! @param weights Weights for the prediction.
!! @param widx Indices of the weights (optional).
!! @param ppd Number of observations (optional).
  subroutine predict_point(this, idx, p, obs_data, cat, weights, widx, ppd)
    ! Abstract Base Class method - should not be called
    implicit none
    class(t_interpolator),intent(inout)  :: this
    integer,intent(in)                   :: idx, cat
    real    ,intent(in)                  :: p(:)
    class(t_dataset)                     :: obs_data(:)
    real                                 :: weights(:)
    integer, optional                    :: widx(:)              ! idx associated with weights
    integer, optional                    :: ppd(size(obs_data))  ! points per dataset

  end subroutine predict_point

!-------------------------------------------------------------------------------------------------!
!> Estimates the maximum size of the weight array for the interpolation.
!! This subroutine returns the maximum number of observation points used.
!!
!! @param this The interpolator object.
!! @param obs_data Observed data used for interpolation.
!! @param obs_idx Indices of the observed data points.
!! @return Maximum size of the weight array.
  function wsize_estimate(this, obs_data, obs_idx, opt)
    ! To be overwritten where necessary
    implicit none
    class(t_interpolator),intent(inout)  :: this
    class(t_layerdata)                   :: obs_data(:)
    integer,intent(in)                   :: obs_idx(:)
    type(t_options),intent(in)           :: opt
    integer                              :: i,wsize_estimate
    wsize_estimate = 0
    do i=1, size(obs_data)
      wsize_estimate = wsize_estimate + obs_data(i)%grid%n
    end do
    return
  end function wsize_estimate

!-------------------------------------------------------------------------------------------------!
!> Writes any parameter data necessary to the log file
!! (Generally for kriging routines to write out variograms)
!!
!! @param this The interpolator object.
  subroutine write_summary(this)
    ! To be overwritten where necessary
    implicit none
    class(t_interpolator),intent(inout)  :: this
  
  end subroutine write_summary

!-------------------------------------------------------------------------------------------------!
  
  subroutine finalize(this)
    implicit none
    class(t_interpolator),intent(inout)  :: this
    ! Nothing for the base class
  end subroutine finalize

!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!
! INVERSE DISTANCE WEIGHTING (IDW) CLASS TYPE-BOUND PROCEDURES
!-------------------------------------------------------------------------------------------------!

!-------------------------------------------------------------------------------------------------!

  subroutine setup_idw(this, obs_data, obs_idx, to_grid, cat, to_cats, weights, opt, has_data)
    use m_global, only             : NODATA
    implicit none
    class(t_idw),intent(inout)    :: this
    class(t_dataset)              :: obs_data(:)
    integer,intent(in)            :: obs_idx(:)
    type(t_grid),intent(in)       :: to_grid
    real                          :: weights(:,:)
    integer,intent(in)            :: cat
    integer,intent(in)            :: to_cats(:)
    type(t_options),intent(in)    :: opt
    logical,intent(inout)         :: has_data
    type(t_layerdata),pointer     :: p
    logical                       :: na_mask(obs_data(1)%grid%n)

    this%nobs = obs_data(1)%grid%n
    this%power = opt%IDW_POWER
    na_mask = .true.
    has_data = .false.

    allocate(this%catmask(this%nobs))
    ! For layer dataset, ignore all NA values
    select type(p=>obs_data(1))
      class is (t_layerdata)
        na_mask = .not. any(p%values(1:, :) == NODATA, dim=1)
    end select
    this%catmask = (obs_data(1)%category == cat) .and. (na_mask)
    if (count(this%catmask)>1) has_data = .true.
  end subroutine setup_idw

!-------------------------------------------------------------------------------------------------!

  subroutine predict_point_idw(this, idx, p, obs_data, cat, weights, widx, ppd)
    implicit none
    class(t_idw),intent(inout)    :: this
    integer,intent(in)            :: idx, cat
    real    ,intent(in)           :: p(:)
    class(t_dataset)              :: obs_data(:)
    real                          :: weights(:)
    real                          :: distances(this%nobs)
    real                          :: total_weight
    integer                       :: zero_count, i
    integer, optional             :: widx(:)              ! idx associated with weights
    integer, optional             :: ppd(size(obs_data))  ! points per dataset

    ! Calculate distances for points matching the mask
    distances = huge(0.0d0)
    block
      integer, allocatable          :: ii(:)
      ii = pack([(i,i=1,this%nobs)], this%catmask)
      distances(ii) = dist_vector(p, obs_data(1)%grid%coords(:, ii))
    end block
    where(this%catmask)
      weights = 1.0 / distances**this%power
    elsewhere
      weights = 0.0
    end where

     ! Check if there are any zero distances
    zero_count = count(distances == 0.0)
    if (zero_count > 0) then
      ! Assign equal weight to all zero-distance points (TODO should this produce a warning?)
      weights(:) = 0.0
      where (distances == 0.0)
        weights = 1.0 / zero_count
      end where
    else
      ! Normalize weights to sum up to 1
      total_weight = sum(weights)
      weights = weights / total_weight
    end if

    ! Handle widx & ppd presence
    if (present(widx)) then
      ! todo oneliner?
      do i=1,obs_data(1)%grid%n
        widx = i
      end do
    end if
    if (present(ppd)) then
      do i=1,size(obs_data)
        ppd(i) = obs_data(i)%grid%n
      end do
    end if

  end subroutine predict_point_idw
!-------------------------------------------------------------------------------------------------!

  subroutine finalize_idw(this)
    implicit none
    class(t_idw),intent(inout)  :: this

    if (allocated(this%catmask)) deallocate(this%catmask)
  end subroutine finalize_idw

!-------------------------------------------------------------------------------------------------!
end module m_interpolator