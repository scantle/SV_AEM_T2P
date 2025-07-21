MODULE define_fields

IMPLICIT NONE

TYPE polygon
  INTEGER      :: SWBM_id, SWBM_LU, subws_ID, irr_type, well_idx, water_source, well_id
  INTEGER      :: nModelCells, landcover_id, irr_flag, WL2CP_year, runoff_ISEG
  REAL*8       :: area, whc, init_fill_frac, max_infil_rate, precip_fact, mar_depth, curtail_frac, et_cor
  LOGICAL      :: Irrigating, ILR, ILR_Active
END TYPE

TYPE accumulator
	INTEGER :: ET_active  !daydef
	REAL*8 :: tot_irr, gw_irr, recharge, swc, pET, aET, deficiency, change_in_storage
  REAL*8 :: effprecip, residual, tot_irr_vol, recharge_vol, gw_irr_vol, swc_vol!,MAR
	REAL*8 :: pET_vol, aET_vol, deficiency_vol, change_in_storage_vol, effprecip_vol!, MAR_vol
	REAL*8 :: runoff, runoff_vol, mar_depth

  contains
    procedure :: init  => init_accumulator
END TYPE

TYPE well
    INTEGER :: layer, well_row, well_col, well_id
    REAL :: coordx, coordy, top_scrn_z, bot_scrn_z
    REAL :: daily_vol, monthly_vol, annual_vol, monthly_rate, specified_volume, specified_rate
    CHARACTER(50) :: well_name
END TYPE

TYPE crop_table
    INTEGER :: landcover_id, IrrMonthStart, IrrDayStart, IrrMonthEnd, IrrDayEnd
    REAL    :: IrrSWC, RootDepth, irreff_flood, irreff_wl, irreff_cp, daily_Kc, Kc_mult, rd_Mult
    CHARACTER(50) :: landcover_name
    LOGICAL :: irrigated, ET
END TYPE

TYPE surface_water
    REAL*8 :: inflow_irr(31), inflow_nonirr(31), sw_irr, avail_sw_vol
END TYPE

TYPE subws_flow_partitioning
    INTEGER :: subws_ID, SFR_segment
    REAL :: frac_subws_flow(31)
    CHARACTER(50) :: subwsName, streamName
END TYPE

TYPE Stream_Segments
    INTEGER :: NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, tabunit
    REAL*8 :: FLOW, WIDTH1, WIDTH2, MANNING_N, RUNOFF, FLOW_DAILY(31)
    CHARACTER(12) :: Bed_K_Param, Manning_n_Param
END TYPE

TYPE(polygon), ALLOCATABLE, DIMENSION(:) :: fields
TYPE(accumulator), ALLOCATABLE, DIMENSION(:):: previous, monthly, daily, yearly
TYPE(well), ALLOCATABLE, DIMENSION(:) :: ag_wells, spec_wells
TYPE(crop_table), ALLOCATABLE, DIMENSION(:) :: crops
TYPE(surface_water), ALLOCATABLE, DIMENSION(:) :: surfaceWater
TYPE(subws_flow_partitioning), ALLOCATABLE, DIMENSION(:) :: SFR_allocation
TYPE(Stream_Segments), ALLOCATABLE, DIMENSION(:) :: SFR_Routing
INTEGER :: npoly, nrotations, nAgWells, nSpecWells, ip, nlandcover
REAL :: ann_spec_well_vol

contains

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE readpoly(npoly, nrows, ncols, rch_zones)

  INTEGER, INTENT(IN):: npoly, nrows, ncols
  INTEGER, DIMENSION(nrows, ncols), INTENT(IN)  :: rch_zones
  INTEGER :: i, dummy
  INTEGER, DIMENSION(nrows, ncols)  :: dummy_mat


  ALLOCATE(fields(npoly), monthly(npoly), daily(npoly), yearly(npoly), previous(npoly))
  ! Ensure accumulators start at 0
  do i=1, npoly
    call daily   (i)%init()
    call previous(i)%init()
    call monthly (i)%init()
    call yearly  (i)%init()
  end do

  open(unit=10,file="polygons_table.txt",status="old")
  read(10,*)       ! read headers into nothing
  write(800,*)'SWBM_ID subws_ID SWBM_LU SWBM_IRR area &
    &Water_Src whc init_fill_frac WL2CP_year' ! ILR'
  open(unit=11,file="precip_factors.txt",status="old")
  read(11,*) ! read headers into nothing

    do i=1, npoly
      read(10,*)fields(i)%SWBM_id, fields(i)%subws_ID, fields(i)%SWBM_LU, &
        fields(i)%irr_type, fields(i)%area, fields(i)%water_source, fields(i)%whc, &
        fields(i)%init_fill_frac, fields(i)%max_infil_rate, fields(i)%runoff_ISEG,&
        fields(i)%WL2CP_year , fields(i)%ILR
        if (fields(i)%irr_type == 999) then
          fields(i)%irr_type = 2                ! Change unknown irrigation type to wheel line
        end if
        if (fields(i)%irr_type == 555) then       ! Change non-irrigated field to dry irrigation type (to match old SWBM - LS)
           fields(i)%water_source = 5
        endif
        if (fields(i)%water_source == 999) then
          !fields(i)%irr_type = 2                ! Change unknown water source to groundwater
          fields(i)%water_source = 2                ! Change unknown water source to groundwater
        end if
      read(11,*)dummy, fields(i)%precip_fact
      dummy_mat = 0
      where (rch_zones(:,:) == i)
        dummy_mat(:,:) = 1
      end where
      fields(i)%nModelCells = SUM(dummy_mat)
      write(800,'(i6,i3,i4,i5,f20.7,i5,f20.5,f20.5,i6,l3)')i ,fields(i)%subws_ID, fields(i)%SWBM_LU, &
      fields(i)%irr_type, fields(i)%area, fields(i)%water_source, fields(i)%whc, fields(i)%init_fill_frac, &
      fields(i)%WL2CP_year , fields(i)%ILR
    enddo
  close(10)
  close(11)
end subroutine readpoly

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE read_landcover_table(nlandcover)

  INTEGER, INTENT(IN) :: nlandcover
  INTEGER :: i

  ALLOCATE(crops(nlandcover))

  open(unit=19, file = 'landcover_table.txt', status = 'old')
  read(19,*) ! read header into nothing
  do i=1, nlandcover
    read(19,*) crops(i)%landcover_id, crops(i)%landcover_name, crops(i)%irrigated, crops(i)%ET, &
    crops(i)%IrrSWC, crops(i)%IrrMonthStart, crops(i)%IrrDayStart, crops(i)%IrrMonthEnd, crops(i)%IrrDayEnd, &
    crops(i)%RootDepth, crops(i)%rd_mult, crops(i)%irreff_flood, crops(i)%irreff_wl, crops(i)%irreff_cp, &
    crops(i)%kc_mult
  enddo

  ! Adjust root depths by multiplier
  crops(:)%RootDepth = crops(:)%RootDepth * crops(:)%rd_mult

END SUBROUTINE read_landcover_table

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

subroutine init_accumulator(this)
  ! Just to ensure all values start at zero
  class(accumulator)   :: this

  this%ET_active             = 0
	this%tot_irr               = 0.0d0
  this%gw_irr                = 0.0d0
  this%recharge              = 0.0d0
  this%swc                   = 0.0d0
  this%pET                   = 0.0d0
  this%aET                   = 0.0d0
  this%deficiency            = 0.0d0
  this%change_in_storage     = 0.0d0
  this%effprecip             = 0.0d0
  this%residual              = 0.0d0
  this%tot_irr_vol           = 0.0d0
  this%recharge_vol          = 0.0d0
  this%gw_irr_vol            = 0.0d0
  this%swc_vol               = 0.0d0
	this%pET_vol               = 0.0d0
  this%aET_vol               = 0.0d0
  this%deficiency_vol        = 0.0d0
  this%change_in_storage_vol = 0.0d0
  this%effprecip_vol         = 0.0d0
	this%runoff                = 0.0d0
  this%runoff_vol            = 0.0d0
  this%mar_depth             = 0.0d0

end subroutine init_accumulator

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE initialize_wells(npoly, nAgWells, nSpecWells)
  use m_global, only: agwell_locs_file, poly_agwell_file, specwell_locs_file, specwell_vol_file

  INTEGER, INTENT(IN) :: npoly, nAgWells, nSpecWells
  INTEGER, DIMENSION(nAgWells) :: ag_well_id
  INTEGER, DIMENSION(nSpecWells) :: spec_well_id
  INTEGER :: i, j, poly_id, well_id
  CHARACTER(20) :: dummy

  ALLOCATE(ag_wells(nAgWells))
  ALLOCATE(spec_wells(nSpecWells))

  open(unit=10, file=agwell_locs_file, status="old")
  read(10,*)
  !write(*,*) nAgWells
  do i=1, nAgWells
    read(10,*) ag_wells(i)%well_id, ag_wells(i)%well_name, ag_wells(i)%layer,&
      ag_wells(i)%top_scrn_z, ag_wells(i)%bot_scrn_z,&
      ag_wells(i)%well_row, ag_wells(i)%well_col, ag_wells(i)%coordx, ag_wells(i)%coordy
      ag_wells(i)%well_name = trim(ag_wells(i)%well_name)
  enddo
  close(10)
  fields%well_id = 0
  fields%well_idx = 0
  
  open(unit=10, file=poly_agwell_file, status="old")
  read(10,*)  ! read header into nothing
  write(800,*)'SWBM_ID  Well_ID  WELL_IDX'
  do i = 1, npoly
    read(10,*) poly_id, well_id
    do j = 1, nAgWells
      if (poly_id==fields(i)%SWBM_id) fields(i)%well_id = well_id
      if (well_id==ag_wells(j)%well_id) fields(i)%well_idx = j
    enddo
  write(800,'(3i8)') fields(i)%SWBM_id, fields(i)%well_id, fields(i)%well_idx
  enddo
  close(10)
  
  ! If specified wells are active
  if (nSpecWells>0) then
    ! Open specified well rate file, just read columns to check order
    open(unit=539, file=specwell_vol_file, status="old")
    read(539,*)  dummy, spec_well_id(:)
    
    ! Open specified well locations file, read by line & ensure order matches
    open(unit=10, file=specwell_locs_file, status="old")
    read(10,*)  ! Skip header
    do i=1, nSpecWells
      read(10,*) spec_wells(i)%well_id, spec_wells(i)%well_name, spec_wells(i)%layer, spec_wells(i)%top_scrn_z, spec_wells(i)%bot_scrn_z,&
      spec_wells(i)%well_row, spec_wells(i)%well_col, spec_wells(i)%coordx, spec_wells(i)%coordy
      spec_wells(i)%well_name = trim(spec_wells(i)%well_name)
      if (spec_wells(i)%well_id /= spec_well_id(i)) then
    	  write(*,*) "Ordering of specified wells in "// specwell_locs_file //" differes from that in "// specwell_vol_file
    	  write(800,*) "Ordering of specified wells in "// specwell_locs_file //" differes from that in "// specwell_vol_file
      call EXIT
      endif
    enddo
    close(10)
    spec_wells%specified_volume = 0.0
    spec_wells%specified_rate   = 0.0
  end if
END subroutine initialize_wells

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine zero_month

  monthly%tot_irr = 0.
  monthly%recharge = 0.
  monthly%gw_irr = 0.
  monthly%swc = 0.
  monthly%pET = 0.
  monthly%aET = 0.
  monthly%deficiency = 0.
  monthly%effprecip = 0.
  monthly%change_in_storage = 0.
  monthly%mar_depth = 0.
  monthly%runoff = 0.

end subroutine zero_month
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine zero_year

  yearly%tot_irr = 0.
  yearly%recharge = 0.
  yearly%gw_irr = 0.
  yearly%swc = 0.
  yearly%pET = 0.
  yearly%aET = 0.
  yearly%deficiency = 0.
  yearly%effprecip = 0.
  yearly%change_in_storage = 0.
  yearly%mar_depth = 0.
  yearly%runoff = 0.

  ann_spec_well_vol = 0.

end subroutine zero_year
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine initial_conditions

  INTEGER :: i

  previous%tot_irr = 0.
  previous%recharge = 0.
  previous%gw_irr = 0.
  previous%pET = 0.
  previous%aET = 0.
  previous%deficiency = 0.
  previous%effprecip = 0.
  previous%change_in_storage = 0.

  !LS vectorized for efficiency
  daily(:)%swc  = fields(:)%whc * crops(fields(:)%landcover_id)%RootDepth * fields(:)%init_fill_frac

  !write(*,*) fields(1)%whc
  previous%swc = daily%swc     ! Set previous day's swc to same as initial condition

end subroutine initial_conditions
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine IRR2CP(WY)

  INTEGER, INTENT(IN) :: WY
  INTEGER :: ip

  do ip = 1, npoly
     if (fields(ip)%WL2CP_year == WY) then    ! If WL2CP Year
       fields(ip)%irr_type = 3                                                       ! Change irrigation type to center pivot
     end if
   enddo

end subroutine IRR2CP

end MODULE define_fields
