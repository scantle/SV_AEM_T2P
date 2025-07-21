PROGRAM SWBM
  
! Scott valley soil water budget model (SWBM)
! 
! *****************************************************************************
! Input and Output is in Metric units:
! 
!                        [L]   =  meters
!                        [L^2] =  squared meters
!                        [L^3] =  cubic meters
!                        [T]   =  days
  
! subwn      : watershed name
! SWBM_LU    : 
! rotation   : 
! irr_type   : 
! tot_irr efficiency coefficients: 
! area       : 
! watersource: 
! whc        : 
  use m_global
  USE define_fields
  USE irrigation
  USE SWBM_output
  USE ditch_module
  USE m_options, only: t_options
  USE water_mover, only: read_water_mover_input_file, water_mover_setup_month, water_mover_sfr
  USE m_read_main_input, only: read_main_input, read_array_file
  USE m_file_io, only: get_command_args,io_initialize
  
  IMPLICIT NONE

  INTEGER :: numdays, WY, month, jday, i, im, ncmds, dummy, simday, total_days, loopdays
  INTEGER :: n_wel_param, unit_num  !, num_MAR_fields
  INTEGER :: abs_irr_month, abs_irr_day
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: rch_zones, ET_Zone_Cells
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: ndays, SFR_inflow_segs!, MAR_fields
  REAL, ALLOCATABLE, DIMENSION(:,:) :: ET_Cells_ex_depth
  REAL :: stn_precip, Total_Ref_ET, MAR_vol
  REAL, ALLOCATABLE, DIMENSION(:)  :: max_MAR_field_rate!, moisture_save
  REAL :: start, finish
  CHARACTER(10) :: suffix, date_text ! , scenario
  !CHARACTER(10) :: recharge_scenario, flow_lim_scenario, nat_veg_scenario
  CHARACTER(20) :: text_dummy
  CHARACTER(30) :: filename !, wel_file
  CHARACTER(400) :: cmd
  INTEGER, DIMENSION(31) :: ET_Active
  !LOGICAL :: MAR_active, ILR_active, daily_out_flag, inflow_is_vol, using_neighbor_irr_rule, using_abs_irr_date, daily_sw
  REAL :: eff_precip
  
  ! New variables
  ! Globals
  character(100)    :: main_input_file  ! Main input file
  type(t_options)   :: opt              ! Options object with default values to be overwritten
  integer           :: log_unit         ! Unit the logfile is opened on
  
  ! Initialization
  call init_globals()
  call io_initialize()
  call opt%initialize()
  call initialize_irr_ditch()
  
  ! Get command line arguments, returns main input file:
  call get_command_args(main_input_file)
 
  CALL cpu_time(start)
  
  log_unit = 800    ! TODO replace all instances of 800
  open(log_unit, file='SWBM.log')                         ! Open log file to mirror important screen output in file
  eff_precip = 0.
  Total_Ref_ET = 0.
  
  ! Read Main input file
  call read_main_input(main_input_file, opt)

  ! Write info to log
  write(*,'(A7,I6,A14,I4,A12,I5,A14,I5,A12,I3,A10,I5)') "npoly =", npoly, "nlandcover =", nlandcover, "nAgWells =", nAgWells, &
  "nSpecWells =", nSpecWells, "nSubws =", nSubws, "nmonths =", nmonths
  write(log_unit,'(A7,I6,A14,I4,A12,I5,A14,I5,A12,I3,A10,I5)') "npoly =", npoly, "nlandcover =", nlandcover, "nAgWells =", nAgWells, &
  "nSpecWells =", nSpecWells, "nSubws =", nSubws, "nmonths =", nmonths
  call opt%write_options_to_log(log_unit)
  
  ! Copy template files over for writing (previously done by system_commands.txt)
  call copy_file(sfr_network_file, trim(model_name)//'.sfr')
  call copy_file(ets_template_file, trim(model_name)//'.ets')
  call copy_file(wel_template_file, trim(model_name)//'.wel')
  if (trim(sfr_jtf_file) /= "") call copy_file(sfr_jtf_file, trim(model_name)//'_SFR.jtf')  ! TODO should error if WRITE_UCODE is TRUE

  ! Read in zone/property related files before time loop
  ALLOCATE(rch_zones(nrows,ncols))
  ALLOCATE(ET_Zone_Cells(nrows,ncols))
  ALLOCATE(ET_Cells_ex_depth(nrows,ncols))
  filename = trim(model_name) // '.wel'
  open (unit=536, file= filename, status="old")     
  read(536,*) ! Read heading line into nothing
  read(536,*) text_dummy, n_wel_param  ! Read "PARAMETER" as dummy text; read in number of well parameters (for printing WEL file later)
  close(536)

  open(unit=10, file='stress_period_days.txt', status = 'old')
  read(10,*)   ! read headers into nothing
  ALLOCATE(ndays(nmonths))
  do i=1, nmonths
  	read(10,*) dummy, ndays(i)   ! Reads the stress period # in the first column to "dummy" variable
    write(*, '(I6, I6)') i, ndays(i)
  enddo
  close(10)
  
  ! Read in MODFLOW recharge zone matrix
  rch_zones = 0
  call read_array_file(recharge_zones_file, rch_zones, nrows, ncols)
  
  ! Read in extinction depths for MODFLOW ET-from-groundwater zones
  ET_Cells_ex_depth = 0
  call read_array_file(et_ext_depth_file, ET_Cells_ex_depth, nrows, ncols)
  
  ! Read in 1-0 grid of cells with MODFLOW ET-from-groundwater zones (if passed, else assume 0s)
  ET_Zone_Cells = 0
  if (trim(et_zones_file) /= "") call read_array_file(et_zones_file, ET_Zone_Cells, nrows, ncols)
  
  CALL initialize_streams(nSubws, nSFR_inflow_segs)
  CALL read_landcover_table(nlandcover)

  CALL readpoly(npoly, nrows, ncols, rch_zones)                  ! Read in field info
  CALL initialize_wells(npoly, nAgWells, nSpecWells)             ! Read in Ag well info
  
  !LS Read in irrigation ditch & water mover
  if (trim(ditch_file)       /= "") call read_irr_ditch_input_file(ditch_file)
  if (trim(water_mover_file) /= "") call read_water_mover_input_file(water_mover_file)

  open(unit=82, file = poly_landcover_file, status = 'old')
  read(82,*)  ! read header into nothing

  ! Input files specifying field-by-field, 1 per stress period values
  open(unit=79, file=kc_frac_file, status = 'old')   
  read(79,*)  ! read header into nothing
  open(unit=85, file = sfr_partition_file, status = 'old')
  read(85,*)  ! read header into nothing
  open(unit=86, file = MAR_depth_file, status = 'old')
  read(86,*)  ! read header into nothing
  open(unit=87, file = curtail_frac_file, status = 'old')
  read(87,*)  ! read header into nothing
  if (trim(et_cor_file) /= "") then
    open(unit=89, file=et_cor_file, status='old')
    read(89,*)  ! read header into nothing
  end if
  
  ! Input files specifying 1 value per stress period
  open(unit=887,file=precip_file, status = 'old')
  !read(887,*)  ! read header into nothing                   
  open(unit=88,file=et_file, status = 'old')
  read(88,*)  ! read header into nothing

  CALL output_files(model_name)
  fields%irr_flag = 0          ! Initialize irrigating status flag array
  fields%et_cor   = 1.0        ! ET Correction
  month = 10                   ! Initialize month variable to start in October
  simday = 0                   ! Counter of day of simulation
  total_days = sum(ndays(:))   ! Total days in simulation
  loopdays = 1
  
  ! open(unit=801, file= trim(model_name)//'.mnw2', Access = 'append', status='old')       
  WY = WYstart   ! water year
  do im=1, nmonths                ! Loop over each month
    numdays = ndays(im)        ! Number of days in the current month
    if (opt%DAILY_SW) loopdays = numdays  ! needed for reading daily values, when doing daily sw calcs
    read(82,*) date_text, fields(1:)%landcover_id           ! read in landuse type (by field, for each month)
    !write(*,*) fields(1)%landcover_id
    read(86,*) date_text, fields(1:)%mar_depth     ! read in monthly MAR application depths (not driven by irrigation demand) (by field, for each month)
    read(87,*) date_text, fields(1:)%curtail_frac   ! read in curtailment fractions  (by field, for each month)
    if (trim(et_cor_file) /= "") read(89,*) date_text, fields(1:)%et_cor  ! read in et correction  (by field, for each month)
    do jday=1, loopdays
      read(85,*) date_text, SFR_allocation(:)%frac_subws_flow(jday)        ! read in multiplier for converting remaining subwatershed flows to SFR inflows
    end do
    if (nSpecWells>0) read(539,*) date_text, spec_wells(:)%specified_volume
    if (im==1) CALL initial_conditions                  ! initialize soil-water content for fields  
    if (month==10) then
      CALL zero_year           ! If October zero out yearly accumulated volume
    elseif (month==13) then
    	month = 1                ! Reset month to January
    endif
    Total_Ref_ET = 0.          ! Reset monthly Average ET
    CALL zero_month                               ! Zero out monthly accumulated volume
    CALL read_monthly_stream_inflow(opt%INFLOW_IS_VOL, numdays, opt%DAILY_SW)
    call water_mover_setup_month(im, numdays, opt%DAILY_SW)
    write(*,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',month,'   Length (days): ', numdays
    write(800,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',month,'   Length (days): ', numdays
       
    do jday=1, numdays                              ! Loop over days in each month
      simday = simday + 1
      if (jday==1) monthly%ET_active = 0            ! Set ET counter to 0 at the beginning of the month. Used for turning ET on and off in MODFLOW so it is not double counted.    
      daily%ET_active  = 0                          ! Reset ET active counter
      daily%tot_irr = 0.                            ! Reset daily tot_irr value to zero
      daily%gw_irr = 0.                             ! Reset daily gw_irr value to zero
      daily%effprecip = 0.                          ! Reset daily effective precip value to zero
      daily%pET = 0.                                ! Reset daily ET value to zero
      daily%recharge = 0.                           ! Reset daily recharge value to zero 
      daily%runoff = 0.                             ! Reset daily runoff value to zero
      read(88,*)  ETo, ETo_in, date_text
      Total_Ref_ET = Total_Ref_ET + ETo        ! Increment monthly ET rate  
      read(887,*) stn_precip
      read(79,*) date_text, crops(:)%daily_kc 
      daily%effprecip = stn_precip * fields%precip_fact
      daily%mar_depth = fields(:)%mar_depth / numdays
      ! Currently here - checking pET for native veg land use
      !write(*,'(A25,F4.2,F4.2)') "natveg k_c and kc_mult: ",crops(4)%daily_kc, crops(4)%kc_mult
      daily%pET=ETo * crops(fields%landcover_id)%daily_kc * crops(fields%landcover_id)%kc_mult * fields%et_cor    ! Set ET to current value for the day
      
      if(irrigating .eqv. .false.) then  ! if not irrigating yet, check number of fields irrigating
          if(sum(fields%irr_flag)>=opt%NEIGHBOR_RULE) then  ! Under the neighbor-irrigating rule,
            irrigating = .true.  ! if 20% of the fields are irrigating (by number, not area; 1251 total irrigated fields), set logical to true
            write(*,'(A,I2,A,I2,A,I4)')   " - Irrigating started: month ", month, " day " , jday, " WY ", WY
            write(800,'(A,I2,A,I2,A,I4)') " - Irrigating started: month ", month, " day " , jday, " WY ", WY
            !write(*,*) " "
        endif
      endif
      
      ! Check to see if we've hit the absolute irrigating date (Default is (999,999)
      if (month==opt%ABSOLUTE_IRR_DATE(1).and.jday>=opt%ABSOLUTE_IRR_DATE(2)) irrigating = .TRUE.

      do ip=1, npoly
        if (daily(ip)%effprecip < (0.2*ETo)) daily(ip)%effprecip = 0  ! if precip is less than 20% of ET, assume precip is lost as evaporating dew and 0 precip inflitrates to soil zone
        !if (ILR_active) then
          ! CALL IRRIGATION_ILR(ip, month, jday, eff_precip)
	      !else
	      CALL IRRIGATION_RULESET(ip, month, jday, irrigating, numdays)
	      !endif 
	      CALL water_budget(ip,jday,month)!,moisture_save,MAR_active)   
        if (month==12 .and. jday==31 .and. ip==npoly) then               ! If last day of the year, set tot_irr flags and logical to zero
		      fields%irr_flag = 0         
		      irrigating = .false.
 	        CALL IRR2CP(WY)                          ! Convert fields to center pivot irrigation
	      endif
      enddo           ! End of field/polygon loop
      
      CALL daily_out()                                                            ! Print Daily Output for Selected Fields
      CALL groundwater_pumping(jday, nAgWells, npoly, numdays,im)      ! Assign gw_irr to wells
      CALL monthly_SUM                                                            ! add daily value to monthly total (e.g., monthly%tot_irr = monthly%tot_irr + daily%tot_irr)
      CALL annual_SUM                                                             ! add daily value to annual total (e.g., yearly%tot_irr = yearly%tot_irr + daily%tot_irr)
      if (jday==numdays) then                                         
      	CALL SFR_streamflow(npoly, numdays, nSubws, nSegs, nSFR_inflow_segs, month, opt%DAILY_SW)      ! Convert remaining surface water and runoff to SFR inflows at end of the month	
        CALL water_mover_sfr(im, numdays, opt%daily_sw)
        ann_spec_well_vol = ann_spec_well_vol + SUM(spec_wells%specified_volume)  ! add monthly specified volume to annual total
      endif
    enddo             ! End of day loop  

    CALL convert_length_to_volume
    CALL monthly_out_by_field(im)
    CALL print_monthly_output(im, nlandcover, nSubws)
    if (opt%write_modflow) then
      CALL write_MODFLOW_RCH(im,numdays,nrows,ncols,rch_zones)
      CALL write_MODFLOW_ETS(im,numdays,nrows,ncols,rch_zones,Total_Ref_ET,ET_Zone_Cells, ET_Cells_ex_depth, npoly)
      CALL write_MODFLOW_SFR(im, month, nSegs, model_name, total_days, opt%DAILY_SW)
      CALL write_MODFLOW_SFR_tabfiles(im, numdays, simday, nSegs, opt%DAILY_SW)
      CALL write_MODFLOW_WEL(im, month, nAgWells, n_wel_param, model_name)       
    end if
    if (opt%write_ucode) CALL write_UCODE_SFR_template(im, month, nSegs, model_name, total_days, opt%DAILY_SW)   ! Write JTF file for UCODE
    if (opt%write_pest) CALL write_PEST_SFR_template(im, month, nSegs, model_name, total_days, opt%DAILY_SW)   ! Write JTF file for UCODE
    ! CALL write_MODFLOW_MNW2(im, nAgWells, nSpecWells, ag_wells_specified)          
    if (month==9) then
    CALL print_annual(WY)        ! print annual values at the end of September
      WY = WY +1
    endif
    month = month + 1
!    call flush
  enddo                  ! End of month loop
  CALL cpu_time(finish)
  write(*,'(A23,F6.2,A8)')'Model run completed in ',((finish-start)/60),' minutes'
  write(800,'(A23,F6.2,A8)')'Model run completed in ',((finish-start)/60),' minutes'
  
END PROGRAM SWBM