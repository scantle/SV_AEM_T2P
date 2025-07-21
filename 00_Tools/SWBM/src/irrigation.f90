MODULE irrigation
  
  use define_fields
  IMPLICIT NONE
  
  REAL:: kc_grain, kc_alfalfa,kc_alfalfa_mult, kc_grain_mult, kc_noirr
  REAL:: kc_pasture, kc_pasture_mult
  REAL:: irreff_flood, irreff_wl_LU11, irreff_cp_LU11, irreff_wl_LU2, irreff_cp_LU2
  REAL :: AV_REF_ET_1a, AV_REF_ET_1b, AV_REF_ET_2, ETo, ETo_in
  REAL :: monthly_precip_vol
  REAL :: EF_SF_Ratio, Sugar_Ratio, Johnson_Ratio, Crystal_Ratio, Patterson_Ratio
  LOGICAL :: irrigating
  INTEGER  :: nSegs, nSubws, nSFRdiv
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: div_segs, div_IPRIOR
  REAL, ALLOCATABLE, DIMENSION(:)  :: div_rate
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) ::  seg_name 
  
  CONTAINS

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE initialize_streams(nSubws, nSFR_inflow_segs)
  
    INTEGER, INTENT(IN) :: nSubws, nSFR_inflow_segs
    INTEGER :: dummy, i 
    character(60)  :: line
    logical        :: past_header=.false.
    
    open(unit=10, file='SFR_network.txt', status='old')
    !DO i=1,6   ! read first 6 comments lines of SFR file into nothing
    !  read(10,*) 
    !ENDDO
    ! Find header end - to the best of my knowledge, all possible options
    do while (past_header .neqv. .true.)
      read(10, '(a60)') line
      line = adjustl(line)
      if (line(1:1)=='#' .or. index(line, 'REACHINPUT')>0 .or. index(line, 'TRANSROUTE')>0 .or. index(line, 'TABFILES')>0) then
        continue
      else
        past_header = .true.
      end if
    end do
    read(line,*) dummy, nSegs 
    close(10)
    ALLOCATE(SFR_Routing(nSegs)) ! Allocate arrays of length equal to number of SFR segments       
    ALLOCATE(SFR_allocation(nSFR_inflow_segs))                       ! Allocate array of length equal to number of segments where inflow is specified
    ALLOCATE(surfaceWater(nSubws))                                   ! Allocate array of length equal to number of subwatersheds   
    open(unit=10, file='SFR_routing.txt', status='old')
    read(10,*) ! Read header into nothing
    DO i=1, nSegs
      read(10,*)SFR_Routing(i)%NSEG, SFR_Routing(i)%ICALC, SFR_Routing(i)%OUTSEG, SFR_Routing(i)%IUPSEG, &
                SFR_Routing(i)%IPRIOR, SFR_Routing(i)%WIDTH1, SFR_Routing(i)%WIDTH2, SFR_Routing(i)%MANNING_N, &
                SFR_Routing(i)%Bed_K_Param, SFR_Routing(i)%Manning_n_Param, SFR_Routing(i)%tabunit
      SFR_Routing(i)%Bed_K_Param = trim(SFR_Routing(i)%Bed_K_Param)
      SFR_Routing(i)%Manning_n_Param = trim(SFR_Routing(i)%Manning_n_Param)
    ENDDO
    close(10)     
    open(unit=10, file='SFR_inflow_segments.txt', status='old') 
    read(10,*)  ! read header into nothing
    DO i=1, nSFR_inflow_segs
  	  read(10,*) SFR_allocation(i)%subws_ID, SFR_allocation(i)%SFR_segment,&
  	  SFR_allocation(i)%subwsName, SFR_allocation(i)%streamName
    ENDDO
    close(10)
    open(unit = 215, file = 'subwatershed_irrigation_inflows.txt', status = 'old')
    read(215,*) ! Read header into nothing
    open(unit = 216, file = 'subwatershed_nonirrigation_inflows.txt', status = 'old')
    read(216,*) ! Read header into nothing
    open(unit = 217, file = 'SFR_diversions.txt', status = 'old')
    read(217,*) nSFRdiv
    ALLOCATE(div_segs(nSFRdiv))
    ALLOCATE(div_rate(nSFRdiv))
    ALLOCATE(div_IPRIOR(nSFRdiv))   
    read(217,*) (div_segs(i), i=1, nSFRdiv)   
    read(217,*) (div_IPRIOR(i), i=1, nSFRdiv) 
    read(217,*) (div_rate(i), i=1, nSFRdiv)   
    
  END SUBROUTINE initialize_streams

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine read_monthly_stream_inflow(inflow_is_vol, numdays, daily_sw)
    
    INTEGER, INTENT(IN) :: numdays
    LOGICAL, INTENT(IN) :: inflow_is_vol, daily_sw
    INTEGER :: i, j, looplen=1
    CHARACTER(10) :: date_dummy
    REAL*8 :: inflow_irr_tmp(nSubws)
    REAL*8 :: inflow_nonirr_tmp(nSubws)
    
    ! set looplen to days in month if doing daily flows
    if (daily_sw) looplen = numdays
    
    ! Reset to zero variables
    surfaceWater(:)%sw_irr = 0.0d0
    surfaceWater(:)%avail_sw_vol = 0.0d0
    
    do i = 1, looplen
      ! Read into temporary arrays (prevents i/o warning from reading into non-contiguous locations in memory)
      read(215,*) date_dummy, inflow_irr_tmp(1:)
      read(216,*) date_dummy, inflow_nonirr_tmp(1:)

      ! Assign values to surfaceWater components
      do j = 1, nSubws
        surfaceWater(j)%inflow_irr(i) = inflow_irr_tmp(j)
        surfaceWater(j)%inflow_nonirr(i) = inflow_nonirr_tmp(j)
      end do
    end do

    if (inflow_is_vol .or. daily_sw) then
      do i=1, nSubws
    	  surfaceWater(i)%avail_sw_vol = sum(surfaceWater(i)%inflow_irr(1:looplen))       ! Define available SW vol
      end do
    else
      surfaceWater%inflow_irr(1) = surfaceWater%inflow_irr(1) * numdays                                ! If inflow is an average monthly rate, convert to volume
      surfaceWater%inflow_nonirr(1) = surfaceWater%inflow_nonirr(1) * numdays
      surfaceWater%avail_sw_vol = surfaceWater%inflow_irr(1) 
    end if

    !write(*,'(A60)') "surfaceWater attributes: Irr, nonIrr, SWIrr, availSW:"
    !write(*,'(A30,es10.2,A3,es10.2,A3,es10.2,A3,es10.2,A3,es10.2)') "Irr, nonIrr, SWIrr, availSW:", &
    !write(*,*) surfaceWater%inflow_irr
    !write(*,*) surfaceWater%inflow_nonirr
    !write(*,*) surfaceWater%sw_irr
    !write(*,'(A60)') "AvailSW, after read_monthly_stream_inflow:"
    !write(*,*) surfaceWater%avail_sw_vol
  end subroutine read_monthly_stream_inflow
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  subroutine SFR_streamflow(npoly, numdays, nSubws, nSegs, nSFR_inflow_segs, month, daily_sw)
    
    INTEGER, INTENT(IN) :: npoly, numdays, nSubws, nSegs, nSFR_inflow_segs
    LOGICAL, INTENT(IN) :: daily_sw
    INTEGER :: i, j, wsid, seg, month, looplen=1
    REAL*8 :: temp_flow, avg_frac, flowsum
    
    ! set looplen to days in month if doing daily flows
    if (daily_sw) looplen = numdays
    
    SFR_Routing%FLOW = 0.0                                         ! Reset all SFR flow to zero
    do i=1, nSegs
      SFR_Routing(i)%FLOW_DAILY(:) = 0.0
    end do
    
    DO i=1,nSFR_inflow_segs
      wsid = SFR_allocation(i)%subws_ID
      seg = SFR_allocation(i)%SFR_segment
      temp_flow = surfaceWater(wsid)%avail_sw_vol + sum(surfaceWater(wsid)%inflow_nonirr(1:looplen))
      avg_frac = sum(SFR_allocation(i)%frac_subws_flow(1:looplen))/looplen
      
      !write(*,'(A20,I3, A20, es10.2)') "SFR inflow seg: " , seg, "SFR flow: ", temp_flow
      SFR_Routing(seg)%FLOW = (temp_flow*avg_frac)/numdays
      
      if (daily_sw .and. surfaceWater(wsid)%avail_sw_vol > 0.0) then
        flowsum = sum(surfaceWater(wsid)%inflow_irr(1:looplen))
        ! For daily flow, we subtract out a flow-proporational fraction of the monthly average irrigation rate.
        ! Subwatershed/Segments with zero flow left at the end of the month (avail_sw_vol = 0) get skipped -
        ! this leaves them with zero flow every day. This is equivalent to just subtracting out flow when it's 
        ! available. We know it's all allocated already!
        do j=1, numdays
          avg_frac = (surfaceWater(wsid)%inflow_irr(j) / flowsum)
          SFR_Routing(seg)%FLOW_DAILY(j) = (surfaceWater(wsid)%inflow_irr(j) - &
                   (surfaceWater(wsid)%sw_irr * avg_frac) + &
                   surfaceWater(wsid)%inflow_nonirr(j)) * SFR_allocation(i)%frac_subws_flow(j)
        end do
        ! LS Temp
        !if (minval(SFR_Routing(seg)%FLOW_DAILY(:))<0.0) then
        !  write(*,'(2(a,i3),a,es14.6)') 'Negative flow value on day', minloc(SFR_Routing(seg)%FLOW_DAILY(:)), &
        !                       ' | segment', i, '| Value:', minval(SFR_Routing(seg)%FLOW_DAILY(:))
        !end if
      else if (daily_sw .and. surfaceWater(wsid)%avail_sw_vol == 0.0) then 
        ! The available flow will just be the daily nonirrigation inflow
        SFR_Routing(seg)%FLOW_DAILY(1:looplen) = surfaceWater(wsid)%inflow_nonirr(1:looplen) * SFR_allocation(i)%frac_subws_flow(1:looplen)
      end if
      ! Debug
      !if (i==1) write(*,'(3a5,4a14)') 'i','wsid','seg','SWRemainder','AvgDailyPump','Flow','AvgDailyFlow'
      !write(*,'(3i5,4es14.6)') i, wsid, seg, surfaceWater(wsid)%avail_sw_vol, &
      !       (surfaceWater(wsid)%sw_irr / numdays), SFR_Routing(seg)%FLOW, &
      !       sum(SFR_Routing(seg)%FLOW_DAILY(:))/numdays
    ENDDO

    !if (month .ge. 4 .and. month .le. 7) then ! If April-July, when the ditches are running
    !  SFR_Routing(div_segs)%FLOW = div_rate ! divert water from mainstem
    !else 
    !  SFR_Routing(div_segs)%FLOW = 0 ! In Aug-Mar, divert no water
    !endif
      
    SFR_Routing%RUNOFF = 0.                                        ! Reset runoff to zero 
    DO i=1,npoly                                                   ! Sum runoff volumes for each SFR segment
      SFR_Routing(fields(i)%runoff_ISEG)%RUNOFF = SFR_Routing(fields(i)%runoff_ISEG)%RUNOFF + monthly(i)%runoff*fields(i)%area 	
    ENDDO 
    SFR_Routing%RUNOFF = SFR_Routing%RUNOFF / numdays              ! Calculate average daily runoff for model input
        
  end subroutine SFR_streamflow
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE IRRIGATION_RULESET(ip, month, jday, irrigating, numdays)
  
    INTEGER, INTENT(IN) :: ip, month, jday, numdays
    INTEGER :: subws
    LOGICAL :: irrigating
    REAL :: irreff   
          
    subws = fields(ip)%subws_ID
    irreff = 0.
    select case (fields(ip)%irr_type)
      case(1)  ! Flood irrigation
  	  	irreff = crops(fields(ip)%landcover_id)%irreff_flood
  	  case(2)   ! Wheel line irrigation
  	  	irreff = crops(fields(ip)%landcover_id)%irreff_wl
  	  case(3)   ! Center pivot irrigation
  	  	irreff = crops(fields(ip)%landcover_id)%irreff_cp
      case(999)   ! Unknown irrigation 
        if( crops(fields(ip)%landcover_id)%irrigated ) then
          irreff = crops(fields(ip)%landcover_id)%irreff_wl  ! assign Wheel Line irr. eff. for irrigated crops
        else 
          irreff = 1.0                                       ! Assign 100% efficiency for non-irrigated crops 
        endif
    end select

    !if(ip==1 .and. jday==10) then
    !  write(*,'(A20,I3)') "Field 1 wat source", fields(ip)%water_source
    !end if


    if ( (crops(fields(ip)%landcover_id)%irrigated .and. fields(ip)%irr_type /= 555 .and.  fields(ip)%water_source /= 5) &     ! If field is irrigated
    .and. ((month == crops(fields(ip)%landcover_id)%IrrMonthStart .and. jday >= crops(fields(ip)%landcover_id)%IrrDayStart)&   ! and during defined irrigation season
    .or.  (month > crops(fields(ip)%landcover_id)%IrrMonthStart .and. month < crops(fields(ip)%landcover_id)%IrrMonthEnd)&
    .or.  (month == crops(fields(ip)%landcover_id)%IrrMonthEnd .and. jday <= crops(fields(ip)%landcover_id)%IrrDayEnd))& 
    .and. ((daily(ip)%swc < crops(fields(ip)%landcover_id)%IrrSWC * fields(ip)%whc * crops(fields(ip)%landcover_id)%RootDepth) & ! and EITHER SWC has dropped
    .or. irrigating) ) then                         ! below defined irrigation trigger OR >20% of the neighbors are irrigating already
          ! If all of the above are true, apply irrigation ruleset
          fields(ip)%irr_flag = 1 ! Set field status to irrigating (even if already the case)
          daily(ip)%tot_irr=max(0.,((daily(ip)%pET-daily(ip)%effprecip)/irreff))                                   ! Calculate applied linear irrigation (depth units)
          daily(ip)%tot_irr = daily(ip)%tot_irr * (1 - fields(ip)%curtail_frac)                                    ! Subtract curtailment fraction (default: 0) from calculated irr. demand
          ! daily(ip)%tot_irr = daily(ip)
          select case (fields(ip)%water_source)                                                                        ! Assign irrigation to water source
          case(1) ! surface water
              ! add MAR to tot_irr to make sure MAR volume is accounted for
              !debug
              !write(*,'(a10,I3,A24,F16.4)') "subws:", subws, "subws avail sw vol: ", surfaceWater(subws)%avail_sw_vol
              !if(ip==1627) write(*,'(I4,F8.1)') jday, daily(ip)%tot_irr * fields(ip)%area
    			if (surfaceWater(subws)%avail_sw_vol >= ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area)) then                                ! If available surface water exceeds demand
                    !debug
                    !write(*,*) "made it into the Avail_SW > demand loop"
                    !write(*,'(A15,I3, A7, I3)') "watersource: ", fields(ip)%water_source, "crop: ", fields(ip)%landcover_id
    			  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area)          ! Add daily irrigation volume to total surface water irrigation 
    			  surfaceWater(subws)%avail_sw_vol = surfaceWater(subws)%avail_sw_vol - ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area)      ! Update available surface water volume	
    			else if (surfaceWater(subws)%avail_sw_vol < ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area) &                                  ! If supply is less than demand but greater than 0
    				 .and. surfaceWater(subws)%avail_sw_vol /= 0) then
    				 	daily(ip)%tot_irr = surfaceWater(subws)%avail_sw_vol / fields(ip)%area                                         ! Use remaining surface water for irrigation
            	surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + surfaceWater(subws)%avail_sw_vol                     ! Use remaining surface water for irrigation
            	surfaceWater(subws)%avail_sw_vol = 0.0d0                                                                       ! Set remaining surface water to zero
    			else
    				  daily(ip)%tot_irr = 0.0d0                                                  ! Irrigation set to zero when surface-water supplies are exceeded 
    			end if
    	  case(2) ! groundwater
    	  	daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well
          case(3) ! Mixed Water Source
              !add MAR depth to tot_irr to ensure volume is accounted for
    			if (surfaceWater(subws)%avail_sw_vol >= ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area)) then   !If there is more SW avail. than irr. demand + MAR                             ! If available surface water exceeds demand
    			  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area)                  ! Add daily irrigation volume to total surface water irrigation 
    			  surfaceWater(subws)%avail_sw_vol = surfaceWater(subws)%avail_sw_vol - ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area)      ! Update available surface water volume	
    			else if (surfaceWater(subws)%avail_sw_vol < ((daily(ip)%tot_irr + daily(ip)%mar_depth) * fields(ip)%area) &                                ! If supply is less than demand but greater than 0
    				 .and. surfaceWater(subws)%avail_sw_vol /= 0) then
            	surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + surfaceWater(subws)%avail_sw_vol                     ! Use remaining surface water for irrigation
            	daily(ip)%gw_irr = (daily(ip)%tot_irr + daily(ip)%mar_depth) - (surfaceWater(subws)%avail_sw_vol / fields(ip)%area)                    ! Use GW for remainder of demand
            	surfaceWater(subws)%avail_sw_vol = 0.                                                                          ! Set remaining surface water to zero
    			else !if (surfaceWater(subws)%avail_sw_vol == 0)
    				  daily(ip)%gw_irr =  daily(ip)%tot_irr                                   ! All irrigation assigned to groundwater well
    			end if 
    	  case(4) ! sub-irrigated
    	  	daily(ip)%tot_irr = 0.0d0  ! No irrigation applied
        case(5) ! dry-farmed
    	  	daily(ip)%tot_irr = 0.0d0  ! No irrigation applied
    	  case(999) ! unknown, assume GW source
    	  	daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well
          end select
    else                 ! if not irrigating, add MAR volume to tot_irr for field and to SW Irr from each subwatershed (if enough SW is avail.)
        if (surfaceWater(subws)%avail_sw_vol >= (daily(ip)%mar_depth * fields(ip)%area)) then       ! If available surface water exceeds demand
            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + (daily(ip)%mar_depth * fields(ip)%area)          ! Add daily irrigation volume to total surface water irrigation 
    		surfaceWater(subws)%avail_sw_vol = surfaceWater(subws)%avail_sw_vol - (daily(ip)%mar_depth * fields(ip)%area)      ! Update available surface water volume	
        else if (surfaceWater(subws)%avail_sw_vol < (daily(ip)%mar_depth * fields(ip)%area) &                                  ! If supply is less than demand but greater than 0
        .and. surfaceWater(subws)%avail_sw_vol /= 0) then
            daily(ip)%mar_depth = surfaceWater(subws)%avail_sw_vol / fields(ip)%area                                         ! Use remaining surface water for irrigation
            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + surfaceWater(subws)%avail_sw_vol                     ! Use remaining surface water for irrigation
            	surfaceWater(subws)%avail_sw_vol = 0.0d0                                                                     ! Set remaining surface water to zero
        else
            daily(ip)%mar_depth = 0.0d0                                                  ! Irrigation set to zero when surface-water supplies are exceeded 
        endif
        daily(ip)%tot_irr = daily(ip)%tot_irr + daily(ip)%mar_depth       ! Add MAR depth (default: 0) to total irrigation
    endif
    

    !if(daily(ip)%gw_irr > 1000000) then ! if it's infinity
    !  write(*,'(A25,I5)') "infinite GW on field id ", ip
    !  write(*,'(A12,I6,A15,I4,A12,I5)') "landcover =", fields(ip)%landcover_id, &
    !  "water_source =", fields(ip)%water_source, "irr_type =", fields(ip)%irr_type
      !write(*, '(A33, I5)') "Crop, wat. src. and irr. type: " , fields(ip)%water_source, fields(ip)%irr_type
    !endif

  END SUBROUTINE IRRIGATION_RULESET
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  SUBROUTINE water_budget(ip,jday,month)!,moisture_save,MAR_active)
  
    INTEGER, INTENT (IN) :: ip, jday, month
    !REAL, DIMENSION(npoly), INTENT(inout) :: moisture_save
    !LOGICAL, INTENT(in)  :: MAR_active
    REAL:: rch
  
    daily(ip)%aET=min(daily(ip)%pET, previous(ip)%swc+daily(ip)%effprecip+daily(ip)%tot_irr)  ! Actual ET for field ip is the pET (if enough soil water) or sum of yesterday's SWC, today's eff. precip, and the total irrigation (if not enough soil water)
    daily(ip)%deficiency=daily(ip)%pET - daily(ip)%aET ! calculate crop water deficiency
    if (daily(ip)%aET > 0) daily(ip)%ET_active =  1   ! Set ET flag to 1 if ET is active that day
    rch = max(0., (previous(ip)%swc+daily(ip)%effprecip+daily(ip)%tot_irr-daily(ip)%aET)-& ! Recharge is sum(yesterday's SWC, today's eff. precip, and today's total irrigation) minus today's aET, minus the field's water holding capacity
    fields(ip)%whc*crops(fields(ip)%landcover_id)%RootDepth)

    ! Temporarily disabling the runoff calculation function for comparison to basecase.
        ! Questions for TH: how allow for ponding? 
    !if (rch > fields(ip)%max_infil_rate) then
    !    ! when is this an issue? print which fields it's exceeding on?
    !    print *, '("Rch > max_infil_rate - poly ", ip, "month", month, " jday", jday)'
    !        if(daily(ip)%mar_depth == 0) then !  if mar is active on this field, do nothing. do not implement runoff (allow for ponding and eventual infiltration)
    !           daily(ip)%runoff = (rch - fields(ip)%max_infil_rate) ! calculate runoff if recharge exceeds field's max infiltration rate
    !            rch = fields(ip)%max_infil_rate
    !        endif
    !endif
    daily(ip)%recharge = rch
    daily(ip)%swc=max(0.,previous(ip)%swc+daily(ip)%effprecip+daily(ip)%tot_irr- & ! today's SWC = yesterday's SWC + today's precip + irrigation 
      daily(ip)%aET-daily(ip)%recharge-daily(ip)%runoff)                           !- aET - recharge - runoff
    daily(ip)%residual = daily(ip)%swc-previous(ip)%swc+daily(ip)%aET+daily(ip)%recharge+daily(ip)%runoff- &
                       daily(ip)%effprecip-daily(ip)%tot_irr       
    !if (MAR_active) moisture_save(ip) = previous(ip)%swc 
    previous(ip)%swc = daily(ip)%swc
    daily(ip)%change_in_storage = daily(ip)%effprecip+daily(ip)%tot_irr-daily(ip)%aET-daily(ip)%recharge-daily(ip)%runoff	
  
  END SUBROUTINE water_budget
  
END MODULE irrigation

