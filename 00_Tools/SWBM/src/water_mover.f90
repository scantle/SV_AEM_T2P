module water_mover
  use m_error_handler, only: error_handler
  use m_options, only: t_options
!-------------------------------------------------------------------------------------------------!
! Module variables
  integer                  :: nmoves
  integer                  :: min_month
  integer                  :: max_month
  integer, allocatable     :: from_to(:,:)   ! Move water from index/to index ([from,to], nmoves)
  integer, allocatable     :: move_type(:,:) ! ([from,to], nmoves) 1-Well 2-SFR 3-MAR
  integer, allocatable     :: move_date(:,:) ! start SP, end SP ([start,end], nmoves)
  real, allocatable        :: sfr_rate(:)    ! m3/day (nreaches)
  real, allocatable        :: wel_rate(:)    ! m3/day (nwells)
  real, allocatable        :: mar_rate(:)    ! m3/day (nfields)
  
  ! TODO create mover for subws sw (then water could be moved from a stream properly)
  ! Evaluate if rate variables can/should be combined
  
!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!
  subroutine read_water_mover_input_file(filename)
    use m_file_io
    use m_vstringlist, only: t_vstringlist
    use irrigation, only: nsegs
    use define_fields, only: npoly, nAgWells, ag_wells
    implicit none
    character(*), intent(in)     :: filename
    type(t_file_reader), pointer :: reader
    integer                      :: i,j
    character(10)                :: temp
    real                         :: rate
    
    ! Standard file reader variables
    integer                    :: status, length, eof
    character(30)              :: id, value
    type(t_vstringlist)        :: strings
    
    reader => open_file_reader(filename)
    
    call reader%next_block_id(eof, id)
    select case(trim(id))
      case("WATER_MOVER")  ! Should be the only thing in this file!
        nmoves = reader%get_block_len()
        ! Know everything we need to allocate
        allocate(from_to(2,nmoves), move_type(2,nmoves), move_date(2,nmoves))
        allocate(sfr_rate(nmoves), wel_rate(nmoves), mar_rate(nmoves))
        from_to   = 0
        move_type = 0
        move_date = 0
        sfr_rate  = 0.0
        wel_rate  = 0.0
        mar_rate  = 0.0
        
        ! Read lines
        do i=1, nmoves
          call reader%next_block_item(status, id, strings, length)
          move_type(1,i) = find_string_index_in_list(strings, 1, "WELL,SFR", toupper=.true.)
          from_to(1,i)   = item2int(strings, 2)
          move_type(2,i) = find_string_index_in_list(strings, 3, "WELL,SFR,MAR", toupper=.true.)
          from_to(2,i)   = item2int(strings, 4)
          move_date(1,i) = item2int(strings, 5)
          move_date(2,i) = item2int(strings, 6)
          rate           = item2real(strings,7)
          
          ! Handle Move FROM
          select case(move_type(1,i))
            case(1) ! WELL
              do j=1, nAgWells
                if (from_to(1,i)==ag_wells(j)%well_id) then
                  from_to(1,i) = j
                  exit
                end if
              end do
              ! TODO real check
              if ((from_to(1,i) > 0) .and. (from_to(1,i) <= nAgWells)) then
                wel_rate(i) = rate  ! Rate is positive until written to WEL in write_MODFLOW_WEL
              else
                call item2char(strings, 2, temp)
                call error_handler(1,reader%file,"Invalid from well id, check nAgWells. well id = "//temp)
              end if
            case(2) ! SFR
              if (from_to(1,i) > 0 .and. from_to(1,i) <= nsegs) then
                sfr_rate(i) = -1 * rate
              else
                call item2char(strings, 2, temp)
                call error_handler(1,reader%file,"Invalid from segment id = "//temp)
              end if
            case DEFAULT
              call error_handler(1,reader%file,"Invalid from destination: Must be one of WELL, SFR")
          end select
          
          ! Handle Move TO
          select case(move_type(2,i))
            case(1) ! WELL
              do j=1, nAgWells
                if (from_to(2,i)==ag_wells(j)%well_id) then
                  from_to(2,i) = j
                  exit
                end if
              end do
              ! TODO real check
              if (from_to(2,i) > 0 .or. from_to(2,i) <= nAgWells) then
                wel_rate(i) = -1 * rate  ! Rate is positive until written to WEL in write_MODFLOW_WEL
              else
                call item2char(strings, 4, temp)
                call error_handler(1,reader%file,"Invalid from well id, check nAgWells. well id = "//temp)
              end if
            case(2) ! SFR
              if (from_to(2,i) > 0 .or. from_to(2,i) <= nsegs) then
                sfr_rate(i) = rate
              else
                call item2char(strings, 4, temp)
                call error_handler(1,reader%file,"Invalid from segment id = "//temp)
              end if
            case(3) ! MAR
              if (from_to(2,i) > 0 .or. from_to(2,i) <= npoly) then
                mar_rate(i) = rate
              else
                call item2char(strings, 4, temp)
                call error_handler(1,reader%file,"Invalid Polygon/Field id = "//temp)
              end if
            case DEFAULT
              call error_handler(1,reader%file,"Bad from destination: Must be one of WELL, SFR")
          end select
          
        end do
        
      case DEFAULT
        call error_handler(1,reader%file,"Invalid Block: " // trim(id))
    end select
    
    ! Setup min/max months
    min_month = minval(move_date(1,:))
    max_month = maxval(move_date(2,:))
  
  end subroutine read_water_mover_input_file
!-------------------------------------------------------------------------------------------------!
  
  subroutine water_mover_setup_month(im, numdays, daily_sw)
    use define_fields, only: daily,fields
    ! Runs before day loop and irrigation loop
    ! Intent is to fill any monthly arrays. For now, that only means to MAR.
    implicit none
    integer, intent(in)     :: im, numdays
    logical, intent(in)     :: daily_sw
    integer                 :: i
    
    ! Early exit
    if (im < min_month .or. im >= max_month) return
    
    do i=1, nmoves
      if ((im >= move_date(1,i) .and. im < move_date(2,i)).and.(move_type(2,i)==3)) then  ! GE start and LT end and MAR TO
        ! Add to daily accumulator
        daily(from_to(2,i))%mar_depth = daily(from_to(2,i))%mar_depth + mar_rate(i) / fields(from_to(2,i))%area
      end if
    end do

  end subroutine water_mover_setup_month
!-------------------------------------------------------------------------------------------------!
  
  subroutine water_mover_sfr(im, numdays, daily_sw)
    ! Called on the last day of the month, right after SFR_streamflow
    ! For FROM SFR, this means it is after water has been used. It may make SFR_Routing negative!
    ! For TO SFR, this means the water added cannot be used for irrigation
    use define_fields, only: sfr_routing
    implicit none
    integer, intent(in)     :: im, numdays
    logical, intent(in)     :: daily_sw
    integer                 :: i, looplen
    
    ! To match SFR_streamflow
    looplen = 1
    if (daily_sw) looplen = numdays
    
    ! Early exit
    if (im < min_month .or. im >= max_month) return
    
    do i=1, nmoves
      if (im >= move_date(1,i) .and. im < move_date(2,i)) then  ! GE start and LT end
        ! adjust SFR Routing accumulator
        if (move_type(1,i)==2) SFR_Routing(from_to(1,i))%FLOW_DAILY(1:looplen) = SFR_Routing(from_to(1,i))%FLOW_DAILY(1:looplen) + (sfr_rate(i) * numdays/looplen)
        if (move_type(2,i)==2) SFR_Routing(from_to(2,i))%FLOW_DAILY(1:looplen) = SFR_Routing(from_to(2,i))%FLOW_DAILY(1:looplen) + (sfr_rate(i) * numdays/looplen)
      end if
    end do
  
  end subroutine water_mover_sfr
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine water_mover_well(im, numdays, agwells_monthly_vol)
    ! Called in groundwater_pumping (outputmodule) on the last day of the month
    use define_fields, only: sfr_routing, nAgWells
    implicit none
    integer, intent(in)     :: im, numdays
    real                    :: agwells_monthly_vol(nAgWells)
    integer                 :: i, looplen
    
    ! Early exit
    if (im < min_month .or. im >= max_month) return
    
    do i=1, nmoves
      if (im >= move_date(1,i) .and. im < move_date(2,i)) then  ! GE start and LT end
        ! adjust SFR Routing accumulator
        if (move_type(1,i)==1) agwells_monthly_vol(from_to(1,i)) = agwells_monthly_vol(from_to(1,i)) + (wel_rate(i) * numdays)
        if (move_type(2,i)==1) agwells_monthly_vol(from_to(2,i)) = agwells_monthly_vol(from_to(2,i)) + (wel_rate(i) * numdays)
      end if
    end do
  
  end subroutine water_mover_well
  
!-------------------------------------------------------------------------------------------------!
end module water_mover