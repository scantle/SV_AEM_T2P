module m_log
  use m_file_io, only: open_file_writer, t_file_writer
  use m_version, only: VERSION
  use t_time
  contains

!-------------------------------------------------------------------------------------------------!
  subroutine log_init(log, main_input_file)
    use m_file_io, only: log_unit
    implicit none
    type(t_file_writer),pointer  :: log
    character(100)               :: main_input_file
    integer                      :: i
    character(100)               :: basename
    ! Initialize
    do i=max(1,len_trim(main_input_file)-20), len_trim(main_input_file)
      if (main_input_file(i:i)==".") basename=main_input_file(1:i-1)
    end do
    log => open_file_writer(trim(basename)//".out")
    ! To avoid a circular dependency with the FileIO class
    log_unit = log%unit

    call log%write_line('   _________   _______  ________   ')
    call log%write_line('--|\___   ___\/  ___  \|\   __  \ ----')
    call log%write_line('   \|___ \  \_/__/|_/  /\ \  \|\  \ ')
    call log%write_line('------- \ \  \|__|//  / /\ \   ____\----')
    call log%write_line('         \ \  \   /  /_/__\ \  \___|')
    call log%write_line('--------- \ \__\ |\________\ \__\ --------')
    call log%write_line('           \|__|  \|_______|\|__|   ', blank_end=1)
    call log%write_line('           Texture2Par v'//VERSION)
    call log%write_line( '----------------------------------------------')
    call log%write_line('Copyright S.S. Papadopulos & Associates Inc.', blank_end=1)

    ! TODO write time of start
    call write_time_start(log_unit,'Started at: ')
    call start_timer()

  end subroutine log_init

!-------------------------------------------------------------------------------------------------!

  subroutine write_elapsed_time(log)
    implicit none
    type(t_file_writer),pointer  :: log
    integer                      :: elapsed_time
    integer                      :: hours, minutes, seconds
    character(100)               :: line

    elapsed_time = get_elapsed_time()

    hours = int(elapsed_time / 3600)
    minutes = int(mod(elapsed_time, 3600) / 60)
    seconds = int(mod(elapsed_time, 60))

    if (hours > 0) then
      write(line,'(a,i3,a,(i2,a))') "Elapsed time: ", hours, " hours, ", minutes, " minutes, ", seconds, " seconds."
    else if (minutes > 0) then
      write(line,'(a,2(i2,a))') "Elapsed time: ", minutes, " minutes, ", seconds, " seconds."
    else
      write(line,'(a,i2,a)') "Elapsed time: ", seconds, " seconds."
    endif

    call log%write_line(line, blank_start=1)

  end subroutine write_elapsed_time

!-------------------------------------------------------------------------------------------------!

  subroutine write_input_summary(log, t2p)
    use m_T2P, only: t_t2p
    implicit none
    type(t_file_writer),pointer  :: log
    type(t_t2p)                  :: t2p
    character(128)               :: line

    call log%write_line('--- INPUT SUMMARY ---', blank_start=1)

    ! T2P
    call log%write_valueline('Total Data Classes:',   t2p%nclasses)
    !TODO texture classes
    call log%write_valueline('Texture Data Classes:',   t2p%ntexclasses)
    call log%write_valueline('Covariate Data Classes:', t2p%nsecondary)
    call log%write_valueline('Number of Dataset Files:',t2p%ndatafiles)
    call log%write_valueline('Number of HSUs:',         t2p%nhsus)
    call log%write_valueline('Number of Variograms:',   t2p%nvario)

    ! Flow Model
    call t2p%fmodel%write_input_summary()

    ! Pilot Points
    if (.not. t2p%opt%ONLY_TEXTURE) then
      call log%write_valueline('Number of Pilot Point Zones',       t2p%pilot_points%nppzones)
      call log%write_valueline('Number of Aquifer Pilot Points',      t2p%pilot_points%npp)
      if (t2p%has_pplocs_aquitards) then
        call log%write_valueline('Number of Aquitard Pilot Points',  t2p%pilot_points_aquitard%npp)
      end if
      !call log%write_valueline('Number of Pilot Point Zones', t2p%pilot_points%nppzones)
    end if

    ! Options
    call t2p%opt%write_input_summary() ! Move here?
    
    ! Connections Array
    call t2p%write_connections()
    
    ! Variograms
    if (t2p%opt%INTERP_METHOD=='OK'.or.t2p%opt%INTERP_METHOD=='SK') then
      if (t2p%opt%INTERP_ORDER=="BEFORE") write(line,'(a)') '-- CLASS VARIOGRAMS --'
      if (t2p%opt%INTERP_ORDER=="AFTER") write(line,'(a)')  '-- PARAM VARIOGRAMS --'
      if (t2p%opt%INTERP_DIM==2) then
        call log%write_line(trim(line) //'                  struct type  nugget    sill    min_range    max_range  azimuth', blank_start=1)
      else if (t2p%opt%INTERP_DIM==3) then
        ! this%nugget, this%sill, this%a_hmin, this%a_hmax, this%a_vert, this%ang1, this%ang2, this%ang3
        call log%write_line(trim(line) //'                  struct type  nugget    sill    min_range    max_range   vert_range  azimuth      dip     roll', blank_start=1)
      end if
      call t2p%interp_data%write_summary()
      if (.not. t2p%opt%ONLY_TEXTURE) then
        write(line,'(a)') '-- PILOT POINT VARIOGRAM --'
        call log%write_line(trim(line) //'             struct type  nugget    sill    min_range    max_range  azimuth', blank_start=1)
        call t2p%interp_pp%write_summary()
      end if
    end if
    
    ! Aquifer Class Pilot Points
    if (.not. t2p%opt%ONLY_TEXTURE) then
      call log%write_line('--- AQUIFER PILOT POINTS ---', blank_start=1)
      call t2p%pilot_points%write_pp_table(is_aquitard=.false.)
    
      ! Aquitard Class Pilot Points
      if (t2p%fmodel%has_aquitards) then
        call log%write_line('--- AQUITARD PILOT POINTS ---', blank_start=1)
        call t2p%pilot_points_aquitard%write_pp_table(is_aquitard=.true.)
      end if
    end if
    call log%write_line('')

  end subroutine write_input_summary

!-------------------------------------------------------------------------------------------------!

  subroutine write_warnings(log)
    use m_error_handler, only: warnings
    use m_vstringlist, only: vstrlist_length, vstrlist_index
    use m_vstring
    implicit none
    type(t_file_writer),pointer  :: log
    integer           :: i

    if (vstrlist_length(warnings) < 1) return   ! Early return if no warnings

    call log%write_line('--- INPUT WARNINGS ---', blank_start=1)
    do i=1, vstrlist_length(warnings)
      call log%write_line(vstrlist_index(warnings,i))
    end do
    call log%write_line('') ! blank

  end subroutine write_warnings

!-------------------------------------------------------------------------------------------------!


end module m_log