module ditch_module
  implicit none
  !-----------------------------------------------------------------------------------------------!
  ! Irrigation Ditch Module
  ! Author: Leland Scantlebury, Sept 2022
  !-----------------------------------------------------------------------------------------------!
  ! Represents irrigation ditches using diversions in the MODFLOW SFR package, with ditch leackage
  ! as injection wells in the MODFLOW Well package. Diversion and Leakage rates are specified by
  ! month. The SWBM template file will need to include segments for the ditches to (1) divert water
  ! from and (2) move water too, where it will be removed from the model. These are the diversion
  ! 'upstream segment' and 'segment' values, respectively.
  !-----------------------------------------------------------------------------------------------!

  ! Module Types
  type ditch
    integer              :: ncells
    integer              :: sfr_diversion(2)                      ! segment, upstream segment
    real                 :: diversion_rate(12), leakage_rate(12)  ! Monthly
    real                 :: roughch, width(2)                     ! SFR mannings coef, reach start/end width
    integer, allocatable :: lay(:), row(:), col(:)
  end type

  ! Module Variables
  integer                  :: nditches
  type(ditch), allocatable :: ditches(:)

  ! Module Subroutines/Functions
  contains
  
  !-----------------------------------------------------------------------------------------------!
  subroutine initialize_irr_ditch()
    !---------------------------------------------------------------------------------------------!
    ! Set default values to ensure nothing breaks if ditch file isn't present (=='NONE')
    !---------------------------------------------------------------------------------------------!
    nditches = 0
  end subroutine initialize_irr_ditch
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  subroutine read_irr_ditch_input_file(filename)
    use m_file_io, only: get_next_iunit, return_iunit
    implicit none
    !---------------------------------------------------------------------------------------------!
    ! Reads irrigation ditch input file, loading all needed ditch data
    !
    ! Arguments
    ! - filename: file name/path of irrigation ditch input file
    ! - unit: unit number to open input file on
    !---------------------------------------------------------------------------------------------!
      
    character(*), intent(in) :: filename
    integer                    :: i, j, month, ierr, unit
    integer, allocatable       :: ls(:), rs(:)
    real                       :: factor
    character(300)             :: line
    character(256)             :: cerr
    
    unit = get_next_iunit()
    
    open(unit, file=trim(filename), status='old', iostat=ierr, iomsg=cerr)
    if (ierr /= 0) then
      write(*,*) trim(cerr)
      stop
    end if

    ! Number of ditches, conversion factor
    call read_to_data(unit)
    read(unit,*) nditches, factor
    
    ! Allocate ditches, left side and right side read variables
    allocate(ditches(nditches), ls(2+nditches*2), rs(2+nditches*2))
    
    ! Cells in each ditch
    call read_to_data(unit)
    read(unit,'(a300)') line
    call multisplit(line,nditches,ls,rs,ierr)
    do i=1, nditches
      read(line(ls(i):rs(i)),*) ditches(i)%ncells
      allocate(ditches(i)%lay(ditches(i)%ncells), &
               ditches(i)%row(ditches(i)%ncells), &
               ditches(i)%col(ditches(i)%ncells)  )
    end do
    
    ! SFR Diversion points
    call read_to_data(unit)
    do i=1, nditches
      read(unit,*) ditches(i)%sfr_diversion
    end do
    
    ! SFR Mannings Coefficients and stream widths
    call read_to_data(unit)
    do i=1, nditches
      read(unit,*) ditches(i)%roughch, ditches(i)%width
    end do
    
    ! Watering Schedule
    call read_to_data(unit)
    do i=1, 12
      read(unit,'(a300)') line
      call multisplit(line,1+nditches*2,ls,rs,ierr)
      read(line(ls(1):rs(1)),*) month
      do j=1, nditches
        ! Alternates (diversion rate, leakage rate) by ditch
        read(line(ls(2+(j-1)*2):rs(2+(j-1)*2)),*) ditches(j)%diversion_rate(month)
        read(line(ls(1+j*2):rs(1+j*2)),*) ditches(j)%leakage_rate(month)
        ! Apply conversion factor
        ditches(j)%diversion_rate(month) = ditches(j)%diversion_rate(month) * factor
        ditches(j)%leakage_rate  (month) = ditches(j)%leakage_rate  (month) * factor
      end do
    end do
    
    ! Cells
    do i=1, nditches
      call read_to_data(unit)
      do j=1, ditches(i)%ncells
        read(unit,*) ditches(i)%lay(j), ditches(i)%row(j), ditches(i)%col(j)
      end do
    end do
    
    close(unit)
    call return_iunit(unit)

  end subroutine read_irr_ditch_input_file
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  integer function is_ditch(segment)
    implicit none
    !---------------------------------------------------------------------------------------------!
    ! Given SFR segment returns ditch index number
    ! Returns -999 if segment does not correspond to a ditch
    !
    ! Arguments
    ! - segment: SFR segment id
    !---------------------------------------------------------------------------------------------!
    
    integer, intent(in)   :: segment
    integer               :: i
    
    is_ditch = -999
    
    do i=1, nditches
      if (ditches(i)%sfr_diversion(1)==segment) then
        is_ditch = i
      end if
    end do
  return
  
  end function is_ditch
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  subroutine write_ditch_diversion(unit, iditch, month)
    implicit none
    !---------------------------------------------------------------------------------------------!
    ! Writes the time-varying reach data for the ditch diversions - corresponding to Data Set 6a
    ! of the SFR file. OUTSEG is hard coded to 0 to remove the water from the model since ditches
    ! are currently assumed to be not explicitly represented.
    !
    ! see: https://water.usgs.gov/ogw/modflow-nwt/MODFLOW-NWT-Guide/sfr.html
    !
    ! Arguments:
    ! - unit: unit number SFR file is open on
    ! - iditch: index of ditch in ditches(nditches) being written
    ! - month: Month to be written (currently assumed 1-12)
    !---------------------------------------------------------------------------------------------!
    
    integer, intent(in)  :: unit, iditch,month
    
    ! NSEG ICALC OUTSEG IUPSEG IPRIOR FLOW RUNOFF ETSW PPTSW ROUGHCH
    write(unit,100) ditches(iditch)%sfr_diversion(1), 1, 0, &
                    ditches(iditch)%sfr_diversion(2), 0,    &
                    ditches(iditch)%diversion_rate(month), &
                    0, 0, 0, ditches(iditch)%roughch
    ! Start and end reach widths
    write(unit,'(es10.2)') ditches(iditch)%width(1)
    write(unit,'(es10.2)') ditches(iditch)%width(2)
    
100 format(i4,4(1x,i2),es14.6,3(1x,i2),1x,f6.3)  
  
  end subroutine write_ditch_diversion
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  integer function nditch_wells(month)
    implicit none
    !---------------------------------------------------------------------------------------------!
    ! Returns the number of active ditch leakage "well" cells given the month of the simulation
    !
    ! Arguments:
    ! - month: Month
    !---------------------------------------------------------------------------------------------! 
    
    integer, intent(in) :: month
    integer              :: i
    
    ! Loop over ditches seeing if they have any leakage this month
    nditch_wells = 0
    do i=1, nditches
      if (ditches(i)%leakage_rate(month) > 0.0) then
        nditch_wells = nditch_wells + ditches(i)%ncells
      end if
    end do
    return
  
  end function nditch_wells
  !-----------------------------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------------------------------------!
  subroutine write_ditch_wells(unit, month)
    implicit none
    !---------------------------------------------------------------------------------------------!
    ! Writes lines to MODFLOW Well package file representing ditches as injection wells. Total
    ! ditch leakage is evenly divided among cells. Writes all active ditches (rate > 0)
    !
    ! Arguments:
    ! - unit: unit number the WEL file is open on
    ! - month: Month
    !---------------------------------------------------------------------------------------------!
    
    integer, intent(in)  :: unit, month
    integer              :: i, j, mnth
    real                 :: cell_rate
    
    do i=1, nditches
      ! Skip ditch if no leackage
      if (ditches(i)%leakage_rate(month) > 0.0) then
        cell_rate = ditches(i)%leakage_rate(month) / ditches(i)%ncells
        do j=1, ditches(i)%ncells
          write(unit,'(3I10,ES15.3)') ditches(i)%lay(j), &
                                      ditches(i)%row(j), &
                                      ditches(i)%col(j), &
                                      cell_rate
        end do
      end if
    end do
  
  end subroutine write_ditch_wells
  !-----------------------------------------------------------------------------------------------!

  !-----------------------------------------------------------------------------------------------!
  ! TODO: Move all below to a seperate module for reading files
  subroutine read_to_data(unit)
    implicit none
    !---------------------------------------------------------------------------------------------!
    ! Reads lines to find next line with data. Moves past comment lines (#, *) and blank lines.
    ! Moves back one lines when the data line is discovered - allowing the calling routine/program
    ! to read/handle the line.
    ! Author: Leland Scantlebury, Sept 2022
    !---------------------------------------------------------------------------------------------!

    integer, intent(in)  :: unit
    integer              :: ierr
    character(300)       :: line
    character(1)         :: compare

    do
      read(unit, '(a300)', iostat=ierr) line
      ! Handle EOF
      if (ierr /= 0) then
        ! TODO Add error handling module/subroutine
        write(*,'(a,i3)') "Error - End of file reached unexpectedly on unit", unit
        stop
      end if
      compare = adjustl(line)
      if ((compare == '#').or.(compare == '*').or.(compare == '')) then
        ! Comment line, blank line
        continue 
      else
        ! Data
        backspace(unit)
        exit
      end if
    end do

  end subroutine read_to_data
  !-----------------------------------------------------------------------------------------------!
  
  SUBROUTINE multisplit(CLINE,NUM,LW,RW,IFAIL)

! -- Subroutine multisplit splits a string into blank-delimited fragments.

! -- Subroutine arguments are as follows:-
!       cline:    character string
!       num:      number of items expected in string
!       lw:       (returned) start index of each item in string
!       rw:       (returned) end index of each item in string
!       ifail:    returned as non-zero in case of failure

! -- Author:-
!       John Doherty

       INTEGER IFAIL,NW,NBLC,J,I
       INTEGER NUM,NBLNK
       INTEGER LW(NUM),RW(NUM)
       CHARACTER*(*) CLINE
       IFAIL=0
       NW=0
       NBLC=LEN_TRIM(CLINE)
       IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
         CALL TABREM(CLINE)
         NBLC=LEN_TRIM(CLINE)
       ENDIF
       IF(NBLC.EQ.0) THEN
         IFAIL=-1
         RETURN
       END IF
       J=0
5      IF(NW.EQ.NUM) RETURN
       DO 10 I=J+1,NBLC
         IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.&
         (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10     CONTINUE
       IFAIL=1
       RETURN
20     NW=NW+1
       LW(NW)=I
       DO 30 I=LW(NW)+1,NBLC
         IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.&
         (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30     CONTINUE
       RW(NW)=NBLC
       IF(NW.LT.NUM) IFAIL=1
       RETURN
40     RW(NW)=I-1
       J=RW(NW)
       GO TO 5

    END subroutine multisplit

!-----------------------------------------------------------------------------!

  subroutine TABREM(CLINE)

! -- Subroutine TABREM removes tabs from a string.

! -- Subroutine arguments are as follows:-
!       cline:    character string
  
! -- Author:-
!       John Doherty

       INTEGER I
       CHARACTER*(*) CLINE

       DO 10 I=1,LEN(CLINE)
10     IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

       RETURN
  end subroutine tabrem
  
  !-----------------------------------------------------------------------------!

  subroutine intread(CLINE,iTEMP,IFAIL)
      integer, intent(out)            ::IFAIL
      character (len=*), intent(in)   ::cline
      integer, intent(out)            ::iTEMP

! -- Subroutine intREAD reads an integer number from a string.

! -- Subroutine arguments are as follows:-
!       cline:    character string
!       itemp:    return integer
!       ifail:    returned as non-zero in case of failure
      
! -- Author:-
!       John Doherty

       CHARACTER*6 AFMT

       IFAIL=0
       AFMT='(i   )'
       WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
       READ(CLINE,AFMT,ERR=100) iTEMP

       RETURN

100    IFAIL=1
       RETURN
  end subroutine intread

end module ditch_module