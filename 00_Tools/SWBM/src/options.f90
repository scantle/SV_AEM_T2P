module m_options
!-------------------------------------------------------------------------------------------------!  
! HOW TO ADD AN OPTION
!  1. Add it as a variable in the type t_options
!  2. Add it to the initialize routine and give it a default value
!  3. Create a new case for it in the read_options routine. Write code to handle assignment.
!  4. Add it to the write_options_to_log routine
!-------------------------------------------------------------------------------------------------!
  type t_options
    logical             :: INFLOW_IS_VOL         ! Inflow is in volumetric units
    logical             :: DAILY_SW              ! Write daily sw (SFR tabfiles)
    logical             :: WRITE_UCODE           ! Write UCODE template files
    logical             :: WRITE_PEST            ! Write PEST template files
    logical             :: WRITE_MODFLOW         ! Write MODFLOW files
    integer             :: ABSOLUTE_IRR_DATE(2)  ! Month, Day of absolute irrigation start
    integer             :: NEIGHBOR_RULE         ! # of fields irrigating before everyone starts irrigating
    
    contains
      procedure, public :: initialize
      procedure, public :: read_options
      procedure, public :: write_options_to_log
    
  end type t_options
  
!-------------------------------------------------------------------------------------------------!
  
  contains

!-------------------------------------------------------------------------------------------------!

  subroutine initialize(this)
    implicit none
    class(t_options),intent(inout)         :: this
    
    this%INFLOW_IS_VOL       = .false.
    this%DAILY_SW            = .true.
    this%WRITE_UCODE         = .false.
    this%WRITE_PEST          = .false.
    this%WRITE_MODFLOW       = .true.
    this%ABSOLUTE_IRR_DATE   = (/999,999/)
    this%NEIGHBOR_RULE       = huge(1)
    
  end subroutine initialize

!-------------------------------------------------------------------------------------------------!
  
  subroutine read_options(this, reader)
    use m_file_io, only: t_file_reader, item2int, item2char, item2real
    use m_vstringlist, only: t_vstringlist
    use m_error_handler, only: error_handler
    implicit none
    class(t_options)             :: this
    type(t_file_reader), pointer :: reader
    integer                      :: status, length
    character(30)                :: id, temp
    type(t_vstringlist)          :: strings

    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("INFLOW_IS_VOL")
          this%INFLOW_IS_VOL          = .true.
        case("DAILY_SW")
          this%DAILY_SW               = .true.
        case("WRITE_UCODE")
          this%WRITE_UCODE            = .true.
        case("WRITE_PEST")
          this%WRITE_PEST             = .true.
        case("WRITE_MODFLOW")
          this%WRITE_MODFLOW          = .true.
        case("ABSOLUTE_IRR_DATE")
          this%ABSOLUTE_IRR_DATE(1)   = item2int(strings,2)
          this%ABSOLUTE_IRR_DATE(2)   = item2int(strings,3)
        case("NEIGHBOR_RULE") 
          this%NEIGHBOR_RULE          = item2int(strings,2)
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Option Name: " // trim(id))
      end select
    end do
    
  end subroutine read_options
!-------------------------------------------------------------------------------------------------!
  
  subroutine write_options_to_log(this, log_unit)
    implicit none
    class(t_options)             :: this
    integer,intent(in)           :: log_unit
    
    write(log_unit, '(A)') '*------- Options -------*'
    write(log_unit, '(A20,L5)')  "INFLOW_IS_VOL",     this%INFLOW_IS_VOL
    write(log_unit, '(A20,L5)')  "DAILY_SW",          this%DAILY_SW         
    write(log_unit, '(A20,L5)')  "WRITE_UCODE",       this%WRITE_UCODE      
    write(log_unit, '(A20,L5)')  "WRITE_PEST",        this%WRITE_PEST       
    write(log_unit, '(A20,L5)')  "WRITE_MODFLOW",     this%WRITE_MODFLOW    
    write(log_unit, '(A20,2i3)') "ABSOLUTE_IRR_DATE", this%ABSOLUTE_IRR_DATE
    write(log_unit, '(A20,i6)')  "NEIGHBOR_RULE",     this%NEIGHBOR_RULE    
  
  end subroutine write_options_to_log
  
!-------------------------------------------------------------------------------------------------!
  
end module m_options
!-------------------------------------------------------------------------------------------------!