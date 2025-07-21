module m_error_handler
  use m_vstringlist, only: t_vstringlist
  implicit none
!-------------------------------------------------------------------------------------------------!
  ! Module variables
  type(t_vstringlist)              :: warnings
  
!-------------------------------------------------------------------------------------------------!

  contains

!-------------------------------------------------------------------------------------------------!
  subroutine early_exit()
    implicit none
    
    write(*,'(a)') 'SWBM Early Termination.'
    !call log%write_line('SWBM Early Termination')
    !call log%close_file()
    error stop
    
  end subroutine early_exit
    
!-------------------------------------------------------------------------------------------------!
  
  subroutine normal_exit()
    use m_file_io, only: close_file
    implicit none
    
    write(*,'(a)') 'SWBM Normal Termination.'
    !call log%write_line('SWBM Normal Termination')
    !call log%close_file()
    
  end subroutine normal_exit
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine error_handler(err,filename,opt_msg)
    implicit none
    integer, intent(in)              :: err
    character(*),intent(in),optional :: filename
    character(*),intent(in),optional :: opt_msg
    character(200)                   :: err_msg
    
    ! Start message
    err_msg = "Error -"
    
    ! All available errors
    select case(err)
      case(0) ! Not an error
        ! Important to cover all your bases :)
        return ! Returns with no error
      case(1) ! Generic Input Error
        err_msg = trim(err_msg) // " Invalid Input -"
      case(2) ! Missing Data Error
        err_msg = trim(err_msg) // " Missing Required Data -"
      case(3) ! Invalid data, unable to convert
        err_msg = trim(err_msg) // " Data Format/Conversion Failure -"
      case(4) ! Interpolation error... an error that happened during interpolation
        err_msg = trim(err_msg) // " Interpolator Error -"
      case(9) ! Internal Error ("my bad")
        err_msg = trim(err_msg) // " Internal Error -"
      case DEFAULT
        write(*,*) "An unknown error has occured. Please contact the developer."
    end select
    
    ! Add optional stuff
    if (present(opt_msg)) then
      err_msg = trim(err_msg) //" "// trim(opt_msg) // "."
    end if
    if (present(filename)) then
      err_msg = trim(err_msg) // " File: " // trim(filename)
    end if
    
    ! Write the error
    write(*,'(a)') err_msg
    !call log%write_line(err_msg, blank_start=1)
    
    ! Call early_exit handler
    call early_exit()
  
  end subroutine error_handler
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine warning_handler(warning)
    use m_vstringlist, only: vstrlist_append
    implicit none
    character(*), intent(in)         :: warning
    
    call vstrlist_append(warnings, warning)
  
  end subroutine warning_handler

!-------------------------------------------------------------------------------------------------!

end module m_error_handler