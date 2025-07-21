module t_time
!-------------------------------------------------------------------------------------------------!  
  implicit none

  public :: write_time_start, start_timer, get_elapsed_time

  integer :: start_time
  
!-------------------------------------------------------------------------------------------------!  
contains
!-------------------------------------------------------------------------------------------------!  

  subroutine write_time_start(unit, pretext)
    implicit none
    integer,intent(in) :: unit
    character(*)       :: pretext
    integer            :: values(8)
    !integer            :: iso_year, iso_month, iso_day, iso_hour, iso_minute, iso_second, iso_millisecond

    call date_and_time(values=values)
    write(unit,'(a,1x,i4,5(a1,i0.2))'), trim(pretext), values(1), "-", values(2), "-", values(3), " ", values(5), ":", values(6), ":", values(7)
  end subroutine write_time_start
  
!-------------------------------------------------------------------------------------------------!  

  subroutine start_timer()
    implicit none
    integer :: current_count ! Corrected to integer type

    call system_clock(count=current_count)
    start_time = current_count
  end subroutine start_timer

!-------------------------------------------------------------------------------------------------!  
  
  function get_elapsed_time() result(elapsed)
    implicit none
    real    :: current_time, elapsed
    integer :: count_rate, current_count

    call system_clock(count=current_count, count_rate=count_rate)
    elapsed = real(current_count - start_time) / real(count_rate)

  end function get_elapsed_time

!-------------------------------------------------------------------------------------------------!  
end module t_time
