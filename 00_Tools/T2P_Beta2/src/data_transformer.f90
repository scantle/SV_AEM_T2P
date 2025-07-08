module m_data_transformer

  implicit none
!-------------------------------------------------------------------------------------------------!
  type t_data_transformer
    real                                          :: param
    procedure(transform), pointer,nopass, private :: transform_ptr => null()
    contains
      procedure,public                              :: set_transform
      procedure,public                              :: apply
  end type t_data_transformer
  
  abstract interface
    real function transform(x, param)
      real, intent(in) :: x, param
    end function transform
  end interface
!-------------------------------------------------------------------------------------------------!
contains
!-------------------------------------------------------------------------------------------------!
! DATA TRANSFORM FUNCTIONS
!-------------------------------------------------------------------------------------------------!
  real     function identity_transform(x, param)  ! Returns the input unchanged
    real    , intent(in) :: x, param
    identity_transform = x
  end function identity_transform
!-------------------------------------------------------------------------------------------------!
  real     function log10_transform(x, param)
    implicit none
    real    , intent(in) :: x, param
    log10_transform = log10(x)
  end function log10_transform
!-------------------------------------------------------------------------------------------------!
  real     function ln_transform(x, param)
    implicit none
    real    , intent(in) :: x, param
    ln_transform = log(x)
  end function ln_transform
!-------------------------------------------------------------------------------------------------!
  real     function sqrt_transform(x, param)
    implicit none
    real    , intent(in) :: x, param
    sqrt_transform = sqrt(x)
  end function sqrt_transform
!-------------------------------------------------------------------------------------------------!
  real     function exp_transform(x, param)
    implicit none
    real    , intent(in) :: x, param
    exp_transform = exp(x)
  end function exp_transform
!-------------------------------------------------------------------------------------------------!
  real     function inverse_transform(x, param)
    implicit none
    real    , intent(in) :: x, param
    inverse_transform = 1.0 / x
  end function inverse_transform
!-------------------------------------------------------------------------------------------------!
  real     function power_transform(x, param)
    implicit none
    real    , intent(in) :: x, param
    power_transform = x**param
  end function power_transform
!-------------------------------------------------------------------------------------------------!
! MODULE PROCEDURES
!-------------------------------------------------------------------------------------------------!
  subroutine set_transform(this, string, param_string)  ! Can implement non-string version if necessary
    use m_vstring, only: t_vstring, vstring_toupper, vstring_cast
    use m_error_handler, only: error_handler
    implicit none
    class(t_data_transformer), intent(inout) :: this
    type(t_vstring),intent(in)               :: string
    type(t_vstring),intent(in),optional      :: param_string
    character(20)                            :: transform_type
    
    call vstring_cast(vstring_toupper(string), transform_type)
    if (present(param_string)) call vstring_cast(param_string, this%param)
    select case (trim(transform_type))
      case('NONE')
        this%transform_ptr => identity_transform
      case('LOG10')
        this%transform_ptr => log10_transform
      case('LN','LOGNAT')
        this%transform_ptr => ln_transform
      case('SQRT')
        this%transform_ptr => sqrt_transform
      case('EXP')
        this%transform_ptr => exp_transform
      case('INV','INVERSE')
        this%transform_ptr => inverse_transform
      case('POWER')
        this%transform_ptr => power_transform
      case DEFAULT
        call error_handler(1,opt_msg="Invalid Data Transformation: "//transform_type)
    end select

  end subroutine set_transform
!-------------------------------------------------------------------------------------------------!
  subroutine apply(this, values)
    implicit none
    class(t_data_transformer), intent(inout) :: this
    real    , intent(inout)                  :: values(:)
    integer                                  :: i
    do i=1, size(values)
      values(i) = this%transform_ptr(values(i), this%param)
    end do
  end subroutine apply
!-------------------------------------------------------------------------------------------------!
end module m_data_transformer