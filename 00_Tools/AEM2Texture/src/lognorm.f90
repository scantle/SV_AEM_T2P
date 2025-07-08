module lognorm
! Obtained from https://people.sc.fsu.edu/~jburkardt/f_src/log_normal/log_normal.f90
! On 3/14/2023
  implicit none
  
  real(8), parameter  :: pi = 4.D0*DATAN(1.D0)
  
  contains
  
  function lognorm_mu(norm_mu, norm_sigma) result(mu)
    implicit none
    real(kind=8), intent(in) :: norm_mu, norm_sigma
    real(kind=8) :: mu
  
    mu = log(norm_mu**2 / sqrt(norm_sigma**2 + norm_mu**2))
  
  end function lognorm_mu
  
subroutine log_normal_cdf ( x, mu, sigma, cdf )

!*****************************************************************************80
!
!! log_normal_cdf() evaluates the Log Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of the PDF.
!    0.0 < X.
!
!    Input, real ( kind = rk ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = rk ) CDF, the value of the CDF.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) cdf
  real ( kind = rk ) logx
  real ( kind = rk ) mu
  real ( kind = rk ) sigma
  real ( kind = rk ) x

  if ( x <= 0.0D+00 ) then

    cdf = 0.0D+00

  else

    logx = log ( x )

    call normal_cdf ( logx, lognorm_mu(mu, sigma), sigma, cdf ) ! Modified by LS b/c to match scipy input/output

  end if

  return
end

subroutine log_normal_pdf(x, mu, sigma, pdf)
!*****************************************************************************80
!
!! LOG_NORMAL_PDF evaluates the Log Normal PDF.
!
!  Discussion:
!
!    PDF(A,B;X)
!      = exp ( - 0.5 * ( ( log ( X ) - MU ) / SIGMA )^2 )
!        / ( SIGMA * X * sqrt ( 2 * PI ) )
!
!    The Log Normal PDF is also known as the Cobb-Douglas PDF,
!    and as the Antilog_normal PDF.
!
!    The Log Normal PDF describes a variable X whose logarithm
!    is normally distributed.
!
!    The special case MU = 0, SIGMA = 1 is known as Gilbrat's PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of the PDF.
!    0.0 < X
!
!    Input, real ( kind = rk ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = rk ) PDF, the value of the PDF.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) mu
  real ( kind = rk ) pdf

  real ( kind = rk ) sigma
  real ( kind = rk ) x

  if ( x <= 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = exp ( - 0.5D+00 * ( ( log ( x ) - mu ) / sigma ) ** 2 ) &
      / ( sigma * x * sqrt ( 2.0D+00 * pi ) )
  end if

  return
end

subroutine normal_cdf ( x, mu, sigma, cdf )

!*****************************************************************************80
!
!! NORMAL_CDF evaluates the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of the CDF.
!
!    Input, real ( kind = rk ) MU, SIGMA, the parameters of the PDF.
!    0.0 < SIGMA.
!
!    Output, real ( kind = rk ) CDF, the value of the CDF.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) cdf
  real ( kind = rk ) mu
  real ( kind = rk ) sigma
  real ( kind = rk ) x
  real ( kind = rk ) y

  y = ( x - mu ) / sigma

  call normal_01_cdf ( y, cdf )

  return
end

subroutine normal_01_cdf ( x, cdf )

!*****************************************************************************80
!
!! NORMAL_01_CDF evaluates the Normal 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39,
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, pages 197-198, 1969.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of the CDF.
!
!    Output, real ( kind = rk ) CDF, the value of the CDF.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ), parameter :: a1 = 0.398942280444D+00
  real ( kind = rk ), parameter :: a2 = 0.399903438504D+00
  real ( kind = rk ), parameter :: a3 = 5.75885480458D+00
  real ( kind = rk ), parameter :: a4 = 29.8213557808D+00
  real ( kind = rk ), parameter :: a5 = 2.62433121679D+00
  real ( kind = rk ), parameter :: a6 = 48.6959930692D+00
  real ( kind = rk ), parameter :: a7 = 5.92885724438D+00
  real ( kind = rk ), parameter :: b0 = 0.398942280385D+00
  real ( kind = rk ), parameter :: b1 = 3.8052D-08
  real ( kind = rk ), parameter :: b2 = 1.00000615302D+00
  real ( kind = rk ), parameter :: b3 = 3.98064794D-04
  real ( kind = rk ), parameter :: b4 = 1.98615381364D+00
  real ( kind = rk ), parameter :: b5 = 0.151679116635D+00
  real ( kind = rk ), parameter :: b6 = 5.29330324926D+00
  real ( kind = rk ), parameter :: b7 = 4.8385912808D+00
  real ( kind = rk ), parameter :: b8 = 15.1508972451D+00
  real ( kind = rk ), parameter :: b9 = 0.742380924027D+00
  real ( kind = rk ), parameter :: b10 = 30.789933034D+00
  real ( kind = rk ), parameter :: b11 = 3.99019417011D+00
  real ( kind = rk ) cdf
  real ( kind = rk ) q
  real ( kind = rk ) x
  real ( kind = rk ) y
!
!  |X| <= 1.28.
!
  if ( abs ( x ) <= 1.28D+00 ) then

    y = 0.5D+00 * x * x

    q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 / ( y + a5 &
      + a6 / ( y + a7 ) ) ) )
!
!  1.28 < |X| <= 12.7
!
  else if ( abs ( x ) <= 12.7D+00 ) then

    y = 0.5D+00 * x * x

    q = exp ( - y ) * b0 / ( abs ( x ) - b1 &
      + b2 / ( abs ( x ) + b3 &
      + b4 / ( abs ( x ) - b5 &
      + b6 / ( abs ( x ) + b7 &
      - b8 / ( abs ( x ) + b9 &
      + b10 / ( abs ( x ) + b11 ) ) ) ) ) )
!
!  12.7 < |X|
!
  else

    q = 0.0D+00

  end if
!
!  Take account of negative X.
!
  if ( x < 0.0D+00 ) then
    cdf = q
  else
    cdf = 1.0D+00 - q
  end if

  return
end

function lognorm_prob(x, delta, mu, sigma, location) result(prob)
  implicit none
  real(kind=8), intent(in) :: x, delta, mu, sigma, location
  real(kind=8) :: right, left, prob, shifted_x
  
  shifted_x = x - location
  call log_normal_cdf(shifted_x + delta, mu, sigma, right)
  call log_normal_cdf(shifted_x - delta, mu, sigma, left)
  
  prob = right - left
  
end function lognorm_prob

end module lognorm