! $Id$

!****f* Arrays/cross_product *
!
! NAME
!    cross_product - Calculate the cross product between two vectors.
!
! SYNOPSIS
!    z = cross_product(x, y)
! 
! DESCRIPTION
!    This function returns the cross product between two 1D vectors.
!
! INPUTS
!    real(wp), dimension(3) :: x, y
!
! OUTPUT
!    real(wp), dimension(3) :: z
!
! NOTES
!    This function is obviously only useful for vectors from R^3.
!    Both float and double arguments are supported.
!
! EXAMPLE
!    To calculate the cross product between vectors representing
!    the x- and y-coordinate axis, try
!
!      use arrays
!        ...
!      real(wp), dimension (3) :: x, y, z
!        ...
!      x = (/ 1., 0., 0./)
!      y = (/ 0., 1., 0./)
!        ...
!      z = cross_product(x, y)
!
! SEE ALSO
!    outer_product
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!--------------------------------------------------------------------------
! 1. Float
!--------------------------------------------------------------------------

function cross_product_float(x, y) result(z)

! Declarations
! ------------

  use typeSizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension(3), intent(in) :: x, y
  real(wp), dimension(3)             :: z

! Calculate the cross product
! ---------------------------

  z(1) = x(2)*y(3) - x(3)*y(2)
  z(2) = x(3)*y(1) - x(1)*y(3)
  z(3) = x(1)*y(2) - x(2)*y(1)

end function cross_product_float


!--------------------------------------------------------------------------
! 1. Float
!--------------------------------------------------------------------------

function cross_product_double(x, y) result(z)

! Declarations
! ------------

  use typeSizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(3), intent(in) :: x, y
  real(wp), dimension(3)             :: z

! Calculate the cross product
! ---------------------------

  z(1) = x(2)*y(3) - x(3)*y(2)
  z(2) = x(3)*y(1) - x(1)*y(3)
  z(3) = x(1)*y(2) - x(2)*y(1)

end function cross_product_double
