! $Id$

!****f* Arrays/outer_product *
!
! NAME
!    outer_product - Calculate the outer product of two vectors.
!
! SYNOPSIS
!    z = outer_product(x, y)
! 
! DESCRIPTION
!    This function returns the outer (dyadic) product between two
!    1D vectors.
!
! INPUTS
!    real(wp), dimension(:)   :: x, y
!
! OUTPUT
!    real(wp), dimension(:,:) :: z
!
! NOTES
!    The result of an outer product of two 1d vectors is matrix!
!    Both float and double arguments are supported.
!
! EXAMPLE
!    To calculate the dyadic (outer) product of vectors representing
!    the x- and y-coordinate axis, try
!
!      use arrays
!        ...
!      real(wp), dimension(3)   :: x, y
!      real(wp), dimension(3,3) :: A
!        ...
!      x = (/ 1., 0., 0./)
!      y = (/ 0., 1., 0./)
!        ...
!      A = outer_product(x, y)
!
! SEE ALSO
!      cross_product
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

function outer_product_float(x, y) result(z)

! Declarations
! ------------

  use typeSizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension(:), intent(in)    :: x, y
  real(wp), dimension(size(x), size(y)) :: z

  integer                               :: i, j

! Calculate the outer product
! ---------------------------

  do i = 1, size(x)
     do j = 1, size(y)
        z(i,j) = x(i) * y(j)
     enddo
  enddo

end function outer_product_float


function outer_product_double(x, y) result(z)

! Declarations
! ------------

  use typeSizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(:), intent(in)    :: x, y
  real(wp), dimension(size(x), size(y)) :: z

  integer                               :: i, j

! Calculate the outer product
! ---------------------------

  do i = 1, size(x)
     do j = 1, size(y)
        z(i,j) = x(i) * y(j)
     enddo
  enddo

end function outer_product_double
