! $Id$

!****f* Arrays/l2norm *
!
! NAME
!    l2norm - Calculate the L2 (Euclidian) norm of a vector.
!
! SYNOPSIS
!    use arrays
!      ...
!    norm = l2norm(vector)
! 
! DESCRIPTION
!    This function computes the L2 (i.e., the usual Euclidian) norm of
!    a 1D vector.
!
! INPUTS
!    ..., dim(:) :: vector    1D array.
!
! RESULT
!    ...         :: norm      Scalar value of the norm of vector.
!
! NOTES
!    Vector can be a float, double, complex or double complex
!    array, while the norm will always be real (float or double).
!
! EXAMPLE
!    To calculate the norm of an arbitrary vector in physical space
!    (like a coordinate vector), try
!
!      use arrays
!        ...
!      real(wp), dimension(3) :: vec
!      real                   :: length
!        ...
!      vec    = ...
!      length = l2norm(vec)
!
! SEE ALSO
!      unit_vector
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!-----------------------------------------------------------------------
! 1. Float
!-----------------------------------------------------------------------

function l2norm_float(vector) result(norm)

! Declarations
! ------------

  use typeSizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: vector
  real(wp)                           :: norm

! Calculate the norm
! ------------------

  norm = sqrt(dot_product(vector, vector))

end function l2norm_float


!-----------------------------------------------------------------------
! 2. Double
!-----------------------------------------------------------------------

function l2norm_double(vector) result(norm)

! Declarations
! ------------

  use typeSizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: vector
  real(wp)                           :: norm

! Calculate the norm
! ------------------

  norm = sqrt(dot_product(vector, vector))

end function l2norm_double


!-----------------------------------------------------------------------
! 3. Complex
!-----------------------------------------------------------------------

function l2norm_complex(vector) result(norm)

! Declarations
! ------------

  use typeSizes, only: wp => FourByteReal

  implicit none

  complex(wp), dimension(:), intent(in) :: vector
  complex(wp)                           :: norm

! Calculate the norm
! ------------------

  norm = sqrt(dot_product(vector, vector))

end function l2norm_complex


!-----------------------------------------------------------------------
! 2. Double complex
!-----------------------------------------------------------------------

function l2norm_doublecomplex(vector) result(norm)

! Declarations
! ------------

  use typeSizes, only: wp => EightByteReal

  implicit none

  complex(wp), dimension(:), intent(in) :: vector
  complex(wp)                           :: norm

! Calculate the norm
! ------------------

  norm = sqrt(dot_product(vector, vector))

end function l2norm_doublecomplex
