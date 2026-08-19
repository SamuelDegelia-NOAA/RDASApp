! $Id$

!****f* Arrays/unit_vector *
!
! NAME
!    unit_vector
!
! SYNOPSIS
!    uvector = unit_vector(vector)
! 
! DESCRIPTION
!    This function calculates the unit vector of a 1D vector.
!
! INPUTS
!    real, dim(:) :: vector   1D array to be normalized.
!
! RESULT
!    real, dim(:) :: uvector  Normalized version of vector, i.e.
!                               vector divided by its norm.
!
! NOTES
!    Vector can be a float, double, complex or double complex
!    array; the calculated unit vector will be of the same type.
!
! EXAMPLE
!    To calculate a unit vector from an arbitray vector in physical
!    space (like a coordinate vector), try
!
!      [use tools90|use arrays]
!        ...
!      real(wp), dimension(3) :: vec, uvec
!        ...
!      vec  = ...
!      uvec = unit_vector(vec)
!
! SEE ALSO
!      l2norm
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!-----------------------------------------------------------------------
! 1. Float
!-----------------------------------------------------------------------

function unit_vector_float(vector) result (uvector)

! Declarations
! ------------

  use typeSizes, only: wp => FourByteReal
  use arrays,    only: l2norm

  implicit none

  real(wp), dimension(:), intent(in) :: vector
  real(wp), dimension(size(vector))  :: uvector

! Calculate the unit vector
! -------------------------

  uvector = vector / l2norm(vector)

end function unit_vector_float


!-----------------------------------------------------------------------
! 2. Double
!-----------------------------------------------------------------------

function unit_vector_double(vector) result (uvector)

! Declarations
! ------------

  use typeSizes, only: wp => EightByteReal
  use arrays,    only: l2norm

  implicit none

  real(wp), dimension(:), intent(in) :: vector
  real(wp), dimension(size(vector))  :: uvector

! Calculate the unit vector
! -------------------------

  uvector = vector / l2norm(vector)

end function unit_vector_double


!-----------------------------------------------------------------------
! 3. Complex
!-----------------------------------------------------------------------

function unit_vector_complex(vector) result (uvector)

! Declarations
! ------------

  use typeSizes, only: wp => FourByteReal
  use arrays,    only: l2norm

  implicit none

  complex(wp), dimension(:), intent(in) :: vector
  complex(wp), dimension(size(vector))  :: uvector

! Calculate the unit vector
! -------------------------

  uvector = vector / l2norm(vector)

end function unit_vector_complex


!-----------------------------------------------------------------------
! 4. Double complex
!-----------------------------------------------------------------------

function unit_vector_doublecomplex(vector) result (uvector)

! Declarations
! ------------

  use typeSizes, only: wp => EightByteReal
  use arrays,    only: l2norm

  implicit none

  complex(wp), dimension(:), intent(in) :: vector
  complex(wp), dimension(size(vector))  :: uvector

! Calculate the unit vector
! -------------------------

  uvector = vector / l2norm(vector)

end function unit_vector_doublecomplex
