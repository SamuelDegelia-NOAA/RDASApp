! $Id$

!****s* Arrays/swap *
!
! NAME
!    swap - Swap the two arguments.
!
! SYNOPSIS
!    call swap(a, b)
! 
! DESCRIPTION
!    This subroutine swaps the content of the two arguments. This is an
!    elemental subroutine.
!
! INPUTS
!    a   -  First argument, to be swapped with...
!    b   -  ... second argument.
!
! OUTPUT
!    a   -  is now the former b.
!    b   -  is now the former a.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!--------------------------------------------------------------------------
! 1. Integer
!--------------------------------------------------------------------------

elemental subroutine swap_int(a, b)

  use arrays, not_this => swap_int

  implicit none

  integer, intent(inout) :: a
  integer, intent(inout) :: b

  integer                :: tmp

  tmp = a
  a   = b
  b   = tmp

end subroutine swap_int

!--------------------------------------------------------------------------
! 2. Float
!--------------------------------------------------------------------------

elemental subroutine swap_float(a, b)

  use typesizes, only: wp => FourByteReal
  use arrays, not_this => swap_float

  implicit none

  real(wp), intent(inout) :: a
  real(wp), intent(inout) :: b

  real(wp)                :: tmp

  tmp = a
  a   = b
  b   = tmp

end subroutine swap_float

!--------------------------------------------------------------------------
! 3. Double
!--------------------------------------------------------------------------

elemental subroutine swap_double(a, b)

  use typesizes, only: wp => EightByteReal
  use arrays, not_this => swap_double

  implicit none

  real(wp), intent(inout) :: a
  real(wp), intent(inout) :: b

  real(wp)                :: tmp

  tmp = a
  a   = b
  b   = tmp

end subroutine swap_double

!--------------------------------------------------------------------------
! 4. Complex float
!--------------------------------------------------------------------------

elemental subroutine swap_complex_float(a, b)

  use typesizes, only: wp => FourByteReal
  use arrays, not_this => swap_complex_float

  implicit none

  complex(wp), intent(inout) :: a
  complex(wp), intent(inout) :: b

  complex(wp)                :: tmp

  tmp = a
  a   = b
  b   = tmp

end subroutine swap_complex_float

!--------------------------------------------------------------------------
! 3. Complex double
!--------------------------------------------------------------------------

elemental subroutine swap_complex_double(a, b)

  use typesizes, only: wp => EightByteReal
  use arrays, not_this => swap_complex_double

  implicit none

  complex(wp), intent(inout) :: a
  complex(wp), intent(inout) :: b

  complex(wp)                :: tmp

  tmp = a
  a   = b
  b   = tmp

end subroutine swap_complex_double

