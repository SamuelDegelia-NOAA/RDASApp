! $Id$

!****f* Arrays/blend *
!
! NAME
!    blend - Sets up weighting array for blending arrays together.
!
! SYNOPSIS
!    w = blend(n, i, j)
! 
! DESCRIPTION
!    This function calculates cosine weights for blending two arrays.
!
! INPUTS
!    n   size of array to make
!    i   index of first value to blend
!    j   index of last value to blend
!
! OUTPUT
!    w   weigthing array (0.0 until i, 1.0 after j, cos-weight in between)
!
! NOTES
!    To blend two arrays A and B:
!
!        blended =  A * (1 - w) + B * w
!
!    The result starts with pure A and ends with pure B.
!
! REFERENCES
!    This function is reimplemented in Fortran 90 from the original
!    IDL function blend.pro written by R. Sterner; this function is
!    is part of the John Hopkins University / Applied Physics
!    Laboratory (JHU/APL) IDL library.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

function blend(n, i, j) result (weights)

  use typesizes, only: wp => EightByteReal

  implicit none

  integer,  intent(in)   :: n
  integer,  intent(in)   :: i
  integer,  intent(in)   :: j
  real(wp), dimension(n) :: weights

  real(wp)               :: pi
  integer                :: ii

!-------------------------------------------------------------------------------
! 2. Set up weighting array
!-------------------------------------------------------------------------------

  pi = 4.0_wp * atan(1.0_wp)

  weights(1:i) = 0.0_wp
  weights(j:)  = pi

  if (i < j) then
     weights(i:j) = (/ (real(ii-i, wp)/real(j-i, wp) * pi, ii = i, j) /)
  else
     weights(i:j) = pi
  endif

  weights = 0.5_wp * (1.0_wp - cos(weights))

end function blend
