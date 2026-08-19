! $Id: ropp_fm_asinh.f90 4010 2014-01-10 11:07:40Z idculv $

!****f* Ionosphere/ropp_fm_asinh
!
! NAME
!   ropp_fm_asinh  - compute asinh(x) for any real x.
!
! SYNOPSIS
!   a = ropp_fm_asinh(x)
!
! DESCRIPTION
!   The inverse hyperbolic sine of x, asinh(x), is calculated according to 
!   its asymptotic forms for large and small |x|, and exactly in between.
!
! INPUTS
!    x
!
! OUTPUT
!    a = asinh(x)
!
! NOTES
!
! SEE ALSO
!   ropp_fm_zorro
!   ropp_fm_dzorro_dlg
!
! REFERENCES
!
! AUTHOR
!   Met Office, Exeter, UK and ECMWF, Reading, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

FUNCTION ropp_fm_asinh(x) RESULT(asinh)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE

  REAL(wp), DIMENSION(:)              :: x
  REAL(wp), DIMENSION(SIZE(x))        :: asinh

!-------------------------------------------------------------------------------
! 2. Calculations
!-------------------------------------------------------------------------------

  WHERE (ABS(x) < 1.0e-10_wp)
    asinh = x
  ELSEWHERE (ABS(x) > 1.0e7_wp)
    asinh = SIGN(1.0_wp, x)*LOG(2.0_wp*ABS(x))
  ELSEWHERE
    asinh = LOG(x + SQRT(1.0_wp + x*x))
  ENDWHERE

  RETURN

END FUNCTION ropp_fm_asinh
