! $Id: lngamma.f90 3551 2013-02-25 09:51:28Z idculv $

RECURSIVE FUNCTION lngamma(x) RESULT (lg)

!****f* Functions/lngamma *
!
! NAME
!   lngamma - natural logarithm of gamma function.
!
! SYNOPSIS
!   CALL lngamma(x)
!
! DESCRIPTION
!   Calculate log(|gamma|) using Lanczos' method for x > 0,
!   as described in Numerical Recipes.
!   Use Euler's reflection formula for x < 0.
!
! NOTES
!   Lanczos' method is a modification of Stirling's formula.
!
! REFERENCES
!   (1) Lanczos, C. 1964, A Precision Approximation of the Gamma Function,
!   SIAM Journal on Numerical Analysis, ser. B, vol. 1, pp. 86-96.
!   (2) Press et. al., Numerical Recipes in Fortran, 2nd edition, Sec 6.1.
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE messages

  IMPLICIT NONE

  REAL(wp), INTENT(in)                :: x
  REAL(wp)                            :: lg

  REAL(wp), PARAMETER                 :: pi=3.14159265358979323846_wp
  REAL(wp), PARAMETER                 :: sqrt2pi=2.5066282746310005_wp

  REAL(wp), DIMENSION(7), PARAMETER   :: coeff = (/ &
                                                    1.000000000190015_wp, &
                                                   76.18009172947146_wp, &
                                                  -86.50532032941677_wp, &
                                                   24.01409824083091_wp, &
                                                   -1.231739572450155_wp, &
                                                    0.1208650973866179E-2_wp, &
                                                   -0.5395239384953E-5_wp &
                                                  /)

  INTEGER                             :: i
  REAL(wp)                            :: series
  REAL(wp)                            :: y, tmp
  CHARACTER(LEN=12)                   :: sx
  CHARACTER(LEN=256)                  :: routine ! For messaging

!-------------------------------------------------------------------------------
! 2. Define routine name for messaging
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('lngamma')

!--------------------------------------------------------------------------
! 3. Bail out if x e {..., -3, -2, -1, 0}
!--------------------------------------------------------------------------

  lg = -999.0_wp ! closest thing ropp_utils has to a missing data indicator

  IF ( ABS(x - NINT(x)) < 1.E-06_wp .AND. x <= 0.0_wp ) THEN
    WRITE (sx, '(1PE12.5)') x
    CALL message( msg_error, 'Gamma(' // TRIM(ADJUSTL(sx)) // ') not defined' )
    RETURN
  ENDIF

!--------------------------------------------------------------------------
! 4. Call again if x < 0
!--------------------------------------------------------------------------

  IF ( x < 0.0_wp ) THEN
    lg = LOG(-pi / x / ABS(SIN(pi*x))) - lngamma(-x)
    RETURN
  ENDIF

!--------------------------------------------------------------------------
! 5. Calculate series
!--------------------------------------------------------------------------

  y = x

  series = coeff(1)

  DO i=2,SIZE(coeff)
    y = y + 1.0_wp
    series = series + (coeff(i)/y)
  ENDDO

!--------------------------------------------------------------------------
! 6. Calculate multipliers
!--------------------------------------------------------------------------

  tmp = x + 5.5_wp
  tmp = (x + 0.5_wp) * LOG(tmp) - tmp

!--------------------------------------------------------------------------
! 7. Calculate ln(gamma(x))
!--------------------------------------------------------------------------

  lg = tmp + LOG(sqrt2pi * series / x)

!-------------------------------------------------------------------------------
! 8. Clean up
!-------------------------------------------------------------------------------

  CALL message_set_routine(routine)

END FUNCTION lngamma
