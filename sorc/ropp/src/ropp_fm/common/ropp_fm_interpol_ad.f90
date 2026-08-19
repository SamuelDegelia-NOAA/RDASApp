! $Id$

SUBROUTINE ropp_fm_interpol_ad(x, newx, array, x_ad, array_ad, interp_ad)

!****s* Interpolation/ropp_fm_interpol_ad *
!
! NAME
!    ropp_fm_interpol_ad - Adjoint of ropp_fm_interpol().
!
! SYNOPSIS
!    call ropp_fm_interpol_ad(x, newx, array, x_ad, array_ad, interp_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_interpol.
!
! INPUTS
!    real(wp), dim(:) :: x          Coordinate values.
!    real(wp), dim(:) :: newx       New coordinate values.
!    real(wp), dim(:) :: array      Data to be interpolated (lives on x).
!    real(wp), dim(:) :: interp_ad  Adjoint forcing
!
! OUTPUT
!    real(wp), dim(:) :: x_ad
!    real(wp), dim(:) :: array_ad
!
! NOTES
!    The coordinate array x must be strictly monotonically increasing. If
!    elements of newx are outside the range of x, data will be extrapolated.
!
!    None of the above conditions are checked for, but wrong or unexpected
!    results will be obtained if one of them is not met. 
!
! SEE ALSO
!    ropp_fm_interpol
!    ropp_fm_interpol_tl
!
!    The routine should be tested with the program test_interpol_ad.
!
! REFERENCES
!    This routine was created with the help of the 
!
!      Tangent linear and Adjoint Model Compiler,  TAMC 5.3.0.
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

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)    :: x
  REAL(wp), DIMENSION(:), INTENT(in)    :: newx
  REAL(wp), DIMENSION(:), INTENT(in)    :: array
  REAL(wp), DIMENSION(:), INTENT(inout) :: x_ad
  REAL(wp), DIMENSION(:), INTENT(inout) :: array_ad
  REAL(wp), DIMENSION(:), INTENT(inout) :: interp_ad

  INTEGER                               :: i, j, k

!-------------------------------------------------------------------------------
! 2. Adjoint computations
!-------------------------------------------------------------------------------

  DO k = 1, SIZE(newx)
     j = 2
     DO WHILE (j < SIZE(x) .AND. x(j) < newx(k) )
        j = j + 1
     END DO
     i = j - 1

     array_ad(i) = array_ad(i) + interp_ad(k) * ((newx(k)-x(j))/(x(i)-x(j)))
     array_ad(j) = array_ad(j) +    &
                     interp_ad(k) * (1.0_wp-(newx(k)-x(j))/(x(i)-x(j)))

     x_ad(i) = x_ad(i) - interp_ad(k)                        &
                  * (newx(k)-x(j))/((x(i)-x(j))*(x(i)-x(j))) &
                  * (array(i)-array(j))
     x_ad(j) = x_ad(j) + interp_ad(k)                        &
                  * ((-1)/(x(i)-x(j))+(newx(k)-x(j)) /       &
                  ((x(i)-x(j))*(x(i)-x(j)))) * (array(i)-array(j))
     interp_ad(k) = 0.

  END DO

END SUBROUTINE ropp_fm_interpol_ad
