! $Id$

SUBROUTINE ropp_fm_interpol_tl(x, newx, array, x_tl, array_tl, interp_tl)

!****f* Interpolation/ropp_interpol_tl *
!
! NAME
!    ropp_fm_interpol_tl - Tangent linear of ropp_fm_interpol().
!
! SYNOPSIS
!    call ropp_fm_interpol_tl(x, newx, array, x_tl, array_tl, interp_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_interpol.
!
! INPUTS
!    real(wp), dim(:) :: x          Coordinate values.
!    real(wp), dim(:) :: newx       New coordinate values.
!    real(wp), dim(:) :: array      Data to be interpolated (lives on x).
!    real(wp), dim(:) :: x_tl       Perturbation in x.
!    real(wp), dim(:) :: array_tl   Perturbations in array.
!
! OUTPUT
!    real(wp), dim(:) :: interp_ad  Interpolated data adjoint
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
!    ropp_fm_interpol_ad
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

  REAL(wp), DIMENSION(:) :: x
  REAL(wp), DIMENSION(:) :: newx
  REAL(wp), DIMENSION(:) :: array
  REAL(wp), DIMENSION(:) :: x_tl
  REAL(wp), DIMENSION(:) :: array_tl
  REAL(wp), DIMENSION(:) :: interp_tl

  INTEGER                :: i, j, k

!-------------------------------------------------------------------------------
! 2. Tangent linear statements
!-------------------------------------------------------------------------------

  DO k = 1, SIZE(newx)
     j = 2
     DO WHILE (j < SIZE(x) .AND. x(j) < newx(k) )
        j = j + 1
     END DO
     i = j - 1
     interp_tl(k) = array_tl(j) * (1.0_wp-(newx(k)-x(j))/(x(i)-x(j)))   &
                      + array_tl(i) * ((newx(k)-x(j))/(x(i)-x(j)))      &
                      + x_tl(j) * ((-1.0_wp)/(x(i)-x(j))                &
                      + (newx(k)-x(j)) / ((x(i)-x(j))*(x(i)-x(j)))) *   &
                      (array(i)-array(j))                               &
                      - x_tl(i) * (newx(k)-x(j)) /                      &
                      ((x(i)-x(j))*(x(i)-x(j))) * (array(i)-array(j))
  END DO

END SUBROUTINE ropp_fm_interpol_tl
