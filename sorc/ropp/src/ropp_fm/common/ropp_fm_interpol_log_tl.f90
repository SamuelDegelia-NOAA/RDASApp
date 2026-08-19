! $Id$

!****s* Interpolation/ropp_fm_interpol_log_tl *
!
! NAME
!    ropp_fm_interpol_log_tl - Tangent linear of ropp_fm_interpol_log().
!
! SYNOPSIS
!    call ropp_fm_interpol_log_tl(x, newx, array, x_tl, array_tl, interp_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_interpol_log.
!
! INPUTS
!    real(wp), dim(:) :: x          Coordinate values.
!    real(wp), dim(:) :: newx       New coordinate values.
!    real(wp), dim(:) :: array      Data to be interpolated (lives on x).
!    real(wp), dim(:) :: x_tl       Perturbation in x.
!    real(wp), dim(:) :: array_tl   Perturbations in array.
!
! OUTPUT
!    real(wp), dim(:) :: interp_ad
!
! NOTES
!    Array must be strictly positive.
!
!    The coordinate array x must be strictly monotonically increasing. If
!    elements of newx are outside the range of x, data will be extrapolated.
!
!    None of the above conditions are checked for, but wrong or unexpected
!    results will be obtained if one of them is not met. 
!
! SEE ALSO
!    ropp_fm_interpol_log
!    ropp_fm_interpol_log_ad
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

SUBROUTINE ropp_fm_interpol_log_tl(x, newx, array, x_tl, array_tl, interp_tl)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)  :: x
  REAL(wp), DIMENSION(:), INTENT(in)  :: newx
  REAL(wp), DIMENSION(:), INTENT(in)  :: array
  REAL(wp), DIMENSION(:), INTENT(in)  :: x_tl
  REAL(wp), DIMENSION(:), INTENT(in)  :: array_tl
  REAL(wp), DIMENSION(:), INTENT(out) :: interp_tl

  REAL(wp), DIMENSION(:), ALLOCATABLE :: interp
  REAL(wp), DIMENSION(:), ALLOCATABLE :: larray
  REAL(wp), DIMENSION(:), ALLOCATABLE :: larray_tl

  INTEGER                             :: i, j, k

!-------------------------------------------------------------------------------
! 2. Tangent linear statements
!-------------------------------------------------------------------------------

  ALLOCATE(interp(SIZE(newx)))
  ALLOCATE(larray(SIZE(array)))
  ALLOCATE(larray_tl(SIZE(array)))
  
  larray_tl = array_tl / array

  larray = LOG(array)

  DO k = 1, SIZE(newx)
     j = 2
     DO WHILE (j < SIZE(x) .AND. x(j) < newx(k) )
        j = j + 1
     END DO
     i = j - 1

     interp(k)  = larray(j) + &
                   (newx(k) - x(j)) / (x(i) - x(j)) * (larray(i) - larray(j))
     
     interp_tl(k) = larray_tl(i) * ((newx(k)-x(j))/(x(i)-x(j))) &
                      + larray_tl(j) * (1-(newx(k)-x(j))/(x(i)-x(j))) &
                      + ((larray(i)-larray(j))/((x(i)-x(j))**2)) &
                      *((newx(k)-x(i))*x_tl(j) - (newx(k)-x(j))*x_tl(i) )

  END DO

  interp_tl = interp_tl * EXP(interp)

  DEALLOCATE(interp)
  DEALLOCATE(larray)
  DEALLOCATE(larray_tl)

END SUBROUTINE ropp_fm_interpol_log_tl


