! $Id$

SUBROUTINE ropp_fm_interpol(x, newx, array, interp)

!****s* Interpolation/ropp_fm_interpol *
!
! NAME
!    ropp_fm_interpol - Interpolate linearly.
!
! SYNOPSIS
!    call ropp_fm_interpol(x, newx, array, interp)
! 
! DESCRIPTION
!    This subroutine interpolates an array assuming it varies linearly in x.
!
! INPUTS
!    real(wp), dim(:) :: x       Coordinate values.
!    real(wp), dim(:) :: newx    New coordinate values.
!    real(wp), dim(:) :: array   Data to be interpolated (lives on x).
!
! OUTPUT
!    real(wp), dim(:) :: interp  Interpolated data (lives on newx).
!
! NOTES
!    The coordinate array x must be strictly monotonically increasing. If
!    elements of newx are outside the range of x, data will be extrapolated.
!
!    None of the above conditions are checked for, but wrong or unexpected
!    results will be obtained if one of them is not met. 
!
! SEE ALSO
!    ropp_fm_interpol_ad
!    ropp_fm_interpol_tl
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

  REAL(wp), DIMENSION(:), INTENT(in)  :: x
  REAL(wp), DIMENSION(:), INTENT(in)  :: newx
  REAL(wp), DIMENSION(:), INTENT(in)  :: array
  REAL(wp), DIMENSION(:), INTENT(out) :: interp

  INTEGER                             :: i, j, k

!-------------------------------------------------------------------------------
! 2. Do the interpolation
!-------------------------------------------------------------------------------

  DO k = 1, SIZE(newx)
     j = 2
     DO WHILE (j < SIZE(x) .AND. x(j) < newx(k))
        j = j + 1
     ENDDO
     i = j - 1

     interp(k) = array(j) + &
          ( (newx(k) - x(j)) / (x(i) - x(j)) * (array(i) - array(j)) )

  ENDDO

END SUBROUTINE ropp_fm_interpol
