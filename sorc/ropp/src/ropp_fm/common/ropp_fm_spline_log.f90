! $Id$

SUBROUTINE ropp_fm_spline_log(x, newx, array, interp)

!****s* Interpolation/ropp_fm_spline_log *
!
! NAME
!    ropp_fm_spline_log - Interpolate logarithmically along a spline.
!
! SYNOPSIS
!    call ropp_fm_spline_log(x, newx, array, interp)
! 
! DESCRIPTION
!    This subroutine interpolates an array along a spline assuming it varies exponentially 
!    in x, i.e. assuming its log varies as function of x.
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
!    Array must be strictly positive.
!
!    The coordinate array x must be strictly monotonically increasing. If
!    elements of newx are outside the range of x, data will be extrapolated.
!
!    None of the above conditions are checked for, but wrong or unexpected
!    results will be obtained if one of them is not met. 
!
! SEE ALSO
!    ropp_fm_spline_log_ad
!    ropp_fm_spline_log_tl
!
! AUTHOR
!
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
  USE ropp_utils 

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)  :: x
  REAL(wp), DIMENSION(:), INTENT(in)  :: newx
  REAL(wp), DIMENSION(:,:), INTENT(in)  :: array
  REAL(wp), DIMENSION(:), INTENT(out) :: interp

  INTEGER                             :: i, j, k
  INTEGER                             :: khoriz   ! no. of horizontal locations 
  INTEGER                             :: klev     ! number of full model levels
  INTEGER                             :: knewlev  ! no. of interpolated levels.

  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: x_re
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: y2
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: larray

!-------------------------------------------------------------------------------
! 2. Do the interpolation
!-------------------------------------------------------------------------------

  ! 2a preparation for using 1d or 2d code

  knewlev = size(newx, DIM = 1)
  
  klev    = SIZE(array, DIM = 1)
  khoriz  = SIZE(array, DIM = 2)

  ALLOCATE(x_re(klev,khoriz))
  ALLOCATE(y2(klev,khoriz))
  ALLOCATE(larray(klev,khoriz))
 
  x_re     = RESHAPE(x, (/klev,khoriz/))

  y2(:,:)        = ropp_MDFV
  larray(:,:)    = ropp_MDFV
 
  !2b take log 
  DO j = 1, khoriz
    DO i = 1, klev
      IF (array(i,j) > 0.0) THEN
        larray(i,j) = LOG(array(i,j))
      ENDIF
    ENDDO ! i

  !2c initialisation
    CALL ropp_fm_spline_init(klev,x_re(:,j),larray(:,j),y2(:,j))

  ENDDO !j 

  !2d do spline interpolation 
  CALL ropp_fm_splint(klev,knewlev,khoriz,x_re,larray,y2,newx,interp)
 
  !interp = EXP(interp)

  DEALLOCATE(larray)
  DEALLOCATE(x_re)
  DEALLOCATE(y2)

END SUBROUTINE ropp_fm_spline_log
