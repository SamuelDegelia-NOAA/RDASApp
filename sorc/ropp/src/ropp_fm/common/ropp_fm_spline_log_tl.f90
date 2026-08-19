! $Id$

SUBROUTINE ropp_fm_spline_log_tl(x, newx, array, x_tl, array_tl, interp_tl)

!****s* Interpolation/ropp_fm_spline_log *
!
! NAME
!    ropp_fm_spline_log_tl - Interpolate logarithmically along a spline.
!
! SYNOPSIS
!    call ropp_fm_spline_log_tl(x, newx, array,x_tl, array_tl, interp_tl)
! 
! DESCRIPTION
!    This subroutine interpolates an array along a spline assuming it varies exponentially 
!    in x, i.e. assuming its log varies as function of x.
!
! INPUTS
!    real(wp), dim(:) :: x       Coordinate values.
!    real(wp), dim(:) :: newx    New coordinate values.
!    real(wp), dim(:) :: array   Data to be interpolated (lives on x).
!    real(wp), dim(:) :: x_tl       Perturbation in x.
!    real(wp), dim(:) :: array_tl   Perturbations in array.
!
! OUTPUT
!    real(wp), dim(:) :: interp_tl  Interpolated data (lives on newx).
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
!    ropp_fm_spline_log
!    ropp_fm_spline_log_tl
!
! AUTHOR
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
  REAL(wp), DIMENSION(:), INTENT(in)  :: x_tl
  REAL(wp), DIMENSION(:), INTENT(in)  :: newx
  REAL(wp), DIMENSION(:,:), INTENT(in)  :: array
  REAL(wp), DIMENSION(:,:), INTENT(in)  :: array_tl
  REAL(wp), DIMENSION(:), INTENT(out)   :: interp_tl

  INTEGER                             :: i, j, k
  INTEGER                             :: khoriz   ! no. of horizontal locations 
  INTEGER                             :: klev     ! number of full model levels
  INTEGER                             :: knewlev  ! no. of interpolated levels.

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: newx_tl             ! obs perturbation
  
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: x_re, x_tl_re
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: y2, y2_tl
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: larray, larray_tl

!-------------------------------------------------------------------------------
! 2. Do the interpolation
!-------------------------------------------------------------------------------

  ! 2a preparation for using 1d or 2d code

  knewlev = size(newx, DIM = 1)
  
  klev    = SIZE(array, DIM = 1)
  khoriz  = SIZE(array, DIM = 2)

  ALLOCATE(x_re(klev,khoriz))
  ALLOCATE(x_tl_re(klev,khoriz))
  ALLOCATE(y2(klev,khoriz))
  ALLOCATE(y2_tl(klev,khoriz))
  ALLOCATE(larray(klev,khoriz))
  ALLOCATE(larray_tl(klev,khoriz))
  ALLOCATE(newx_tl(knewlev))
 
  x_re     = RESHAPE(x, (/klev,khoriz/))
  x_tl_re  = RESHAPE(x_tl, (/klev,khoriz/))

  y2(:,:)    = ropp_MDFV
  y2_tl(:,:) = 0.0_wp
  
  larray(:,:)    = ropp_MDFV
  larray_tl(:,:) = 0.0_wp

  newx_tl(:) = 0.0_wp ! In our case it's 0 as observation height is not a function of space

  !2b take log 
  DO j = 1, khoriz
    DO i = 1, klev
      IF (array(i,j) > 0.0_wp) THEN
        larray(i,j) = LOG(array( i,j))
        larray_tl(i,j) = array_tl(i,j)/array(i,j)
      ENDIF
    ENDDO ! i

  !2c initialisation
    CALL ropp_fm_spline_init_tl(klev,x_re(:,j),x_tl_re(:,j),larray(:,j),larray_tl(:,j),y2(:,j),y2_tl(:,j))

  ENDDO !j 

  !2d do spline interpolation 
  CALL ropp_fm_splint_tl(klev,knewlev,khoriz,x_re,x_tl_re,larray,larray_tl,y2,y2_tl,newx,newx_tl,interp_tl)

  DEALLOCATE(larray)
  DEALLOCATE(larray_tl)
  DEALLOCATE(x_re)
  DEALLOCATE(x_tl_re)
  DEALLOCATE(y2)
  DEALLOCATE(y2_tl)
  DEALLOCATE(newx_tl)

END SUBROUTINE ropp_fm_spline_log_tl
