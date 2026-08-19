! $Id$

SUBROUTINE ropp_fm_spline_log_ad(x, newx, array, x_ad, array_ad, interp_ad)

!****s* Interpolation/ropp_fm_spline_log *
!
! NAME
!    ropp_fm_spline_log_ad - Interpolate logarithmically along a spline.
!
! SYNOPSIS
!    call ropp_fm_spline_log_ad(x, newx, array,x_ad, array_ad, interp_ad)
! 
! DESCRIPTION
!    This subroutine interpolates an array along a spline assuming it varies exponentially 
!    in x, i.e. assuming its log varies as function of x.
!
! INPUTS
!    real(wp), dim(:) :: x       Coordinate values.
!    real(wp), dim(:) :: newx    New coordinate values.
!    real(wp), dim(:) :: array   Data to be interpolated (lives on x).
!    real(wp), dim(:) :: x_ad       Perturbation in x.
!    real(wp), dim(:) :: array_ad   Perturbations in array.
!
! OUTPUT
!    real(wp), dim(:) :: interp_ad  Interpolated data (lives on newx).
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

  REAL(wp), DIMENSION(:), INTENT(in)    :: x
  REAL(wp), DIMENSION(:), INTENT(in)    :: newx
  REAL(wp), DIMENSION(:,:), INTENT(in)  :: array
  REAL(wp), DIMENSION(:), INTENT(inout)    :: x_ad
  REAL(wp), DIMENSION(:,:), INTENT(inout)  :: array_ad
  REAL(wp), DIMENSION(:), INTENT(inout)    :: interp_ad

  INTEGER                             :: i, j, k
  INTEGER                             :: khoriz   ! no. of horizontal locations 
  INTEGER                             :: klev     ! number of full model levels
  INTEGER                             :: knewlev  ! no. of interpolated levels.

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: newx_ad             ! obs perturbation
  
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: x_re, x_ad_re
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: y2, y2_ad
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: larray, larray_ad

!-------------------------------------------------------------------------------
! 2. Do the interpolation
!-------------------------------------------------------------------------------

  ! 2a preparation for using 1d or 2d code

  knewlev = size(newx, DIM = 1)
  
  klev    = SIZE(array, DIM = 1)
  khoriz  = SIZE(array, DIM = 2)

  ALLOCATE(x_re(klev,khoriz))
  ALLOCATE(x_ad_re(klev,khoriz))
  ALLOCATE(y2(klev,khoriz))
  ALLOCATE(y2_ad(klev,khoriz))
  ALLOCATE(larray(klev,khoriz))
  ALLOCATE(larray_ad(klev,khoriz))
  ALLOCATE(newx_ad(knewlev))
 
  x_re     = RESHAPE(x, (/klev,khoriz/))
  x_ad_re  = RESHAPE(x_ad, (/klev,khoriz/))
  
  !y2(:,:) = 0.0_wp
  x_ad_re(:,:)   = 0.0_wp
  x_ad(:)        = 0.0_wp
  larray_ad(:,:) = 0.0_wp
  y2_ad(:,:)     = 0.0_wp

  newx_ad(:)     = 0.0_wp ! In our case it's 0 as observation height is not a function of space
  
  y2(:,:)        = ropp_MDFV
  larray(:,:)    = ropp_MDFV

  !2b take log 
  DO j = 1, khoriz
    DO i = 1, klev
      IF (array(i,j) > 0.0_wp) THEN
        larray(i,j) = LOG(array(i,j))
      ENDIF
    ENDDO ! i

  !2c initialisation
    CALL ropp_fm_spline_init(klev,x_re(:,j),larray(:,j),y2(:,j))

  ENDDO !j 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  2d adjoint code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  CALL ropp_fm_splint_ad(klev,knewlev,khoriz,x_re,x_ad_re,larray,larray_ad,y2,y2_ad,newx,newx_ad,interp_ad)
 
  DO j = 1, khoriz

    CALL ropp_fm_spline_init_ad(klev,x_re(:,j),x_ad_re(:,j),larray(:,j),larray_ad(:,j),y2(:,j),y2_ad(:,j))

    DO i = 1, klev
      IF (array(i,j) > 0.0_wp) THEN
        !larray_tl(i,j) = array_tl(i,j)/array(i,j)
        array_ad(i,j)  = larray_ad(i,j)/array(i,j) + array_ad(i,j)
        larray_ad(i,j) = 0.0_wp  
      ENDIF
    ENDDO ! i
    x_ad(:) = x_ad(:) + x_ad_re(:,j) 
    x_ad_re(:,j) = 0.0_wp 
  ENDDO !j 

  larray_ad(:,:) = 0.0_wp
  y2_ad(:,:)     = 0.0_wp
  interp_ad(:) = 0.0_wp
     
  DEALLOCATE(larray)
  DEALLOCATE(larray_ad)
  DEALLOCATE(x_re)
  DEALLOCATE(x_ad_re)
  DEALLOCATE(y2)
  DEALLOCATE(y2_ad)
  DEALLOCATE(newx_ad)

END SUBROUTINE ropp_fm_spline_log_ad
