! $Id$

SUBROUTINE ropp_fm_spline_2dba(x, kspline, array, newx, interp)

!****s* Interpolation/ropp_fm_spline_2dba *
!
! NAME
!    ropp_fm_spline_2dba - Interpolate logarithmically along a spline for interpolation on finer model grid for 2D BA
!
! SYNOPSIS
!    call ropp_fm_spline_2dba(x, newx, array, interp)
! 
! DESCRIPTION
!    This subroutine interpolates an array along a spline assuming it varies exponentially 
!    in x, i.e. assuming its log varies as function of x.
!
! INPUTS
!    real(wp), dim(:,:) :: x       Coordinate values.
!    integer(wp)      :: kspline 
!    real(wp), dim(:,:) :: array   Data to be interpolated (lives on x).
!
! OUTPUT
!    real(wp), dim(:) :: newx    New coordinate values.
!    real(wp), dim(:,:) :: interp  Interpolated data (lives on newx).
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
!    ropp_fm_spline_2dba_ad
!    ropp_fm_spline_2dba_tl
!
! AUTHOR
!   ECMWF
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

  REAL(wp), DIMENSION(:,:), INTENT(in)  :: x
  INTEGER, INTENT(in)                   :: kspline
  REAL(wp), DIMENSION(:,:), INTENT(in)  :: array
  REAL(wp), DIMENSION(:), INTENT(inout) :: newx 
  REAL(wp), DIMENSION(:,:), INTENT(out) :: interp

  ! local variables
  INTEGER                             :: i, j, k, jk, jj, icen
  INTEGER                             :: khoriz   ! no. of horizontal locations 
  INTEGER                             :: klev     ! number of full model levels
  INTEGER                             :: knewlev  ! no. of interpolated levels.
  REAL(wp)                            :: zdz

  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: y2
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: larray

!-------------------------------------------------------------------------------
! 2. Do the interpolation
!-------------------------------------------------------------------------------

  ! 2a preparation for using 1d or 2d code

  klev    = SIZE(array, DIM = 1)
  khoriz  = SIZE(array, DIM = 2)
  
  knewlev = SIZE(interp, DIM =1) !klev * kspline

  ALLOCATE(y2(klev,khoriz))
  ALLOCATE(larray(klev,khoriz))

  interp(:,:) = ropp_MDFV

! position of central profile

  icen = khoriz/2+1

! calculate the geometric height of the model levels and interpolate

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
    CALL ropp_fm_spline_init(klev,x(:,j),larray(:,j),y2(:,j))
    
  ENDDO !j 

  !2d now interpolate the central profile geopotential heights to finer grid

  jj=1

  DO i = 1, klev-1

    newx(jj) = x(i,icen)

    jj = jj+1
   
    zdz = (x(i+1,icen)-x(i,icen))/REAL(kspline)
    
    DO jk = 1, kspline - 1 
   
      newx(jj) = x(i,icen) + REAL(jk)*zdz
     
      jj = jj +1  
        
    ENDDO

  ENDDO

  ! uppermost level

  newx(knewlev) = x(klev,icen)

   
  !2e do spline interpolation 
  !y2(:,:) = 0.0 ! if 0 then linear interpolation
  CALL ropp_fm_splint(klev,knewlev,khoriz,x,larray,y2,newx,interp)
 
  !interp = EXP(interp)

  DEALLOCATE(larray)
  DEALLOCATE(y2)

END SUBROUTINE ropp_fm_spline_2dba
