! $Id$

!****s* BendingAngle2d/ropp_fm_bangle_2d *
!
! NAME
!    ropp_fm_bangle_2d - Forward model calculating a bending
!                        angle profile from planar information.
!
! SYNOPSIS
!    call ropp_fm_bangle_2d(2D_state, obs)
! 
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of 
!    bending angles from temperature, humidity and surface pressure. Bending
!    angle values are calculated for the impact parameters given in the
!    observation vector.
!
! INPUTS
!    type(State2dFM)       :: x     ! State vector
!    type(Obs1dBangle)     :: y     ! Observation vector (levels required)
!
! OUTPUT
!    type(Obs1dBangle)     :: y     ! Observation vector
!
! NOTES
!    The 2D forward model evaluates bending angles on the observed impact
!    parameter values using a ray tracer to solve the ray equations. The
!    operator neglects tangent point drift and uses the observed impact
!    parameter to determine the ray tangent height. 
! 
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_bangle_2d_ad
!    ropp_fm_bangle_2d_tl
!
! AUTHOR
!   ECMWF, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

SUBROUTINE ropp_fm_bangle_2d(x, y)

! 1.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_bangle_2d
  USE geodesy,   ONLY: gravity, R_eff
  USE ropp_fm_types
  USE ropp_fm_constants
  USE ropp_utils
  
  IMPLICIT NONE

  TYPE(state2dFM),   INTENT(inout) :: x
  TYPE(Obs1dBangle), INTENT(inout) :: y

  INTEGER                                  :: i
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: pwvp
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: pdry
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: z
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: rad
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: impact
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: refrac
  REAL(wp), DIMENSION(y%nobs,2)            :: a_path
  REAL(wp)                                 :: glat,g_ratio,rad_eff   

! non-ideal gas 

  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: z_geop          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: zcomp_dry_inv, zcomp_wet_inv ! compressibilities
  
  REAL(wp)                         :: kap1,kap2,kap3  ! refractivity coefficients used in routine
      
  ! Set maximum height for 2D calulation, perform 1D calculation above
  REAL(wp), PARAMETER                      :: z_2d = 1.0e4_wp

  ! Set step size in ray-tracer
  INTEGER,  PARAMETER                      :: msplit = 4 

! spline

  !REAL(wp), DIMENSION(:,:), ALLOCATABLE     :: refrac_re           ! refractivity
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1,x%n_horiz)       ::  refrac_int !vals interpolated to fixed heights.
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1)                 ::  z_geop_int
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1,x%n_horiz)       ::  z_int
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1,x%n_horiz)       ::  rad_int
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1,x%n_horiz)       ::  impact_int

!-------------------------------------------------------------------------------
! 1. Non-ideal gas options
!-------------------------------------------------------------------------------

! set inverse of compressibilities

  zcomp_dry_inv(:,:) = 1.0_wp
  zcomp_wet_inv(:,:) = 1.0_wp
   
! initialise geopotential heights
   
  z_geop(:,:) = x%geop(:,:)
 
  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

     kap1 = kappa1_comp
     kap2 = kappa2_comp
     kap3 = kappa3_comp   

!    calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress_2d(x,z_geop,zcomp_dry_inv,zcomp_wet_inv)  

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3
     
  ENDIF  


! 1.1 Calculate water vapor  and dry pressure
! ----------------------------------

  pwvp(:,:) = x%pres(:,:) * x%shum(:,:) / (epsilon_water + (1.0_wp - epsilon_water)*x%shum(:,:))

  pdry(:,:) = x%pres(:,:) - pwvp(:,:)

 ! 1.2 Calculate refractivity
! --------------------------

  refrac(:,:) = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp

!-------------------------------------------------------------------------------
! spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------

  IF (x%spline_int) THEN
  
    refrac_int(:,:) = ropp_MDFV    
    
    CALL ropp_fm_spline_2dba(z_geop, x%ispline, refrac, z_geop_int, refrac_int)

! 1.3b Calculate geometric height at each location in plane
! --------------------------------------------------------
    DO i = 1, x%n_horiz
  
! g value and effective radius used in the geopotential -> geometric transform
    
      glat=gravity(x%lat(i))   
      g_ratio = glat/g_wmo  
      rad_eff = R_eff(x%lat(i))

! geometric height

      z_int(:,i) =  z_geop_int(:)/ (g_ratio - z_geop_int(:)/rad_eff)

    ENDDO 

! 1.4b Calculate radius
! ------------------------------
    rad_int(:,:) = z_int(:,:) + y%r_curve + y%undulation

   
! 1.5b Calculate radius-refractive index product 
! ----------------------------------------------
 
    impact_int(:,:) = (1.0_wp + 1.e-6_wp*refrac_int(:,:)) * rad_int(:,:) 
    !x%refrac(:,:) = refrac_int(:,:)
    !x%nr(:,:)     = impact_int(:,:)

 
! 1.6b Calculate bending angles with a 2D operator
! -----------------------------------------------

! call 2D operator based on 2th order Runge-Kutta solution for spline interpolation. 

    CALL ropp_fm_alpha2drk_ec(y%nobs, x%ispline * (x%n_lev - 1) + 1, x%n_horiz, msplit, x%dtheta, y%impact, &
                         refrac_int, rad_int, impact_int, y%r_curve, z_2d, a_path, y%bangle,y%rtan)

  ELSE

!-------------------------------------------------------------------------------
! standard
! 
!-------------------------------------------------------------------------------
 ! 1.3 Calculate geometric height at each location in plane
! --------------------------------------------------------
    DO i = 1, x%n_horiz
  
! g value and effective radius used in the geopotential -> geometric transform
    
      glat=gravity(x%lat(i))   
      g_ratio = glat/g_wmo  
      rad_eff = R_eff(x%lat(i))

! geometric height

      z(:,i) =  z_geop(:,i)/ (g_ratio - z_geop(:,i)/rad_eff)

    ENDDO 

! 1.4 Calculate radius
! ------------------------------
    rad(:,:) = z(:,:) + y%r_curve + y%undulation

  
   
! 1.5 Calculate radius-refractive index product 
! ----------------------------------------------
 
    impact(:,:) = (1.0_wp + 1.e-6_wp*refrac(:,:)) * rad(:,:) 

! store the refractivity and nr product in x. Useful for diagnostics
! These variables are not active, so they don't appear in TL and AD routines.    

    x%refrac(:,:) = refrac(:,:)
    x%nr(:,:)     = impact(:,:)

 
! 1.6 Calculate bending angles with a 2D operator
! -----------------------------------------------

! call 2D operator based on a 4th order Runge-Kutta solution of the ray equations 

    CALL ropp_fm_alpha2drk(y%nobs, x%n_lev, x%n_horiz, msplit, x%dtheta, y%impact, &
                         refrac, rad, impact, y%r_curve, z_2d, a_path, y%bangle, y%rtan)

  ENDIF 


! 1.7 Store the a=nr*sin*(phi) value at ray endpoints 
! ---------------------------------------------------
  y%a_path(:,:) = a_path(:,:)


END SUBROUTINE ropp_fm_bangle_2d
