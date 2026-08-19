! $Id$

!****s* BendingAngle2d/ropp_fm_bangle_2d_tl *
!
! NAME
!    ropp_fm_bangle_2d_tl - Forward model calculating a bending
!                        angle profile from planar information.
!
! SYNOPSIS
!    call ropp_fm_bangle_2d(2D state, obs)
!
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of
!    bending angles from temperature, humidity and surface pressure. Bending
!    angle values are calculated for the impact parameters given in the
!    observation vector.
!
! INPUTS
!    type(State2dFM)    :: x,x_tl     ! State vector
!
! OUTPUT
!    type(Obs1dBangle)     :: y,y_tl     ! Observation vector
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


SUBROUTINE ropp_fm_bangle_2d_tl(x, x_tl,y, y_tl)

! 1.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_bangle_2d_tl
  USE geodesy,   ONLY: gravity, R_eff
  USE ropp_fm_types
  USE ropp_fm_constants
  USE ropp_utils

  IMPLICIT NONE

  TYPE(state2dFM),   INTENT(in)    :: x,x_tl
  TYPE(Obs1dBangle), INTENT(inout) :: y
  TYPE(Obs1dBangle), INTENT(inout) :: y_tl

  INTEGER                                  :: i
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: pwvp,pwvp_tl
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: pdry,pdry_tl
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: z,z_tl
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: rad,rad_tl
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: impact,impact_tl
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: refrac,refrac_tl
  REAL(wp), DIMENSION(y%nobs,2)            :: a_path,a_path_tl
  REAL(wp)                                 :: glat,g_ratio,rad_eff

! non ideal gas

  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: z_geop          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: z_geop_tl          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: zcomp_dry_inv, zcomp_wet_inv ! compressibilities
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: zcomp_dry_inv_tl, zcomp_wet_inv_tl ! compressibilities

  REAL(wp)                         :: kap1,kap2,kap3  ! refractivity coefficients used in routine

  ! Set maximum height for 2D calculation, perform 1D calculation above
  REAL(wp), PARAMETER                      :: z_2d = 1.0e4_wp

  ! Set step size in ray-tracer
  INTEGER,  PARAMETER                      :: msplit = 4

  !spline

  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: refrac_int       ! Interpolated refrac
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: refrac_int_tl    ! Interpolated refrac perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  	       :: z_geop_int       ! interpolated geoptential height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)            :: z_geop_int_tl    ! interpolated geoptential height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: z_int            ! Interpolated geometric height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: z_int_tl         ! Interpolated geometric height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: impact_int       ! Interpolated nr
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: impact_int_tl    ! Interpolated nr perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: rad_int         
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: rad_int_tl      


!-------------------------------------------------------------------------------
! 2. Non ideal gas options
!-------------------------------------------------------------------------------


! set inverse of compressibilities

  zcomp_dry_inv(:,:) = 1.0_wp
  zcomp_wet_inv(:,:) = 1.0_wp

  zcomp_dry_inv_tl(:,:) = 0.0_wp
  zcomp_wet_inv_tl(:,:) = 0.0_wp

! initialise geopotential heights

  z_geop(:,:) = x%geop(:,:)
  z_geop_tl(:,:) = x_tl%geop(:,:)

  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

     kap1 = kappa1_comp
     kap2 = kappa2_comp
     kap3 = kappa3_comp

!    calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress_2d_tl &
     &(x,x_tl,z_geop,z_geop_tl,zcomp_dry_inv,zcomp_dry_inv_tl,&
     &zcomp_wet_inv,zcomp_wet_inv_tl)

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3

  ENDIF


! 1.1 Calculate water vapor pressure
! ----------------------------------

  pwvp(:,:) = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)

  pwvp_tl(:,:) = pwvp * ( x_tl%pres/x%pres + x_tl%shum/x%shum &
                - x_tl%shum*pwvp*(1.0_wp - epsilon_water) / (x%pres*x%shum))

! dry pressure

  pdry = x%pres - pwvp
  pdry_tl =  x_tl%pres - pwvp_tl


! 1.2 Calculate refractivity
! --------------------------

  refrac(:,:) = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp


  refrac_tl(:,:) = kap1 * pdry_tl * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp_tl * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp_tl * zcomp_wet_inv/ x%temp + &
	   kap1 * pdry * zcomp_dry_inv_tl/ x%temp + &
           kap2 * pwvp * zcomp_wet_inv_tl/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv_tl/ x%temp - &
	   (kap1 * pdry * zcomp_dry_inv/ x%temp**2 + &
	   2.0_wp *kap2 * pwvp * zcomp_wet_inv/ x%temp**3 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp**2)* x_tl%temp


!-------------------------------------------------------------------------------
! spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------
  
  IF (x%spline_int) THEN
  
    refrac_int(:,:) = ropp_MDFV    
    
    !refrac_int_tl(:,:) = 0.0_wp !ropp_MDFV   
 
    CALL ropp_fm_spline_2dba(z_geop, x%ispline, refrac, z_geop_int, refrac_int)

    CALL ropp_fm_spline_2dba_tl(z_geop, x%ispline, refrac,  z_geop_int,  z_geop_tl, refrac_tl, z_geop_int_tl, refrac_int_tl)


! 1.3 Calculate geometric height at each location in plane
! --------------------------------------------------------

    DO i = 1, x%n_horiz

! g value and effective radius used in the geopotential -> geometric transform

      glat=gravity(x%lat(i))
      g_ratio = glat/g_wmo
      rad_eff = R_eff(x%lat(i))

! geometric height


      z_int(:,i) =  z_geop_int(:)/ (g_ratio - z_geop_int(:)/rad_eff)
      z_int_tl(:,i) = g_ratio/ (g_ratio - z_geop_int(:)/rad_eff)**2 * z_geop_int_tl(:)

    ENDDO

! 1.4 Calculate radius
! ------------------------------
 
    rad_int(:,:) = z_int(:,:) + y%r_curve + y%undulation
    rad_int_tl(:,:) = z_int_tl(:,:)
  

! 1.5 Calculate radius-refractive index product
! ----------------------------------------------

    impact_int(:,:) = (1.0_wp + 1.e-6_wp*refrac_int(:,:)) * rad_int(:,:) 
    impact_int_tl(:,:) = (1.0_wp + 1.e-6_wp*refrac_int(:,:)) * rad_int_tl(:,:) + &
                   1.0e-6_wp*rad_int(:,:)*refrac_int_tl(:,:)

! 1.6 Calculate bending angles with a 2D operator
! -----------------------------------------------

! call 2D operator based on 2th order Runge-Kutta solution for spline interpolation. 

    CALL ropp_fm_alpha2drk_ec_tl(y%nobs, x%ispline * (x%n_lev - 1) + 1, x%n_horiz, msplit, x%dtheta, y%impact, &
                         refrac_int, refrac_int_tl, rad_int, rad_int_tl, impact_int, impact_int_tl, &
                         y%r_curve, z_2d, a_path, a_path_tl, y%bangle, y_tl%bangle)
 
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

      z(:,i) = z_geop(:,i) / (g_ratio - z_geop(:,i)/rad_eff)
      z_tl(:,i) = g_ratio/ (g_ratio - z_geop(:,i)/rad_eff)**2 * z_geop_tl(:,i)

    ENDDO

! 1.4 Calculate radius
! ------------------------------
 
    rad(:,:) = z(:,:) + y%r_curve + y%undulation
    rad_tl(:,:) = z_tl(:,:)
  

! 1.5 Calculate radius-refractive index product
! ----------------------------------------------

    impact(:,:) = (1.0_wp + 1.e-6_wp*refrac(:,:)) * rad(:,:)
    impact_tl(:,:) = (1.0_wp + 1.e-6_wp*refrac(:,:)) * rad_tl(:,:) + &
                   1.0e-6_wp*rad(:,:)*refrac_tl(:,:)

! 1.6 Calculate bending angles with a 2D operator
! -----------------------------------------------

! call 2D operator based on a 4th order Runge-Kutta solution of the ray equations 

    CALL ropp_fm_alpha2drk_tl(y%nobs, x%n_lev, x%n_horiz, msplit, x%dtheta, y%impact, &
                         refrac, refrac_tl, rad, rad_tl, impact, impact_tl, &
  			 y%r_curve, z_2d, a_path, a_path_tl, y%bangle, y_tl%bangle)

  ENDIF


END SUBROUTINE ropp_fm_bangle_2d_tl
