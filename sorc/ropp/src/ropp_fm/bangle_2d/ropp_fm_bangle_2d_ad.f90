! $Id$

!****s* BendingAngle2d/ropp_fm_bangle_2d_ad *
!
! NAME
!    ropp_fm_bangle_2d_ad - Forward model calculating a bending
!                        angle profile from planar information.
!
! SYNOPSIS
!    call ropp_fm_bangle_2d_ad(2D_state, 2D_state_ad, obs, obs_ad)
!
! DESCRIPTION
!
!    This routine is a forward model calculating a vertical profile of
!    bending angles from temperature, humidity and surface pressure. Bending
!    angle values are calculated for the impact parameters given in the
!    observation vector.
!
! INPUTS
!    type(State2dFM)       :: x,x_ad     ! State vector
!    type(Obs1dBangle)     :: y,y_ad     ! Observation vector (levels required)
!
! OUTPUT
!    type(Obs1dBangle)     :: y_ad     ! Observation vector
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

SUBROUTINE ropp_fm_bangle_2d_ad(x, x_ad, y, y_ad)

! 1.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_bangle_2d_ad
  USE geodesy,   ONLY: gravity, R_eff
  USE ropp_fm_types
  USE ropp_fm_constants
  USE ropp_utils

  IMPLICIT NONE

  TYPE(state2dFM),   INTENT(in)        :: x
  TYPE(state2dFM),   INTENT(inout)     :: x_ad
  TYPE(Obs1dBangle), INTENT(in)        :: y
  TYPE(Obs1dBangle), INTENT(inout)     :: y_ad

  INTEGER                                  :: i
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: pwvp,pwvp_ad
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: pdry,pdry_ad
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: z,z_ad
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: rad,rad_ad
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: impact
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: impact_ad
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: refrac
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)   :: refrac_ad,ref1_ad,ref2_ad,ref3_ad
  REAL(wp), DIMENSION(y%nobs,2)            :: a_path_ad
  REAL(wp)                                 :: glat,g_ratio,rad_eff

! non-ideal gas

  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: z_geop          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: z_geop_ad          ! geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: zcomp_dry_inv, zcomp_wet_inv ! compressibilities
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz)     :: zcomp_dry_inv_ad, zcomp_wet_inv_ad ! compressibilities

  REAL(wp)                         :: kap1,kap2,kap3  ! refractivity coefficients used in routine

  ! Set maximum height for 2D calulation, perform 1D calculation above
  REAL(wp), PARAMETER                      :: z_2d = 1.0e4_wp

  ! Set step size in ray-tracer
  INTEGER,  PARAMETER                      :: msplit = 4

  !spline
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: refrac_int       ! Interpolated refrac
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: refrac_int_ad    ! Interpolated refrac perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  	       :: z_geop_int       ! interpolated geoptential height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)            :: z_geop_int_ad    ! interpolated geoptential height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: z_int            ! Interpolated geometric height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: z_int_ad         ! Interpolated geometric height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: impact_int       ! Interpolated nr
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: impact_int_ad    ! Interpolated nr perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: rad_int         
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1,x%n_horiz)  :: rad_int_ad      


! initialise local adjoint variables

pwvp_ad   = 0.0_wp
pdry_ad   = 0.0_wp
z_ad      = 0.0_wp
rad_ad    = 0.0_wp
refrac_ad = 0.0_wp
ref1_ad   = 0.0_wp
ref2_ad   = 0.0_wp
ref3_ad   = 0.0_wp
impact_ad = 0.0_wp
a_path_ad = 0.0_wp

! non-ideal gas

z_geop_ad = 0.0_wp
zcomp_dry_inv_ad = 0.0_wp
zcomp_wet_inv_ad = 0.0_wp

! for spline interpolation
  IF (x%spline_int) THEN
    refrac_int_ad = 0.0_wp
    z_geop_int_ad = 0.0_wp
    z_int_ad      = 0.0_wp
    impact_int_ad = 0.0_wp
    rad_int_ad    = 0.0_wp
  END IF

!-------------------------------------------------------------------------------
! 2. Non ideal gas options
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

!  zcomp_dry_inv(:,:) = 1.0_wp
!  zcomp_wet_inv(:,:) = 1.0_wp
!  z_geop(:,:) = x%geop(:,:)

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3

  ENDIF

! 1.1 Calculate water vapor and dry pressure pressure
! ----------------------------------

  pwvp(:,:) = x%pres(:,:) * x%shum(:,:) / (epsilon_water + (1.0_wp - epsilon_water)*x%shum(:,:))

  pdry(:,:) = x%pres(:,:) - pwvp(:,:)

! 1.2 Calculate refractivity

  refrac = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp *zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp

!-------------------------------------------------------------------------------
! spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------

  IF (x%spline_int) THEN
  
    refrac_int(:,:) = ropp_MDFV    
    rad_int(:,:) = ropp_MDFV    
   
    CALL ropp_fm_spline_2dba(z_geop, x%ispline, refrac, z_geop_int, refrac_int)

! 1.3 Calculate geometric height at each location in plane
! --------------------------------------------------------

     DO i = 1, x%n_horiz

! g value and effective radius used in the geopotential -> geometric transform

      glat=gravity(x%lat(i))
      g_ratio = glat/g_wmo
      rad_eff = R_eff(x%lat(i))

! geometric height

      z_int(:,i) =  z_geop_int(:)/ (g_ratio - z_geop_int(:)/rad_eff)

    ENDDO


! 1.4 Calculate radius
! ------------------------------
    
    rad_int(:,:) = z_int(:,:) + y%r_curve + y%undulation


! 1.5 Calculate radius-refractive index product
! ----------------------------------------------

    impact_int(:,:) = (1.0_wp + 1.e-6_wp*refrac_int(:,:)) * rad_int(:,:) 
 
! 1.6 Calculate bending angles with a 2D operator
! -----------------------------------------------

! call 2D operator based on a 2nd order Runge-Kutta solution of the ray equations.
  
    CALL ropp_fm_alpha2drk_ec_ad(y%nobs,x%ispline * (x%n_lev - 1) + 1, x%n_horiz, msplit, x%dtheta, y%impact, &
                         refrac_int, refrac_int_ad, rad_int, rad_int_ad, impact_int, impact_int_ad, &
			 y%r_curve, z_2d, a_path_ad, y_ad%bangle)
! radius * refractive index product
  
    refrac_int_ad(:,:) = refrac_int_ad(:,:) + 1.0e-6_wp*rad_int(:,:)*impact_int_ad(:,:)
    rad_int_ad(:,:)    = rad_int_ad(:,:)    + (1.0_wp + 1.e-6_wp*refrac_int(:,:))*impact_int_ad(:,:)
    impact_int_ad(:,:) = 0.0_wp

! height
   
    z_int_ad(:,:) = z_int_ad(:,:) + rad_int_ad(:,:)
    rad_int_ad(:,:) = 0.0_wp

    DO i = 1, x%n_horiz

! g value and effective radius used in the geopotential -> geometric transform

      glat=gravity(x%lat(i))
      g_ratio = glat/g_wmo
      rad_eff = R_eff(x%lat(i))

! geopotential -- geometric height
   
      z_geop_int_ad(:) = z_geop_int_ad(:) + &
                     g_ratio/ (g_ratio - z_geop_int(:)/rad_eff)**2*z_int_ad(:,i)
      z_int_ad(:,i) = 0.0_wp

    ENDDO

!-------------------------------------------------------------------------------
! 8.a Interpolation - spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------
  
    CALL ropp_fm_spline_2dba_ad(z_geop, x%ispline, refrac,  z_geop_int,  z_geop_ad, refrac_ad, z_geop_int_ad, refrac_int_ad)
    
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

    ENDDO


! 1.4 Calculate radius
! ------------------------------

    rad(:,:) = z(:,:) + y%r_curve + y%undulation
 
! 1.5 Calculate radius-refractive index product
! ----------------------------------------------
 
    impact(:,:) = (1.0_wp + 1.e-6_wp*refrac(:,:)) * rad(:,:)

! 1.6 Calculate bending angles with a 2D operator
! -----------------------------------------------

! call 2D operator based on a 4th order Runge-Kutta solution of the ray equations.
  
    CALL ropp_fm_alpha2drk_ad(y%nobs, x%n_lev, x%n_horiz, msplit, x%dtheta, y%impact, &
                         refrac, refrac_ad, rad, rad_ad, impact, impact_ad, &
			 y%r_curve, z_2d, a_path_ad, y_ad%bangle)


! radius * refractive index product

    refrac_ad(:,:) = refrac_ad(:,:) + 1.0e-6_wp*rad(:,:)*impact_ad(:,:)
    rad_ad(:,:)    = rad_ad(:,:)    + (1.0_wp + 1.e-6_wp*refrac(:,:))*impact_ad(:,:)
    impact_ad(:,:) = 0.0_wp

! height

    z_ad(:,:) = z_ad(:,:) + rad_ad(:,:)
    rad_ad(:,:) = 0.0_wp

    DO i = 1, x%n_horiz

! g value and effective radius used in the geopotential -> geometric transform

      glat=gravity(x%lat(i))
      g_ratio = glat/g_wmo
      rad_eff = R_eff(x%lat(i))

! geopotential -- geometric height

      z_geop_ad(:,i) = z_geop_ad(:,i) + &
                     g_ratio/ (g_ratio - z_geop(:,i)/rad_eff)**2*z_ad(:,i)
      z_ad(:,i) = 0.0_wp

    ENDDO

  ENDIF

!----------------------------------------------
! 9. Adjoint of refractivity calculation
!----------------------------------------------
 
  pdry_ad = pdry_ad +  refrac_ad * kap1 * zcomp_dry_inv/ x%temp

  pwvp_ad = pwvp_ad +refrac_ad * (kap2 * &
            zcomp_wet_inv/ x%temp**2 + kap3 * zcomp_wet_inv/ x%temp)

  zcomp_dry_inv_ad = zcomp_dry_inv_ad + refrac_ad * kap1 * pdry / x%temp

  zcomp_wet_inv_ad = zcomp_wet_inv_ad + refrac_ad * ( &
                   kap2 * pwvp / x%temp**2 + kap3 * pwvp / x%temp)

  x_ad%temp   = x_ad%temp - refrac_ad * (kap1*pdry*zcomp_dry_inv/(x%temp**2)     &
                                + 2.0_wp*kap2*pwvp*zcomp_wet_inv/(x%temp**3) &
                                  + kap3*pwvp*zcomp_wet_inv/x%temp**2)

  refrac_ad = 0.0_wp

! dry pressure adjoint

  pwvp_ad(:,:) = pwvp_ad(:,:) - pdry_ad(:,:)
  x_ad%pres(:,:) = x_ad%pres(:,:) + pdry_ad(:,:)
  pdry_ad(:,:) = 0.0_wp


! water vapour pressure

  x_ad%pres(:,:) = x_ad%pres(:,:) +  pwvp(:,:)/x%pres(:,:)*pwvp_ad(:,:)
  x_ad%shum(:,:) = x_ad%shum(:,:) + pwvp_ad(:,:)* &
                  x%pres(:,:)*epsilon_water/(epsilon_water + (1.0_wp - epsilon_water)*x%shum(:,:))**2

  pwvp_ad(:,:) = 0.0_wp

! 11 Adjoint of the compressibilty calculation
!-----------------------------------------------------------

  IF (x%non_ideal) THEN

      CALL ropp_fm_compress_2d_ad &
     &(x,x_ad,z_geop_ad,zcomp_dry_inv_ad,zcomp_wet_inv_ad)

  ENDIF

  x_ad%geop(:,:) = x_ad%geop(:,:) + z_geop_ad(:,:)
  z_geop_ad(:,:) = 0.0_wp

  zcomp_dry_inv_ad(:,:) = 0.0_wp
  zcomp_wet_inv_ad(:,:) = 0.0_wp


END SUBROUTINE ropp_fm_bangle_2d_ad
