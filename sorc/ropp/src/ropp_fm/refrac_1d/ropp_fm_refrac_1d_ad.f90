! $Id$

SUBROUTINE ropp_fm_refrac_1d_ad(x, x_ad, y, y_ad)

!****s* Refractivity/ropp_fm_refrac_1d_ad *
!
! NAME
!    ropp_fm_refrac_1d_ad - Adjoint of ropp_fm_refrac_1d().
!
! SYNOPSIS
!    call ropp_fm_refrac_1d_ad(x, x_ad, y, y_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_refrac_1d.
!
! INPUTS
!    type(State1dFM)        :: x        ! State vector structure
!    type(Obs1dRefrac)      :: y        ! Observation vector structure
!    real(wp), dimension(:) :: y_ad     ! Adjoint forcing
!
! OUTPUT
!    type(State1dFM)        :: x_ad     ! State vector adjoint
!
! NOTES
!    The obs vector is required only for the observation's geopotential height
!    levels; no forward simulated refractivity profile is returned.
!
!    The lengths of the arrays x_ad%state and y_ad must agree with the 
!    lengths of the x%state and y%refrac arrays, respectively.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d
!    ropp_fm_refrac_1d_tl
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
  USE ropp_utils
  USE ropp_fm,   not_this => ropp_fm_refrac_1d_ad
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),        INTENT(in)    :: x                ! State vector
  TYPE(State1dFM),        INTENT(inout) :: x_ad             ! State vector adjoint
  TYPE(Obs1dRefrac),      INTENT(in)    :: y                ! Observation vector
  REAL(wp), DIMENSION(:), INTENT(inout) :: y_ad             ! Observation adjoint


  REAL(wp), DIMENSION(x%n_lev)          :: pwvp             ! Partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)          :: pwvp_ad          ! Pwvp perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: pdry             ! Dry pressure
  REAL(wp), DIMENSION(x%n_lev)          :: pdry_ad          ! Pdry perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: refrac           ! Refractivity
  REAL(wp), DIMENSION(x%n_lev)          :: refrac_ad        ! Refractivity perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: z_geop           ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)          :: z_geop_ad        ! GPH perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_dry_inv    ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_dry_inv_ad ! Dry compressibility perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_wet_inv    ! Wet compressibility
  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_wet_inv_ad ! Wet compressibility perturbation

  REAL(wp)                              :: kap1,kap2,kap3   ! Refractivity coefficients used in routine

  INTEGER  :: i

  REAL(wp), DIMENSION(:,:), ALLOCATABLE     :: refrac_ad_re          ! Refractivity
  REAL(wp), DIMENSION(:,:), ALLOCATABLE     :: refrac_re             ! Refractivity
!-------------------------------------------------------------------------------
! 2. Reset local adjoint variables
!-------------------------------------------------------------------------------

  pwvp_ad   = 0.0_wp
  pdry_ad   = 0.0_wp
  z_geop_ad = 0.0_wp
  zcomp_dry_inv_ad = 0.0_wp
  zcomp_wet_inv_ad = 0.0_wp
  refrac_ad = 0.0_wp

!-------------------------------------------------------------------------------
! 2. Non ideal gas options
!-------------------------------------------------------------------------------

! set inverse of compressibilities

  zcomp_dry_inv(:) = 1.0_wp
  zcomp_wet_inv(:) = 1.0_wp

! initialise geopotential heights

  z_geop(:) = x%geop(:)

  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

     kap1 = kappa1_comp
     kap2 = kappa2_comp
     kap3 = kappa3_comp

!    calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress(x,z_geop,zcomp_dry_inv,zcomp_wet_inv)

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3

  ENDIF

!-------------------------------------------------------------------------------
! 3. Standard exponentially varying refractivity assumption
!-------------------------------------------------------------------------------

! 3.1 Calculate water vapor pressure
! ----------------------------------

  pwvp   = x%pres * x%shum / (epsilon_water + (1.0_wp-epsilon_water)*x%shum)

  pdry = x%pres - pwvp

! 3.2 Calculate refractivity
! --------------------------

  refrac = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp *zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp

! 3.3 Adjoint of forward model calculations
!------------------------------------------

  y_ad = y_ad * y%weights

! 3.3.1 Adjoint interpolation to measurement's geopotential levels

  ALLOCATE(refrac_re(SIZE(refrac),1))
  ALLOCATE(refrac_ad_re(SIZE(refrac_ad),1))
 
  refrac_re    = RESHAPE(refrac, (/SIZE(refrac),1/))
  refrac_ad_re = RESHAPE(refrac_ad, (/SIZE(refrac_ad),1/))
    
  refrac_ad_re(:,:) = 0.0_wp

  IF (x%spline_int) THEN
    
    CALL ropp_fm_spline_log_ad(z_geop,y%geop,refrac_re,z_geop_ad,refrac_ad_re,y_ad)
    
    refrac_ad = refrac_ad + refrac_ad_re(:,1)
    refrac_ad_re(:,1) = 0.0_wp  

  ELSE

    CALL ropp_fm_interpol_log_ad(z_geop,y%geop,refrac,z_geop_ad, refrac_ad,y_ad)
  
  ENDIF

  DEALLOCATE(refrac_re)
  DEALLOCATE(refrac_ad_re)


! 3.3.2 Adjoint of refractivity calculation

  pdry_ad = pdry_ad +  refrac_ad * kap1 * zcomp_dry_inv/ x%temp

  pwvp_ad = pwvp_ad +refrac_ad * (kap2 * &
            zcomp_wet_inv/ x%temp**2 + kap3 * zcomp_wet_inv/ x%temp)

  zcomp_dry_inv_ad = zcomp_dry_inv_ad + refrac_ad * kap1 * pdry / x%temp

  zcomp_wet_inv_ad = zcomp_wet_inv_ad + refrac_ad * ( & 
                   kap2 * pwvp / x%temp**2 + kap3 * pwvp / x%temp) 

  x_ad%temp   = x_ad%temp - &
                refrac_ad * (kap1*pdry*zcomp_dry_inv/(x%temp**2) + &
                2.0_wp*kap2*pwvp*zcomp_wet_inv/(x%temp**3) + &
                kap3*pwvp*zcomp_wet_inv/x%temp**2)

  refrac_ad = 0.0_wp
  
! 3.3.3 Adjoint of water vapor pressure calculation

  x_ad%pres = x_ad%pres + pdry_ad

  pwvp_ad = pwvp_ad - pdry_ad

  pdry_ad = 0.0_wp

  x_ad%pres = x_ad%pres + pwvp_ad * &
              (x%shum/(epsilon_water+(1.0_wp-epsilon_water)*x%shum))

  x_ad%shum = x_ad%shum + pwvp_ad * &
              (x%pres/(epsilon_water+(1.0_wp-epsilon_water)*x%shum) - &
               x%pres * x%shum * (1.0_wp - epsilon_water) / &
               ((epsilon_water + (1.0_wp-epsilon_water)*x%shum) * &
               (epsilon_water + (1.0_wp-epsilon_water)*x%shum)))

  pwvp_ad = 0.0_wp

!-------------------------------------------------------------------------------
! 5. Account for non-ideal compressibility options
!-------------------------------------------------------------------------------

  IF (x%non_ideal) THEN

      CALL ropp_fm_compress_ad &
        (x,x_ad,z_geop_ad,zcomp_dry_inv_ad,zcomp_wet_inv_ad)

  ENDIF

!-------------------------------------------------------------------------------
! 6. Final update of adjoint variables
!-------------------------------------------------------------------------------

  x_ad%geop = x_ad%geop + z_geop_ad

  z_geop_ad = 0.0_wp

  zcomp_dry_inv_ad(:) = 0.0_wp

  zcomp_wet_inv_ad(:) = 0.0_wp


END SUBROUTINE ropp_fm_refrac_1d_ad
