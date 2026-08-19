! $Id$

SUBROUTINE ropp_fm_bangle_1d_ad(x, x_ad, y, y_ad)

!****s* BendingAngle/ropp_fm_bangle_1d_ad *
!
! NAME
!    ropp_fm_bangle_1d_ad - Adjoint of ropp_fm_bangle_1d().
!
! SYNOPSIS
!    call ropp_fm_bangle_1d_ad(x, x_ad, y, y_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_bangle_1d.
!
! INPUTS
!    type(State1dFM)        :: x        ! State vector
!    type(Obs1dBangle)      :: y        ! Observation vector
!    real(wp), dimension(:) :: y_ad     ! Adjoint forcing
!
! OUTPUT
!    type(State1dFM)        :: x_ad     ! State vector adjoint
!
! NOTES
!    The obs vector is required only for the observation's geopotential levels;
!    no forward simulated refractivity profile is returned.
!
!    The lengths of the arrays state_ad%state and obs_ad must agree with the 
!    lengths of the state%state and obs%obs arrays, respectively.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_bangle_1d
!    ropp_fm_bangle_1d_tl
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
  USE ropp_fm,   not_this => ropp_fm_bangle_1d_ad
  USE ropp_fm_types
  USE ropp_fm_iono
  USE ropp_fm_constants
  USE geodesy

  IMPLICIT NONE

  TYPE(State1dFM),        INTENT(in)    :: x                ! State vector
  TYPE(State1dFM),        INTENT(inout) :: x_ad             ! State vector adjoint
  TYPE(Obs1dBangle),      INTENT(in)    :: y                ! Observation vector
  REAL(wp), DIMENSION(:), INTENT(inout) :: y_ad             ! Observation adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: pwvp             ! Partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)          :: pwvp_ad          ! Pwvp adjoint
 
  REAL(wp), DIMENSION(x%n_lev)          :: pdry             ! Dry pressure
  REAL(wp), DIMENSION(x%n_lev)          :: pdry_ad          ! Pdry adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: refrac           ! Refractivity
  REAL(wp), DIMENSION(x%n_lev)          :: refrac_ad        ! Refractivity adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: z_geop           ! |Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)          :: z_geop_ad        ! GPH adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_dry_inv    ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_dry_inv_ad ! Dry compressibility adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_wet_inv    ! Wet compressibility
  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_wet_inv_ad ! Wet compressibility adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: h                ! Geometric height
  REAL(wp), DIMENSION(x%n_lev)          :: h_ad             ! Geometric height adjoint

  REAL(wp), DIMENSION(x%n_lev)          :: impact           ! Impact parameter
  REAL(wp), DIMENSION(x%n_lev)          :: impact_ad        ! Impact parameter adjoint

  REAL(wp), DIMENSION(SIZE(y%bangle))   :: bangle

  REAL(wp)                              :: kap1,kap2,kap3   ! Refractivity coefficients used in routine

  REAL(wp)                              :: R_peak           ! Used in iono routines
  REAL(wp)                              :: R_peak_ad        ! Used in iono routines adjoint
  CHARACTER(LEN=256)                    :: routine          ! For messaging


  !spline
  REAL(wp), DIMENSION(:,:), ALLOCATABLE            :: refrac_re        ! Refractivity
  REAL(wp), DIMENSION(:,:), ALLOCATABLE            :: refrac_ad_re     ! Refractivity perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: refrac_int       ! Interpolated refrac
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: refrac_int_ad    ! Interpolated refrac perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: z_geop_int       ! interpolated geoptential height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: z_geop_int_ad    ! interpolated geoptential height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: h_int            ! Interpolated geometric height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: h_int_ad         ! Interpolated geometric height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: impact_int       ! Interpolated nr
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: impact_int_ad    ! Interpolated nr perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: temp_int         ! Interpolated temperature
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: temp_int_ad      ! Interpolated temperature perturbation

!-------------------------------------------------------------------------------
! 2. Define routine name for messaging
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_fm_bangle_1d_ad')

!-------------------------------------------------------------------------------
! 3. Reset local adjoint variables
!-------------------------------------------------------------------------------
  
  pwvp_ad   = 0.0_wp
  refrac_ad = 0.0_wp
  h_ad      = 0.0_wp
  impact_ad = 0.0_wp

! for compressibility

  pdry_ad   = 0.0_wp
  z_geop_ad = 0.0_wp
  zcomp_dry_inv_ad = 0.0_wp
  zcomp_wet_inv_ad = 0.0_wp

! for iono

  R_peak_ad = 0.0_wp

! for spline interpolation
  IF (x%spline_int) THEN
    refrac_int_ad = 0.0_wp
    z_geop_int_ad = 0.0_wp
    h_int_ad      = 0.0_wp
    impact_int_ad = 0.0_wp
    temp_int_ad   = 0.0_wp
  END IF
!-------------------------------------------------------------------------------
! 4. Non ideal gas options
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
! 5. Recompute forward model variables
!-------------------------------------------------------------------------------

! 5.1 Calculate water vapor pressure

  pwvp   = x%pres * x%shum / (epsilon_water + (1.0_wp-epsilon_water)*x%shum)

  pdry = x%pres - pwvp

! 5.2a Calculate refractivity

  refrac = kap1 * pdry * zcomp_dry_inv / x%temp    + &
           kap2 * pwvp * zcomp_wet_inv / x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv / x%temp

! 5.2b Interpolation - spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
  
  ALLOCATE(refrac_re(SIZE(refrac),1))

  refrac_re         = RESHAPE(refrac, (/SIZE(refrac),1/))
  
  IF (x%spline_int) THEN
  
    refrac_int(:) = ropp_MDFV    
    temp_int(:) = ropp_MDFV    
   
    CALL ropp_fm_spline_ba(z_geop, x%ispline, refrac_re,  z_geop_int, refrac_int)

  ENDIF

!-------------------------------------------------------------------------------
! 6. Calculate impact parameter
!-------------------------------------------------------------------------------
  IF (x%spline_int) THEN

    h_int    = y%r_earth * z_geop_int / (y%g_sfc / g_wmo * y%r_earth - z_geop_int)
    
    IF (y%undulation > ropp_MDTV) THEN
      impact_int = (1.0_wp + 1.e-6_wp*refrac_int) * (h_int + y%r_curve + y%undulation) 
    ELSE
      CALL message(msg_warn, "Undulation missing. " // &
                 "Will assume to be zero when calculating full and " // &
                 "perturbed impact parameters.")
      impact_int    = (1.0_wp + 1.e-6_wp*refrac_int) * (h_int + y%r_curve)
    END IF

  ELSE

    h = geopotential2geometric(x%lat, z_geop)

    IF (y%undulation > ropp_MDTV) THEN
      impact = (1.0_wp + refrac*1.0e-6_wp) * (h + y%r_curve + y%undulation)
    ELSE
      CALL message(msg_warn, "Undulation missing. " // &
                 "Will assume to be zero when calculating full and " // &
                 "adjoint impact parameters.")
      impact = (1.0_wp + refrac*1.0e-6_wp) * (h + y%r_curve)
    END IF

  END IF
!-------------------------------------------------------------------------------
! 7. Calculate bending angles
!-------------------------------------------------------------------------------

!  call ropp_fm_abel(impact, refrac, y%impact, bangle)  ! don't need this for neutral BAs

!-------------------------------------------------------------------------------
! 8. Adjoint of ionospheric bending angle computation **IF** we're using L1,L2
!-------------------------------------------------------------------------------

  y_ad = y_ad * y%weights

  IF ( x%direct_ion .AND. (y%r_leo > ropp_MDTV) ) THEN

! need the neutral bending for a test in ropp_fm_iono
    IF (x%spline_int) THEN
      ! overwrite x%new_bangle_op to False, needed for spline interpolation 
      CALL ropp_fm_abel( &
        impact_int, refrac_int, temp_int, y%r_curve, .FALSE., y%impact, bangle)
    ELSE
      CALL ropp_fm_abel &
        (impact, refrac, x%temp, y%r_curve, x%new_bangle_op, y%impact, bangle)
    END IF

! now call adjoint

    R_peak = x%H_peak + y%r_curve

    CALL ropp_fm_iono_bangle_ad(x%Ne_max,    R_peak,    x%H_width, y%R_leo, &
                                x_ad%Ne_max, R_peak_ad, x_ad%H_width, &
                                y%n_L1, y%impact, bangle, y_ad)

    x_ad%H_peak = x_ad%H_peak + R_peak_ad

    R_peak_ad = 0.0_wp

  END IF

!-------------------------------------------------------------------------------
! 9. Adjoint of bending angle computation
!-------------------------------------------------------------------------------

  IF (x%spline_int) THEN
    
    CALL ropp_fm_abel_ad( &
      impact_int, refrac_int, temp_int, temp_int_ad, y%r_curve, .FALSE., &
      y%impact, impact_int_ad, refrac_int_ad, y_ad)
  ELSE
    CALL ropp_fm_abel_ad&
    (impact, refrac, x%temp, x_ad%temp, y%r_curve, x%new_bangle_op, &
     y%impact, impact_ad, refrac_ad, y_ad)
  END IF

!-------------------------------------------------------------------------------
! 10. Adjoint of impact parameter calculation
!-------------------------------------------------------------------------------


  IF (x%spline_int) THEN

    IF (y%undulation > ropp_MDTV) THEN
      refrac_int_ad = refrac_int_ad + 1.0e-6_wp * impact_int_ad * (h_int + y%r_curve + y%undulation)
    ELSE
      refrac_int_ad = refrac_int_ad + 1.0e-6_wp * impact_int_ad * (h_int + y%r_curve)
    END IF

    h_int_ad      = h_int_ad + impact_int_ad * (1.0_wp + 1.0e-6_wp*refrac_int)

    impact_int_ad = 0.0_wp

    z_geop_int_ad = z_geop_int_ad + h_int_ad*(y%r_earth/(((y%g_sfc/g_wmo)*y%r_earth)-z_geop_int) &
               + y%r_earth*z_geop_int/((((y%g_sfc/g_wmo)*y%r_earth)-z_geop_int)**2))

    h_int_ad      = 0.0_wp

  END IF

  IF (y%undulation > ropp_MDTV) THEN
    refrac_ad = refrac_ad + 1.0e-6_wp * impact_ad * (h + y%r_curve + y%undulation)
  ELSE
    refrac_ad = refrac_ad + 1.0e-6_wp * impact_ad * (h + y%r_curve)
  END IF

  h_ad      = h_ad + impact_ad * (1.0_wp + 1.0e-6_wp*refrac)

  impact_ad = 0.0_wp

  z_geop_ad = z_geop_ad + h_ad*(y%r_earth/(((y%g_sfc/g_wmo)*y%r_earth)-z_geop) &
             + y%r_earth*z_geop/((((y%g_sfc/g_wmo)*y%r_earth)-z_geop)**2))

  h_ad      = 0.0_wp


!-------------------------------------------------------------------------------
! 11.a Interpolation - spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------
  
  ALLOCATE(refrac_ad_re(SIZE(refrac),1))
  
  refrac_ad_re = RESHAPE(refrac_ad, (/SIZE(refrac_ad),1/))

  refrac_ad_re(:,:) = 0.0_wp

  IF (x%spline_int) THEN
   
    CALL ropp_fm_spline_ba_ad(z_geop, x%ispline, refrac_re,  z_geop_int,  z_geop_ad, refrac_ad_re, z_geop_int_ad, refrac_int_ad)
    
    refrac_ad = refrac_ad + refrac_ad_re(:,1)
    refrac_ad_re(:,1) = 0.0_wp 
  ENDIF

!-------------------------------------------------------------------------------
! 11. Adjoint of refractivity calculation
!-------------------------------------------------------------------------------


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

!-------------------------------------------------------------------------------
! 12. Adjoint of water vapor and dry air pressure calculation
!-------------------------------------------------------------------------------

  x_ad%pres = x_ad%pres + pdry_ad 
  pwvp_ad = pwvp_ad - pdry_ad
  pdry_ad = 0.0_wp 
  
  x_ad%pres = x_ad%pres + pwvp_ad   &
              * (x%shum/(epsilon_water+(1.0_wp-epsilon_water)*x%shum))
  x_ad%shum = x_ad%shum + pwvp_ad   &
              * (x%pres/(epsilon_water+(1.0_wp-epsilon_water)*x%shum)         &
                 - x%pres * x%shum * (1.0_wp - epsilon_water) &
                   / ((epsilon_water + (1.0_wp-epsilon_water)*x%shum)         &
                    * (epsilon_water + (1.0_wp-epsilon_water)*x%shum)))
  pwvp_ad = 0.0_wp

!-------------------------------------------------------------------------------
! 13. Adjoint of the compressibilty calculation
!-------------------------------------------------------------------------------

  IF (x%non_ideal) THEN

! call the compressibility routines

    CALL ropp_fm_compress_ad &
   &(x,x_ad,z_geop_ad,zcomp_dry_inv_ad,zcomp_wet_inv_ad)  

  ENDIF

  x_ad%geop = x_ad%geop + z_geop_ad
  z_geop_ad = 0.0_wp

  zcomp_dry_inv_ad(:) = 0.0_wp
  zcomp_wet_inv_ad(:) = 0.0_wp


!-------------------------------------------------------------------------------
! 14. Clean up
!-------------------------------------------------------------------------------

  CALL message_set_routine(routine)
  
  DEALLOCATE(refrac_re)
  DEALLOCATE(refrac_ad_re)

END SUBROUTINE ropp_fm_bangle_1d_ad
