! $Id$
!
!****s* BendingAngle/ropp_fm_bangle_1d *
!
! NAME
!    ropp_fm_bangle_1d - Forward model calculating a one dimensional bending
!                        angle profile from the state vector.
!
! SYNOPSIS
!    CALL ropp_fm_bangle_1d(x, y)
! 
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of 
!    bending angles from profiles of temperature, humidity and pressure. 
!    Bending angle values are calculated for the impact parameters given in the
!    observation vector.
!
! INPUTS
!    TYPE(State1dFM)   :: x     ! State vector structure
!    TYPE(Obs1dBangle) :: y     ! Observation vector (levels required)
!
! OUTPUT
!    TYPE(Obs1dBangle) :: y     ! Obs vector with forward modelled bangle
!
! NOTES
!    The forward model assumes that the state vector structure contains 
!    temperature, humidity and pressure values on common geopotential height 
!    levels, independent of the source of those data. Model-dependent 
!    conversion routines can be used to accomplish this with the 
!    ropp_fm_roprof2state() subroutine.
!
!    The forward model assumes that the observation vector contains impact
!    parameters onto which the forward simulated observations are interpolated.
!
!    The interpolation of bending angles calculated at the state vector's
!    geopotential height levels to the observation vector's impact parameters 
!    is carried out assuming that bending angle varies exponentially with
!    impact parameter.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_bangle_1d_ad
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

SUBROUTINE ropp_fm_bangle_1d(x, y)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ropp_fm,   not_this => ropp_fm_bangle_1d
  USE ropp_fm_types
  USE ropp_fm_iono
  USE ropp_fm_constants
  USE geodesy

  IMPLICIT NONE

  TYPE(State1dFM),   INTENT(in)    :: x               ! State vector
  TYPE(Obs1dBangle), INTENT(inout) :: y               ! Observation vector

  REAL(wp), DIMENSION(x%n_lev)     :: pwvp            ! Partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)     :: pdry            ! Partial dry air pressure  
  REAL(wp), DIMENSION(x%n_lev)     :: refrac          ! Refractivity
  REAL(wp), DIMENSION(x%n_lev)     :: h               ! Geometric height
  REAL(wp), DIMENSION(x%n_lev)     :: impact          ! Impact parameter

! non-ideal gas

  REAL(wp), DIMENSION(x%n_lev)     :: z_geop          ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_dry_inv   ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_wet_inv   ! Wet compressibility

  REAL(wp)                         :: kap1,kap2,kap3  ! Refractivity coefficients used in routine
  REAL(wp)                         :: R_peak          ! Used in iono routines
  CHARACTER(LEN=256)               :: routine         ! For messaging

! spline

  REAL(wp), DIMENSION(:,:), ALLOCATABLE     :: refrac_re           ! refractivity
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1)       ::  refrac_int !vals interpolated to fixed heights.
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1)       ::  z_geop_int
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1)       ::  h_int
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1)       ::  impact_int
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) + 1)       ::  temp_int

!-------------------------------------------------------------------------------
! 2. Define routine name for messaging
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_fm_bangle_1d')

!-------------------------------------------------------------------------------
! 3. Non-ideal gas options
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
! 4. Calculate water vapor and dry air pressure 
!-------------------------------------------------------------------------------

  pwvp = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)

  pdry = x%pres - pwvp

!-------------------------------------------------------------------------------
! 5. Calculate refractivity
!-------------------------------------------------------------------------------

  refrac = kap1 * pdry * zcomp_dry_inv / x%temp    + &
           kap2 * pwvp * zcomp_wet_inv / x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv / x%temp


!-------------------------------------------------------------------------------
! 5.b Interpolation - spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------
  
  ALLOCATE(refrac_re(SIZE(refrac),1))

  IF (x%spline_int) THEN
  
    refrac_int(:) = ropp_MDFV    
    temp_int(:) = ropp_MDFV    
    
    refrac_re = RESHAPE(refrac, (/SIZE(refrac),1/))
    CALL ropp_fm_spline_ba(z_geop, x%ispline, refrac_re, z_geop_int, refrac_int)
 
  ENDIF

  DEALLOCATE(refrac_re)

!-------------------------------------------------------------------------------
! 6. Calculate geometric heights
!-------------------------------------------------------------------------------

  IF (x%spline_int) THEN
    h_int = geopotential2geometric(x%lat, z_geop_int)
  ELSE
    h = geopotential2geometric(x%lat, z_geop)
  END IF

!-------------------------------------------------------------------------------
! 7. Calculate impact parameter
!-------------------------------------------------------------------------------

  IF (y%undulation > ropp_MDTV) THEN
    IF (x%spline_int) THEN
      impact_int = (1.0_wp + 1.e-6_wp*refrac_int) * (h_int + y%r_curve + y%undulation)
    ELSE
      impact = (1.0_wp + 1.e-6_wp*refrac) * (h + y%r_curve + y%undulation)
    END IF
  ELSE
    CALL message(msg_warn, "Undulation missing. " // &
                 "Will assume to be zero when calculating impact parameters.")
    IF (x%spline_int) THEN
      impact_int = (1.0_wp + 1.e-6_wp*refrac_int) * (h_int + y%r_curve)
    ELSE
      impact = (1.0_wp + 1.e-6_wp*refrac) * (h + y%r_curve)
    END IF
  END IF

!-------------------------------------------------------------------------------
! 8. Calculate neutral bending angles
!-------------------------------------------------------------------------------

  IF (x%spline_int) THEN
    
    ! overwrite x%new_bangle_op to False, needed for spline interpolation 
    CALL ropp_fm_abel( &
    impact_int, refrac_int, temp_int, y%r_curve, .FALSE., y%impact, y%bangle)
  ELSE
    CALL ropp_fm_abel( &
    impact, refrac, x%temp, y%r_curve, x%new_bangle_op, y%impact, y%bangle)
  END IF

!-------------------------------------------------------------------------------
! 9. Calculate ionospheric contribution to bending if we are forward
!    modelling L1,L2 bending angles directly
!-------------------------------------------------------------------------------

  IF ( x%direct_ion ) THEN

    IF ( y%r_leo > ropp_MDTV ) THEN

      R_peak = x%H_peak + y%r_curve

      CALL ropp_fm_iono_bangle &
        (x%Ne_max, R_peak, x%H_width, y%R_leo, y%n_L1, y%impact, y%bangle)

    ELSE

      CALL message (msg_warn, 'Ionospheric component of bending incalculable ' // &
                              'because r_LEO is missing (or zero) ... setting iono bending to zero')

    END IF

  END IF

!-------------------------------------------------------------------------------
! 10. Adapt weights for missing values
!-------------------------------------------------------------------------------

  WHERE ( y%bangle < -900.0_wp ) y%weights = 0.0_wp

!-------------------------------------------------------------------------------
! 11. Clean up
!-------------------------------------------------------------------------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_fm_bangle_1d
