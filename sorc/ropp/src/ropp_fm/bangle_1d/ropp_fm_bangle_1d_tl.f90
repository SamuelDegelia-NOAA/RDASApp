! $Id$

SUBROUTINE ropp_fm_bangle_1d_tl(x, x_tl, y, y_tl)

!****s* BendingAngle/ropp_fm_bangle_1d_tl *
!
! NAME
!    ropp_fm_bangle_1d_tl - Tangent linear of ropp_fm_bangle_1d().
!
! SYNOPSIS
!    call ropp_fm_bangle_1d_tl(x, x_tl, y, y_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_bangle_1d.
!
! INPUTS
!    type(State1dFM)        :: x      ! State vector structure
!    type(State1dFM)        :: x_tl   ! Perturbation vector structure
!    type(Obs1dBangle)      :: y      ! Bending angle observation vector
!
! OUTPUT
!    real(wp), dimension(:) :: y_tl  ! Bending angle perturbation
!
! NOTES
!    The obs vector is required only for the observation's impact parameter
!    levels; no forward simulated bending angle profile is returned.
!
!    The lengths of the arrays x_tl%state and y_tl must agree with the 
!    lengths of the x%state and y%bangle arrays, respectively.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_bangle_1d
!    ropp_fm_bangle_1d_ad
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
  USE ropp_fm,   not_this => ropp_fm_bangle_1d_tl
  
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),   INTENT(in)                    :: x                ! State vector
  TYPE(State1dFM),   INTENT(in)                    :: x_tl             ! State perturbation
  TYPE(Obs1dBangle), INTENT(in)                    :: y                ! Obs vector
  REAL(wp), DIMENSION(SIZE(y%bangle)), INTENT(out) :: y_tl             ! Obs perturbation

  REAL(wp), DIMENSION(x%n_lev)                     :: pwvp             ! Partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)                     :: pwvp_tl          ! Pwvp perturbation
  REAL(wp), DIMENSION(x%n_lev)                     :: pdry             ! Dry pressure
  REAL(wp), DIMENSION(x%n_lev)                     :: pdry_tl          ! Pdry perturbation
  REAL(wp), DIMENSION(x%n_lev)                     :: refrac           ! Refractivity on bg model levels
  REAL(wp), DIMENSION(x%n_lev)                     :: refrac_tl        ! Refractivity perturbation

  REAL(wp), DIMENSION(x%n_lev)                     :: z_geop           ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)                     :: z_geop_tl        ! GPH perturbation
  REAL(wp), DIMENSION(x%n_lev)                     :: zcomp_dry_inv    ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)                     :: zcomp_dry_inv_tl ! Dry compressibility perturbation
  REAL(wp), DIMENSION(x%n_lev)                     :: zcomp_wet_inv    ! Wet compressibility
  REAL(wp), DIMENSION(x%n_lev)                     :: zcomp_wet_inv_tl ! Wet compressibility perturbation

  REAL(wp), DIMENSION(x%n_lev)                     :: h                ! Geometric height
  REAL(wp), DIMENSION(x%n_lev)                     :: h_tl             ! Geometric height perturbation
  REAL(wp), DIMENSION(x%n_lev)                     :: impact           ! Impact parameter
  REAL(wp), DIMENSION(x%n_lev)                     :: impact_tl        ! Impact perturbation
  REAL(wp), DIMENSION(SIZE(y%bangle))              :: bangle           ! Bending angle

  REAL(wp)                                         :: kap1,kap2,kap3   ! Refractivity coefficients used in routine
  REAL(wp)                                         :: R_peak           ! Radius of Chapman layer peak
  REAL(wp)                                         :: R_peak_tl        ! Radius of Chapman layer peak perturbation
  CHARACTER(LEN=256)                               :: routine          ! For messaging

  !spline

  REAL(wp), DIMENSION(:,:), ALLOCATABLE            :: refrac_re        ! Refractivity
  REAL(wp), DIMENSION(:,:), ALLOCATABLE            :: refrac_tl_re     ! Refractivity perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: refrac_int       ! Interpolated refrac
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: refrac_int_tl    ! Interpolated refrac perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: z_geop_int       ! interpolated geoptential height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: z_geop_int_tl    ! interpolated geoptential height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: h_int            ! Interpolated geometric height
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: h_int_tl         ! Interpolated geometric height perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: impact_int       ! Interpolated nr
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: impact_int_tl    ! Interpolated nr perturbation
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: temp_int         ! Interpolated temperature
  REAL(wp), DIMENSION(x%ispline * (x%n_lev - 1) +1)  :: temp_int_tl      ! Interpolated temperature perturbation

!-------------------------------------------------------------------------------
! 2. Define routine name for messaging
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_fm_bangle_1d_tl')

!-------------------------------------------------------------------------------
! 3. Non ideal gas options 
!-------------------------------------------------------------------------------

! set inverse of compressibilities

  zcomp_dry_inv(:) = 1.0_wp
  zcomp_wet_inv(:) = 1.0_wp
 
  zcomp_dry_inv_tl(:) = 0.0_wp
  zcomp_wet_inv_tl(:) = 0.0_wp
    
! initialise geopotential heights
   
  z_geop(:) = x%geop(:)
  z_geop_tl(:) = x_tl%geop(:)

  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

    kap1 = kappa1_comp
    kap2 = kappa2_comp
    kap3 = kappa3_comp

!    calculate compressibilty and adjust geopotential heights in z_geop

    CALL ropp_fm_compress_tl &
    &(x,x_tl,z_geop,z_geop_tl,zcomp_dry_inv,zcomp_dry_inv_tl,&
    &zcomp_wet_inv,zcomp_wet_inv_tl)

  ELSE

    kap1 = kappa1
    kap2 = kappa2
    kap3 = kappa3

  ENDIF

!-------------------------------------------------------------------------------
! 4. Calculate water vapor  and dry pressure pressure
!-------------------------------------------------------------------------------

  pwvp = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)

  pwvp_tl = pwvp * ( x_tl%pres/x%pres + x_tl%shum/x%shum &
                - x_tl%shum*pwvp*(1.0_wp - epsilon_water) / (x%pres*x%shum))

! dry pressure

  pdry = x%pres - pwvp
  pdry_tl =  x_tl%pres - pwvp_tl

!-------------------------------------------------------------------------------
! 5. Calculate refractivity
!-------------------------------------------------------------------------------

  refrac = kap1 * pdry * zcomp_dry_inv / x%temp    + &
           kap2 * pwvp * zcomp_wet_inv / x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv / x%temp

  refrac_tl = kap1 * pdry_tl * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp_tl * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp_tl * zcomp_wet_inv/ x%temp + &
           kap1 * pdry * zcomp_dry_inv_tl/ x%temp + &
           kap2 * pwvp * zcomp_wet_inv_tl/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv_tl/ x%temp - &
           (kap1 * pdry * zcomp_dry_inv/ x%temp**2 + &
           2.0_wp *kap2 * pwvp * zcomp_wet_inv/ x%temp**3 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp**2)* x_tl%temp


!-------------------------------------------------------------------------------
! 5.b Interpolation - spline (optional)
!     - interpolate refractivity to a finer grid with a cubic spline
!-------------------------------------------------------------------------------
  
  ALLOCATE(refrac_re(SIZE(refrac),1))
  ALLOCATE(refrac_tl_re(SIZE(refrac),1))

  IF (x%spline_int) THEN
  
    refrac_int(:) = ropp_MDFV    
    temp_int(:) = ropp_MDFV    
   
    !refrac_int_tl(:) = 0.0_wp !ropp_MDFV    
    temp_int_tl(:) = 0.0_wp    
     
    refrac_re = RESHAPE(refrac, (/SIZE(refrac),1/))
    refrac_tl_re = RESHAPE(refrac_tl, (/SIZE(refrac_tl),1/))
    
    CALL ropp_fm_spline_ba(z_geop, x%ispline, refrac_re,  z_geop_int, refrac_int)

    CALL ropp_fm_spline_ba_tl(z_geop, x%ispline, refrac_re,  z_geop_int,  z_geop_tl, refrac_tl_re, z_geop_int_tl, refrac_int_tl)
  ENDIF

  DEALLOCATE(refrac_re)
  DEALLOCATE(refrac_tl_re)
!-------------------------------------------------------------------------------
! 6. Calculate geometric height
!-------------------------------------------------------------------------------

  IF (x%spline_int) THEN
  
    h_int    = y%r_earth * z_geop_int / (y%g_sfc / g_wmo * y%r_earth - z_geop_int)
  
    h_int_tl = z_geop_int_tl * (y%r_earth/(((y%g_sfc/g_wmo)*y%r_earth)-z_geop_int) &
               + y%r_earth*z_geop_int/((((y%g_sfc/g_wmo)*y%r_earth)-z_geop_int)**2))

  ELSE

    h    = y%r_earth * z_geop / (y%g_sfc / g_wmo * y%r_earth - z_geop)
  
    h_tl = z_geop_tl * (y%r_earth/(((y%g_sfc/g_wmo)*y%r_earth)-z_geop) &
               + y%r_earth*z_geop/((((y%g_sfc/g_wmo)*y%r_earth)-z_geop)**2))
  ENDIF

!-------------------------------------------------------------------------------
! 7. Calculate impact parameter
!-------------------------------------------------------------------------------

  IF (y%undulation > ropp_MDTV) THEN
    IF (x%spline_int) THEN
      impact_int = (1.0_wp + 1.e-6_wp*refrac_int) * (h_int + y%r_curve + y%undulation)
      impact_int_tl = 1.0e-6_wp * refrac_int_tl * (h_int + y%r_curve + y%undulation) &
                  + h_int_tl * (1.0_wp + 1.0e-6_wp*refrac_int)
    ELSE

      impact    = (1.0_wp + 1.0e-6_wp * refrac) * (h + y%r_curve + y%undulation)
      impact_tl = 1.0e-6_wp * refrac_tl * (h + y%r_curve + y%undulation) &
                  + h_tl * (1.0_wp + 1.0e-6_wp*refrac)
    ENDIF
  ELSE
    CALL message(msg_warn, "Undulation missing. " // &
                 "Will assume to be zero when calculating full and " // &
                 "perturbed impact parameters.")
    IF (x%spline_int) THEN
      impact_int    = (1.0_wp + 1.e-6_wp*refrac_int) * (h_int + y%r_curve)
      impact_int_tl = 1.0e-6_wp * refrac_int_tl * (h_int + y%r_curve) &
                  + h_int_tl * (1.0_wp + 1.0e-6_wp*refrac_int)
    ELSE
      impact    = (1.0_wp + 1.0e-6_wp * refrac) * (h + y%r_curve)
      impact_tl = 1.0e-6_wp * refrac_tl * (h + y%r_curve) &
                  + h_tl * (1.0_wp + 1.0e-6_wp*refrac)
    ENDIF
  END IF
  
!-------------------------------------------------------------------------------
! 8. Calculate neutral bending angle
!-------------------------------------------------------------------------------
 
 IF (x%spline_int) THEN

    CALL ropp_fm_abel_tl( &
      impact_int, refrac_int, temp_int, temp_int_tl, y%r_curve, .FALSE., &
      y%impact, impact_int_tl, refrac_int_tl, y_tl)
  ELSE

    CALL ropp_fm_abel_tl( &
      impact, refrac, x%temp, x_tl%temp, y%r_curve, x%new_bangle_op, &
      y%impact, impact_tl, refrac_tl, y_tl)
  ENDIF

!-------------------------------------------------------------------------------
! 9. Calculate the ionospheric bending if L1 and L2 are used in retrieval
!-------------------------------------------------------------------------------

  IF ( x%direct_ion .AND. (y%r_leo > ropp_MDTV) ) THEN

  ! Need neutral bending angle for a test in ropp_fm_iono_bangle_tl
    IF (x%spline_int) THEN
      
      CALL ropp_fm_abel(impact_int, refrac_int, temp_int, y%r_curve, &
                      .FALSE., y%impact, bangle)
    ELSE

      CALL ropp_fm_abel(impact, refrac, x%temp, y%r_curve, &
                      x%new_bangle_op, y%impact, bangle)
    ENDIF

    R_peak = x%H_peak + y%r_curve

    R_peak_tl = x_tl%H_peak

    CALL ropp_fm_iono_bangle_tl(x%Ne_max,    R_peak,    x%H_width, y%R_leo, &
                                x_tl%Ne_max, R_peak_tl, x_tl%H_width, &
                                y%n_L1, y%impact, bangle, y_tl)

  END IF

!-------------------------------------------------------------------------------
! 10. Clean up
!-------------------------------------------------------------------------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_fm_bangle_1d_tl
