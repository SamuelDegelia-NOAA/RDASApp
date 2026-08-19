! $Id: ropp_fm_tdry_1d.f90 1960 2010-10-02 00:00:00Z idculv $

SUBROUTINE ropp_fm_tdry_1d(x, y, tdry)

!
! NAME
!    ropp_fm_tdry_1d - Forward model to calculate a one dimensional
!                      dry temperature profile from the state vector.
!                      Use linear interpolation from model levels to
!                      observation levels.
!
! SYNOPSIS
!    CALL ropp_fm_tdry_1d(x, y, tdry)
!
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of
!    dry temperatures from a model profile of temperature, humidity and
!    pressure.
!      Dry temperature values are calculated for the geopotential height
!    levels given in the observation vector.
!
! INPUTS
!    TYPE(State1dFM)     :: x       ! State vector
!    TYPE(Obs1dRefrac)   :: y       ! Observation vector (levels required)
!    REAL(wp)            :: tdry(:) ! Allocated vector for model dry temps
!
! OUTPUT
!    REAL(wp)            :: tdry(:) ! Model dry temps mapped to observation levels
!
! NOTES
!    The forward model assumes that the state vector structure contains
!    temperature, humidity and pressure values on common geopotential height
!    levels, independent of the source of those data. Model-dependent
!    conversion routines can be used to accomplish this within the
!    ropp_fm_roprof2state() subroutine.
!
!    Dry temperatures are calculated on model levels, from which they are
!    linearly interpolated onto the geopotential height levels that are
!    held in the observation vector.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d
!    ropp_fm_bangle_1d
!
! AUTHOR
!   Hans Gleisner, DMI.
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
  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm, not_this => ropp_fm_tdry_1d
  USE ropp_fm_types
  USE ropp_fm_constants
  USE geodesy

  IMPLICIT NONE

! Input/output variables
  TYPE(State1dFM),    INTENT(in)    :: x              ! State vector
  TYPE(Obs1dRefrac),  INTENT(in)    :: y              ! Observation vector
  REAL(wp),           INTENT(inout) :: tdry(:)        ! Model dry temps mapped to observation levels

! Local variables
  REAL(wp), DIMENSION(x%n_lev)      :: ppw            ! Partial pressure for water vapour
  REAL(wp), DIMENSION(x%n_lev)      :: ppd            ! Partial pressure for dry air
  REAL(wp), DIMENSION(x%n_lev)      :: refrac         ! Refractivity (on model levels)
  REAL(wp), DIMENSION(x%n_lev)      :: pdry_mod       ! Dry pressure (on model levels)
  REAL(wp), DIMENSION(x%n_lev)      :: tdry_mod       ! Dry temperature (on model levels)
  REAL(wp), DIMENSION(x%n_lev)      :: qdum_mod       ! Dummy water vapour (on model levels)
  REAL(wp), DIMENSION(x%n_lev)      :: alt            ! Geometric height of model levels
  REAL(wp), DIMENSION(x%n_lev)      :: z_geop         ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)      :: zcomp_dry_inv  ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)      :: zcomp_wet_inv  ! Wet compressibility
  REAL(wp)                          :: gtop           ! Gravitational acceleration at model top
  REAL(wp)                          :: Hscale         ! Scale height above model top
  INTEGER                           :: i

  REAL(wp)                          :: kap1,kap2,kap3 ! Refractivity coefficients used in routine

!-------------------------------------------------------------------------------
! 2. Non-ideal gas: adjust refractivity coefficients and geopotential
!                   heights of model levels.
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

! calculate compressibilty and adjust geopotential heights in z_geop

    CALL ropp_fm_compress(x,z_geop,zcomp_dry_inv,zcomp_wet_inv)

  ELSE

    kap1 = kappa1
    kap2 = kappa2
    kap3 = kappa3

  ENDIF

!-------------------------------------------------------------------------------
! 3. Calculate refractivity on model levels using partial pressures
!    for water vapor (ppw) and for dry air (ppd)
!    Note: the latter is not the same as "dry pressure" used in RO.
!-------------------------------------------------------------------------------

  ppw = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)
  ppd = x%pres - ppw

  refrac = kap1 * ppd * zcomp_dry_inv / x%temp    + &
           kap2 * ppw * zcomp_wet_inv / x%temp**2 + &
           kap3 * ppw * zcomp_wet_inv / x%temp

!-------------------------------------------------------------------------------
! 4. Obtain dry p and dry T on model levels by integrating the hydrostatic
!    equation downwards from the second highest model level. The model
!    pressure at that level gives the upper boundary condition for the
!    hydrostatic integration.
!    Tdry at the topmost level is given directly by the model temperature.
!    Pdry at the topmost level is given directly by the model pressure.
!-------------------------------------------------------------------------------

  alt      = geopotential2geometric(x%lat, z_geop)
  pdry_mod = ropp_MDFV
  tdry_mod = ropp_MDFV
  qdum_mod = 0.0_wp
  gtop     = gravity(y%lat, alt(x%n_lev-1))   ! + Hgeoid ?
  Hscale   = (kap1*x%pres(x%n_lev-1)*R_dry)/(gtop*refrac(x%n_lev-1)) ! upper boundary at the second highest model level

  CALL ropp_fm_tdry(y%lat, alt(1:x%n_lev-1), refrac(1:x%n_lev-1), &
                    qdum_mod(1:x%n_lev-1), tdry_mod(1:x%n_lev-1), &
                    pdry_mod(1:x%n_lev-1), Zscale=Hscale) ! , Zstep=100.0d0) ! upper boundary condition step size

  pdry_mod(x%n_lev) = x%pres(x%n_lev)
  tdry_mod(x%n_lev) = x%temp(x%n_lev)

!-------------------------------------------------------------------------------
! 5. Interpolate tdry from model levels to observation levels.
!    Below model top: linear interpolation.
!    Above model top: isothermal (dT/dh = 0), purely exponential (dln(N)/dz = -1/Hscale).
!-------------------------------------------------------------------------------

  !--- below model top
  CALL ropp_fm_interpol(z_geop, y%geop, tdry_mod, tdry)

  !--- above model top
  DO i=1,SIZE(y%geop)
    IF (y%geop(i) > x%geop(x%n_lev)) THEN
      tdry(i) = x%temp(x%n_lev) ! gtop*Hscale/R_dry
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! 6. Set tdry to missing below the lowest model level and above the model top.
!-------------------------------------------------------------------------------

  DO i=1,SIZE(y%geop)
    IF ( (y%geop(i) < x%geop(1)) .OR. (y%geop(i) > x%geop(x%n_lev)) ) THEN
      tdry(i) = ropp_MDFV
    ENDIF
  ENDDO


END SUBROUTINE ropp_fm_tdry_1d
