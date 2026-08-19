! $Id: ropp_fm_refrac_1d_new.f90 4060 2014-02-03 11:13:00Z idculv $

SUBROUTINE ropp_fm_refrac_1d_new(x, y)

!****s* Refractivity/ropp_fm_refrac_1d_new *
!
! NAME
!    ropp_fm_refrac_1d_new - Forward model calculating a one dimensional 
!                        refractivity profile from the state vector 
!                        assuming hydrostatic variation of refractivity 
!                        between model levels.
!
! SYNOPSIS
!    call ropp_fm_refrac_1d_new(x, y)
! 
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of 
!    refractivity from profiles of temperature, humidity and pressure. 
!    Refractivity values are calculated for the geopotential height levels 
!    given in the observation vector.
!
! INPUTS
!    type(State1dFM)     :: x     ! State vector structure
!    type(Obs1dRefrac)   :: y     ! Observation vector (levels required)
!
! OUTPUT
!    type(Obs1dRefrac)   :: y     ! Obs vector with forward modelled refrac
!
! NOTES
!    The forward model assumes that the state vector structure contains 
!    temperature, humidity and pressure values on common geopotential height 
!    levels, independent of the source of those data. Model-dependent 
!    conversion routines can be used to accomplish this within the 
!    ropp_fm_roprof2state() subroutine.
! 
!    The forward model assumes that the observation vector contains 
!    geopotential height levels onto which the forward simulated
!    observations are interpolated.
!
!    The interpolation of refractivity calculated at the state vector's
!    geopotential height levels to the observation vector's geopotential height
!    levels is carried out in temperature and specific humidity.
!    Temperature is interpolated linearly with geopotential height as independent variable.
!    Pressure is interpolated as log(p) with geopotential height as independent variable.
!    It is assumed that the pressure maintains hydrostatic balance.
!    If x%check_qmin == .TRUE. humidity is interpolated as log(q) with geopotential height as
!    independent variable. If x%check_qmin == .FALSE. specific humidity is interpolated
!    linearly with pressure as independent variable.  
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d_new_ad
!    ropp_fm_refrac_1d_new_tl
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
  USE ropp_utils, ONLY: ropp_ZDTV, ropp_MDFV
  USE ropp_fm,   not_this => ropp_fm_refrac_1d_new
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),   INTENT(in)    :: x       ! State vector
  TYPE(Obs1dRefrac), INTENT(inout) :: y       ! Observation vector

! Local variables
  REAL(wp), DIMENSION(x%n_lev)     :: z_geop          ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_dry_inv   ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_wet_inv   ! Wet compressibility

  REAL(wp)                         :: kap1,kap2,kap3  ! Refractivity coefficients used in routine

  REAL(wp)                         :: q_above_temp, q_below_temp ! temporary humidity values

  LOGICAL                          :: below_lowest_lev  ! .TRUE. if ob is below lowest model level
  LOGICAL                          :: above_highest_lev ! .TRUE. if ob is above highest model level

  INTEGER  :: i,j

  REAL(wp) :: beta,t_ob,alpha,q_ob,gamma     ! [Temporary variables for new
  REAL(wp) :: p_ob,ptemp_wet,ptemp_dry,pgrad ! refractivity interpolation]

  REAL(wp) :: zcomp_dry_inv_ob,zcomp_wet_inv_ob ! Comp. factors on ob level

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
! 3. New non-linear refractivity interpolation option - see RSR 15
!-------------------------------------------------------------------------------

! 3.1 Initialise refractivity to ropp_MDFV
! ----------------------------------------
  y%refrac(:) = ropp_MDFV

! 3.2 Cycle over each observation level
! -------------------------------------

  DO i=1, SIZE(y%geop)

    below_lowest_lev=.FALSE.
    above_highest_lev=.FALSE.

! 3.3 Initialise compressibilities for ideal case
! -----------------------------------------------

    zcomp_dry_inv_ob = 1.0_wp

    zcomp_wet_inv_ob = 1.0_wp

! 3.4 Find model layer (j to j+1) in which ob lies
! ------------------------------------------------

    DO j=1, x%n_lev-1
      IF (y%geop(i) < z_geop(1)) THEN
        below_lowest_lev=.TRUE.
        EXIT
      END IF
      IF (y%geop(i) > z_geop(x%n_lev)) THEN
        above_highest_lev=.TRUE.
        EXIT
      END IF
      IF (y%geop(i) > z_geop(j) .AND. y%geop(i) < z_geop(j+1)) THEN
        EXIT
      END IF
    END DO

! 3.5 If ob is below lowest model level or above highest model level, cycle
!--------------------------------------------------------------------------

    IF (below_lowest_lev .OR. above_highest_lev) THEN
      y%weights(i) = 0.0_wp
      CYCLE
    END IF

! 3.6 Calculate temperature
! -------------------------

    beta = (x%temp(j+1)-x%temp(j))/(z_geop(j+1)-z_geop(j))

    t_ob = x%temp(j) + beta * (y%geop(i) - z_geop(j))

! 3.7 Assume exponential pressure if T(j)~=T(j+1) to avoid P_ob=infinity
! ----------------------------------------------------------------------

    IF (ABS(x%temp(j+1) - x%temp(j)) < ropp_ZDTV) THEN
      IF (MIN(x%pres(j+1),x%pres(j)) <= 0.0_wp) THEN
        pgrad = (x%pres(j+1)-x%pres(j))/(z_geop(j+1)-z_geop(j))
        p_ob = x%pres(j) + pgrad * (y%geop(i) - z_geop(j))
      ELSE
        p_ob=x%pres(j) * EXP(LOG(x%pres(j+1)/x%pres(j))* &
          ((y%geop(i) - z_geop(j))/(z_geop(j+1)-z_geop(j))))
      END IF
    ELSE
      gamma = -g_wmo/r_dry * (LOG(x%temp(j+1)/x%temp(j)))/ &
        (LOG(x%pres(j+1)/x%pres(j)))
      p_ob = x%pres(j) * (t_ob / x%temp(j))**(-g_wmo / (r_dry * gamma))
    END IF

! 3.8 Calculate specific humidity
! -------------------------------

    q_above_temp = x%shum(j+1)

    q_below_temp = x%shum(j)

    IF ( x%check_qmin ) THEN

      IF (x%shum(j+1)<0.0_wp) q_above_temp = q_min

      IF (x%shum(j)<0.0_wp) q_below_temp = q_min

      alpha = LOG(q_below_temp / q_above_temp) / (z_geop(j+1) - z_geop(j))

      q_ob = q_below_temp * EXP(-alpha * (y%geop(i) - z_geop(j)))

    ELSE ! Linear interpolation of specific humidity with pressure

      alpha = (q_above_temp - q_below_temp) / (x%pres(j+1) - x%pres(j))

      q_ob = q_below_temp + alpha * (p_ob - x%pres(j))
      
    END IF

! 3.9 Non-ideal gas compressibility factors
! -----------------------------------------

    IF (x%non_ideal) THEN
      CALL ropp_fm_compress_single( &
        t_ob, p_ob, q_ob, zcomp_dry_inv_ob, zcomp_wet_inv_ob)
    END IF

! 3.10 Calculate refractivity
! --------------------------

    ptemp_wet = p_ob * q_ob / &
      (epsilon_water + (1.0_wp - epsilon_water) * q_ob)

    ptemp_dry = p_ob - ptemp_wet

    y%refrac(i) = kap1 * ptemp_dry * zcomp_dry_inv_ob/ t_ob +  &
                  kap2 * ptemp_wet * zcomp_wet_inv_ob/ t_ob**2 + &
                  kap3 * ptemp_wet * zcomp_wet_inv_ob/ t_ob

  END DO


  WHERE (y%weights<=0.0_wp)
    y%refrac=ropp_MDFV
  END WHERE
 

END SUBROUTINE ropp_fm_refrac_1d_new
