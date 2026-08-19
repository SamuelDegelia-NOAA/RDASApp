! $Id: ropp_fm_refrac_1d_new_tl.f90 4060 2014-02-03 11:13:00Z idculv $

SUBROUTINE ropp_fm_refrac_1d_new_tl(x, x_tl, y, y_tl)

!****s* Refractivity/ropp_fm_refrac_1d_new_tl *
!
! NAME
!    ropp_fm_refrac_1d_new_tl - Tangent linear of ropp_fm_refrac_1d_new().
!
! SYNOPSIS
!    call ropp_fm_refrac_1d_new_tl(x, x_tl, y, y_tl)
!
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_refrac_1d_new.
!
! INPUTS
!    type(State1dFM)             :: x      ! State vector
!    type(State1dFM)             :: x_tl   ! Perturbation vector
!    type(Obs1dRefrac)           :: y      ! Observation vector
!
! OUTPUT
!    real(wp), dimension(:)      :: y_tl   ! Observation tangent linear
!
! NOTES
!    The obs vector is required only for the observation's geopotential height
!    levels; no forward simulated refractivity profile is returned.
!
!    The lengths of the arrays x_tl%state and y_tl must agree with the
!    lengths of the x%state and y%refrac arrays, respectively.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d_new
!    ropp_fm_refrac_1d_new_ad
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
  USE ropp_utils, ONLY: ropp_ZDTV
  USE ropp_fm,   not_this => ropp_fm_refrac_1d_new_tl
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),        INTENT(in)  :: x                ! state vector
  TYPE(State1dFM),        INTENT(in)  :: x_tl             ! state perturbation
  TYPE(Obs1dRefrac),      INTENT(in)  :: y                ! obs vector
  REAL(wp), DIMENSION(:), INTENT(out) :: y_tl             ! obs perturbation

  REAL(wp), DIMENSION(x%n_lev)        :: z_geop           ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)        :: z_geop_tl        ! GPH perturbation

  REAL(wp), DIMENSION(x%n_lev)        :: zcomp_dry_inv    ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)        :: zcomp_dry_inv_tl ! Dry compressibility perturbation

  REAL(wp), DIMENSION(x%n_lev)        :: zcomp_wet_inv    ! Wet compressibility
  REAL(wp), DIMENSION(x%n_lev)        :: zcomp_wet_inv_tl ! Wet compressibility perturbation

  REAL(wp)                            :: kap1,kap2,kap3   ! Refractivity coefficients used in routine

  REAL(wp)                            :: q_above_temp, q_below_temp ! temporary humidity values
  REAL(wp)                            :: q_above_temp_tl, q_below_temp_tl

  INTEGER  :: i,j

  LOGICAL                             :: below_lowest_lev  ! .TRUE. if ob is below lowest model level
  LOGICAL                             :: above_highest_lev  ! .TRUE. if ob is above highest model level

! Temporary variables and TLs for new refractivity interpolation
  REAL(wp) :: beta,beta_tl,t_ob,t_ob_tl
  REAL(wp) :: alpha,alpha_tl
  REAL(wp) :: q_ob,q_ob_tl
  REAL(wp) :: gamma,gamma_tl
  REAL(wp) :: p_ob,p_ob_tl
  REAL(wp) :: pgrad,pgrad_tl
  REAL(wp) :: n_ob,n_ob_tl
  REAL(wp) :: zcomp_dry_inv_ob, zcomp_dry_inv_ob_tl
  REAL(wp) :: zcomp_wet_inv_ob, zcomp_wet_inv_ob_tl
  REAL(wp) :: ptemp_wet,ptemp_wet_tl
  REAL(wp) :: ptemp_dry,ptemp_dry_tl


!-------------------------------------------------------------------------------
! 2. Non ideal gas options
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

! calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress_tl &
     &(x,x_tl,z_geop,z_geop_tl,zcomp_dry_inv,zcomp_dry_inv_tl,&
     &zcomp_wet_inv,zcomp_wet_inv_tl)

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3

  ENDIF

!-------------------------------------------------------------------------------
! 3. New non-linear refractivity interpolation option - see RSR 15
!-------------------------------------------------------------------------------

! 3.1 Cycle over each observation level
! -------------------------------------

  DO i=1, SIZE(y%geop)

    below_lowest_lev=.FALSE.
    above_highest_lev=.FALSE.

! 3.2 Initialise compressibilities for ideal case
! -----------------------------------------------

    zcomp_dry_inv_ob = 1.0_wp
    zcomp_wet_inv_ob = 1.0_wp
    zcomp_dry_inv_ob_tl = 0.0_wp
    zcomp_wet_inv_ob_tl = 0.0_wp

! 3.3 Find model layer (j to j+1) in which ob lies
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

! 3.4 If ob is below lowest model level or above highest model level, cycle
! -------------------------------------------------------------------------

    IF (below_lowest_lev .OR. above_highest_lev) THEN
      y_tl(i) = 0.0_wp
      CYCLE
    END IF

! 3.5 Calculate temperature
! -------------------------

    beta = (x%temp(j+1) - x%temp(j)) / (z_geop(j+1) - z_geop(j))

    beta_tl = (-1.0_wp / (z_geop(j+1) - z_geop(j))) * x_tl%temp(j)   + &
              ( 1.0_wp / (z_geop(j+1) - z_geop(j))) * x_tl%temp(j+1) + &
              ( beta / (z_geop(j+1) - z_geop(j))) * z_geop_tl(j)     + &
              (-beta / (z_geop(j+1) - z_geop(j))) * z_geop_tl(j+1)

    t_ob = x%temp(j) + beta * (y%geop(i) - z_geop(j))

    t_ob_tl = x_tl%temp(j) + (y%geop(i) - z_geop(j)) * beta_tl + &
              (-beta) * z_geop_tl(j)

! 3.6 Assume exponential pressure if T(j)~=T(j+1) to avoid P_ob=infinity
! ----------------------------------------------------------------------

    IF (ABS(x%temp(j+1) - x%temp(j)) < ropp_ZDTV) THEN
      IF (MIN(x%pres(j+1),x%pres(j)) <= 0.0_wp) THEN
        pgrad = (x%pres(j+1)-x%pres(j))/(z_geop(j+1)-z_geop(j))
        pgrad_tl = (-1.0_wp / (z_geop(j+1)-z_geop(j))) * x_tl%pres(j) + &
                   ( 1.0_wp / (z_geop(j+1)-z_geop(j))) * x_tl%pres(j+1) + &
                   ( pgrad/(z_geop(j+1)-z_geop(j))) * z_geop_tl(j) + &
                   (-pgrad/(z_geop(j+1)-z_geop(j))) * z_geop_tl(j+1)


        p_ob = x%pres(j) + pgrad * (y%geop(i) - z_geop(j))
        p_ob_tl = (1.0_wp) * x_tl%pres(j) + &
                  (y%geop(i) - z_geop(j)) * pgrad_tl + &
                  (-pgrad) * z_geop_tl(j)
      ELSE
        p_ob = x%pres(j) * &
          EXP(LOG(x%pres(j+1)/x%pres(j))*((y%geop(i) - z_geop(j)) / &
          (z_geop(j+1)-z_geop(j))))

        p_ob_tl = &
          ((1.0_wp - ((y%geop(i) - z_geop(j))/(z_geop(j+1)-z_geop(j)))) * &
          EXP(LOG(x%pres(j+1)/x%pres(j))*((y%geop(i) - z_geop(j)) / & 
          (z_geop(j+1)- z_geop(j))))) * x_tl%pres(j) + &
          ((((y%geop(i) - z_geop(j)) / &
          (z_geop(j+1)-z_geop(j)))*(x%pres(j)/x%pres(j+1))) * &
          EXP(LOG(x%pres(j+1) / x%pres(j)) * &
          ((y%geop(i) - z_geop(j))/(z_geop(j+1)-z_geop(j)))))*x_tl%pres(j+1) + &
          (LOG(x%pres(j+1)/x%pres(j))*p_ob*(y%geop(i) - z_geop(j+1)) / &
          (z_geop(j+1)- z_geop(j))**2) * z_geop_tl(j) + &
          (LOG(x%pres(j+1)/x%pres(j))*p_ob*(z_geop(j) - y%geop(i)) / &
          (z_geop(j+1)-z_geop(j))**2) * z_geop_tl(j+1)
      END IF

    ELSE

      gamma = -g_wmo/r_dry * &
        (LOG(x%temp(j+1)/x%temp(j)))/(LOG(x%pres(j+1)/x%pres(j)))

      gamma_tl = &
        (-(g_wmo/r_dry)*LOG(x%temp(j+1)/x%temp(j)) / &
        (x%pres(j)*(LOG(x%pres(j+1)/x%pres(j)))**2)) * x_tl%pres(j) + &
        ((g_wmo/r_dry)*LOG(x%temp(j+1)/x%temp(j))/ &
        (x%pres(j+1)*(LOG(x%pres(j+1)/x%pres(j)))**2)) * x_tl%pres(j+1) + &
        ( (g_wmo/r_dry)/(x%temp(j)*LOG(x%pres(j+1)/x%pres(j)))) * &
        x_tl%temp(j) + &
        (-(g_wmo/r_dry)/(x%temp(j+1)*LOG(x%pres(j+1)/x%pres(j)))) * &
        x_tl%temp(j+1)

      p_ob = x%pres(j) * (t_ob / x%temp(j))**(-g_wmo / (r_dry * gamma))

      p_ob_tl = (p_ob / x%pres(j)) * x_tl%pres(j) + &
        (g_wmo/(r_dry*gamma) * x%pres(j)*t_ob/( x%temp(j)**2) * &
        (t_ob/x%temp(j))**(-g_wmo/(r_dry*gamma)-1.0_wp)) * x_tl%temp(j) + &
        (g_wmo/(r_dry*gamma**2) * p_ob * LOG(t_ob/x%temp(j))) * gamma_tl + &
        ((-g_wmo / (x%temp(j)*r_dry*gamma))*x%pres(j)*(t_ob / x%temp(j))** &
        (-g_wmo / (r_dry * gamma)-1.0_wp)) * t_ob_tl

    END IF

! 3.7 Calculate specific humidity
! -------------------------------

    q_above_temp = x%shum(j+1)
    q_above_temp_tl = x_tl%shum(j+1)

    q_below_temp = x%shum(j)
    q_below_temp_tl = x_tl%shum(j)

    IF ( x%check_qmin ) THEN

      IF (x%shum(j+1)<0.0_wp) THEN
        q_above_temp = q_min
        q_above_temp_tl = 0.0_wp
      END IF

      IF (x%shum(j)<0.0_wp) THEN
        q_below_temp = q_min
        q_below_temp_tl = 0.0_wp
      END IF

      alpha = LOG(q_below_temp / q_above_temp) / (z_geop(j+1) - z_geop(j))

      alpha_tl = &
        ( (1.0_wp/(z_geop(j+1) - z_geop(j)))/q_below_temp) * q_below_temp_tl + &
        (-(1.0_wp/(z_geop(j+1) - z_geop(j)))/q_above_temp) * q_above_temp_tl + &
        ( alpha / (z_geop(j+1) - z_geop(j))) * z_geop_tl(j) + &
        (-alpha / (z_geop(j+1) - z_geop(j))) * z_geop_tl(j+1)

      q_ob = q_below_temp * EXP(-alpha * (y%geop(i) - z_geop(j)))

      q_ob_tl = (q_ob / q_below_temp) * q_below_temp_tl + &
        (-(y%geop(i) - z_geop(j)) * q_ob) * alpha_tl + &
        (alpha * q_ob) * z_geop_tl(j)

    ELSE ! Linear interpolation of specific humidity with pressure

      alpha = (q_above_temp - q_below_temp) / (x%pres(j+1) - x%pres(j))

      alpha_tl = (-1.0_wp / (x%pres(j+1) - x%pres(j))) * q_below_temp_tl + &
              ( 1.0_wp / (x%pres(j+1) - x%pres(j))) * q_above_temp_tl + &
              ( alpha / (x%pres(j+1) - x%pres(j))) * x_tl%pres(j)     + &
              (-alpha / (x%pres(j+1) - x%pres(j))) * x_tl%pres(j+1)

      q_ob = q_below_temp + alpha * (p_ob - x%pres(j))

      q_ob_tl = q_below_temp_tl + (p_ob - x%pres(j)) * alpha_tl + &
           (-alpha) * x_tl%pres(j) + alpha * p_ob_tl

    ENDIF

! 3.8 Non-ideal gas compressibility factors
! -----------------------------------------

    IF (x%non_ideal) THEN
      CALL ropp_fm_compress_single_tl&
        (t_ob, p_ob, q_ob, t_ob_tl, p_ob_tl, q_ob_tl, zcomp_dry_inv_ob, &
        zcomp_dry_inv_ob_tl, zcomp_wet_inv_ob, zcomp_wet_inv_ob_tl)
    END IF

! 3.9 Calculate refractivity
! --------------------------
    ptemp_wet = p_ob*q_ob / (epsilon_water + (1.0_wp - epsilon_water) * q_ob)

    ptemp_wet_tl = (ptemp_wet / p_ob) * p_ob_tl + &
      (p_ob * epsilon_water / (epsilon_water + (1.0_wp - epsilon_water) * &
       q_ob)**2) * q_ob_tl

    ptemp_dry = p_ob - ptemp_wet

    ptemp_dry_tl = p_ob_tl - ptemp_wet_tl

    n_ob = kap1 * ptemp_dry * zcomp_dry_inv_ob / t_ob +  &
           kap2 * ptemp_wet * zcomp_wet_inv_ob / t_ob**2 + &
           kap3 * ptemp_wet * zcomp_wet_inv_ob / t_ob

    n_ob_tl = (kap1 * zcomp_dry_inv_ob /t_ob) * ptemp_dry_tl + &
      (-(kap1 * ptemp_dry * zcomp_dry_inv_ob / t_ob + &
      2.0_wp * kap2 * ptemp_wet * zcomp_wet_inv_ob / t_ob**2 + &
      kap3 * ptemp_wet * zcomp_wet_inv_ob / t_ob) / t_ob) * t_ob_tl + &
      (kap2 * zcomp_wet_inv_ob / t_ob**2 + &
      kap3 * zcomp_wet_inv_ob / t_ob) * ptemp_wet_tl + &
      (kap1 * ptemp_dry / t_ob) * zcomp_dry_inv_ob_tl + &
      (kap2 * ptemp_wet / t_ob**2 + &
      kap3 * ptemp_wet / t_ob) * zcomp_wet_inv_ob_tl

    y_tl(i) = n_ob_tl

  END DO


END SUBROUTINE ropp_fm_refrac_1d_new_tl
