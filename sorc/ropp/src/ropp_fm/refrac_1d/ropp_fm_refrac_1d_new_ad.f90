! $Id: ropp_fm_refrac_1d_new_ad.f90 4060 2014-02-03 11:13:00Z idculv $

SUBROUTINE ropp_fm_refrac_1d_new_ad(x, x_ad, y, y_ad)

!****s* Refractivity/ropp_fm_refrac_1d_new_ad *
!
! NAME
!    ropp_fm_refrac_1d_ad - Adjoint of ropp_fm_refrac_1d_new().
!
! SYNOPSIS
!    call ropp_fm_refrac_1d_new_ad(x, x_ad, y, y_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_refrac_1d_new.
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
!    ropp_fm_refrac_1d_new
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
  USE ropp_utils, ONLY: ropp_ZDTV
  USE ropp_fm,   not_this => ropp_fm_refrac_1d_new_ad
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),        INTENT(in)    :: x                ! State vector
  TYPE(State1dFM),        INTENT(inout) :: x_ad             ! State vector adjoint
  TYPE(Obs1dRefrac),      INTENT(in)    :: y                ! Observation vector
  REAL(wp), DIMENSION(:), INTENT(inout) :: y_ad             ! Observation adjoint

  REAL(wp)                              :: refrac_ob        ! Refractivity on ob height

  REAL(wp), DIMENSION(x%n_lev)          :: z_geop           ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)          :: z_geop_ad        ! GPH perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_dry_inv    ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_dry_inv_ad ! Dry compressibility perturbation

  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_wet_inv    ! Wet compressibility
  REAL(wp), DIMENSION(x%n_lev)          :: zcomp_wet_inv_ad ! Wet compressibility perturbation

  REAL(wp)                              :: kap1,kap2,kap3   ! Refractivity coefficients used in routine

  LOGICAL                               :: below_lowest_lev ! .TRUE. if ob is below lowest bg lev
  LOGICAL                               :: above_highest_lev ! .TRUE. if ob is above highest model level

  INTEGER  ::  i,j

! Temporary variables and ADs for new refractivity interpolation
  REAL(wp) ::  q_above_temp, q_below_temp, qabove_temp_ad, qbelow_temp_ad
  REAL(wp) ::  beta,beta_ad
  REAL(wp) ::  t_ob,Tob_ad
  REAL(wp) ::  alpha,alpha_ad
  REAL(wp) ::  q_ob,Qob_ad
  REAL(wp) ::  p_ob,Pob_ad
  REAL(wp) ::  pgrad,pgrad_ad
  REAL(wp) ::  gamma,gamma_ad
  REAL(wp) ::  Qbelow_ad
  REAL(wp) ::  Qabove_ad,Tbelow_ad,Tabove_ad,Pbelow_ad,Pabove_ad
  REAL(wp) ::  geopbelow_ad,geopabove_ad
  REAL(wp) ::  ptemp_dry, ptemp_wet
  REAL(wp) ::  zcomp_dry_inv_ob,zcomp_wet_inv_ob
  REAL(wp) ::  ptemp_dry_ad, ptemp_wet_ad
  REAL(wp) ::  zcomp_dry_inv_ob_ad,zcomp_wet_inv_ob_ad

!-------------------------------------------------------------------------------
! 2. Reset local adjoint variables
!-------------------------------------------------------------------------------

  z_geop_ad = 0.0_wp
  zcomp_dry_inv_ad = 0.0_wp
  zcomp_wet_inv_ad = 0.0_wp

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

! 3.1 Cycle over each observation level
! -------------------------------------

  DO i=1, SIZE(y%geop)

    below_lowest_lev=.FALSE.
    above_highest_lev=.FALSE.

    zcomp_dry_inv_ob = 1.0_wp
    zcomp_wet_inv_ob = 1.0_wp

! 3.2 Find model layer (j to j+1) in which ob lies
! ------------------------------------------------

    DO j=1,x%n_lev-1
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


! 3.3 If ob is below lowest model level or above highest model level, cycle
! -------------------------------------------------------------------------

    IF (below_lowest_lev .OR. above_highest_lev) THEN
      CYCLE
    END IF

! 3.4 Calculate temperature
! -------------------------

    beta = (x%temp(j+1) - x%temp(j)) / (z_geop(j+1) - z_geop(j))
    t_ob = x%temp(j) + beta * (y%geop(i) - z_geop(j))

! 3.5 Assume exponential pressure if T(j)~=T(j+1) to avoid P_ob=infinity
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
      gamma = -g_wmo/r_dry * &
        (LOG(x%temp(j+1)/x%temp(j)))/(LOG(x%pres(j+1)/x%pres(j)))
      p_ob = x%pres(j) * (t_ob / x%temp(j))**(-g_wmo / (r_dry * gamma))
    END IF

! 3.6 Calculate specific humidity
! -------------------------------

    q_above_temp = x%shum(j+1)
    q_below_temp = x%shum(j)
    IF (x%check_qmin) THEN
      IF (q_above_temp<0.0_wp) q_above_temp = q_min
      IF (q_below_temp<0.0_wp) q_below_temp = q_min
      alpha = LOG(q_below_temp / q_above_temp) / (z_geop(j+1) - z_geop(j))
      q_ob = q_below_temp * EXP(-alpha * (y%geop(i) - z_geop(j)))
    ELSE
      alpha = (q_above_temp - q_below_temp) / (x%pres(j+1) - x%pres(j))
      q_ob = q_below_temp + alpha * (p_ob - x%pres(j))
    ENDIF

! 3.7 Non-ideal gas compressibility factors
! -----------------------------------------

    IF (x%non_ideal) THEN
      CALL ropp_fm_compress_single( &
        t_ob,p_ob,q_ob, zcomp_dry_inv_ob, zcomp_wet_inv_ob)
    END IF

! 3.8 Calculate refractivity
! --------------------------

    ptemp_wet = p_ob * q_ob / &
      (epsilon_water + (1.0_wp - epsilon_water) * q_ob)

    ptemp_dry = p_ob - ptemp_wet

    refrac_ob = kap1 * ptemp_dry * zcomp_dry_inv_ob / t_ob +  &
                kap2 * ptemp_wet * zcomp_wet_inv_ob / t_ob**2 + &
                kap3 * ptemp_wet * zcomp_wet_inv_ob / t_ob

! 3.9 Adjoint of forward model calculations
! -----------------------------------------

! 3.9.1 Initialise adjoint variables

    Pob_ad = 0.0_wp
    Tob_ad = 0.0_wp
    Qob_ad = 0.0_wp
    gamma_ad = 0.0_wp
    alpha_ad = 0.0_wp
    beta_ad = 0.0_wp
    Qbelow_ad = 0.0_wp
    Qabove_ad = 0.0_wp
    Qbelow_temp_ad = 0.0_wp
    Qabove_temp_ad = 0.0_wp
    Tbelow_ad = 0.0_wp
    Tabove_ad = 0.0_wp
    Pbelow_ad = 0.0_wp
    Pabove_ad = 0.0_wp
    pgrad_ad = 0.0_wp
    geopbelow_ad = 0.0_wp
    geopabove_ad = 0.0_wp
    ptemp_dry_ad = 0.0_wp
    ptemp_wet_ad = 0.0_wp
    zcomp_dry_inv_ob_ad = 0.0_wp
    zcomp_wet_inv_ob_ad = 0.0_wp

! 3.9.2 Calculate adjoint variables

    y_ad = y_ad * y%weights

    ptemp_dry_ad = ptemp_dry_ad + (kap1 * zcomp_dry_inv_ob / t_ob) * y_ad(i)

    ptemp_wet_ad = ptemp_wet_ad + &
      (kap2 * zcomp_wet_inv_ob / t_ob**2 + kap3 * &
      zcomp_wet_inv_ob / t_ob) * y_ad(i)

    zcomp_dry_inv_ob_ad = zcomp_dry_inv_ob_ad + &
      (kap1 * ptemp_dry / t_ob) * y_ad(i)

    zcomp_wet_inv_ob_ad = zcomp_wet_inv_ob_ad + &
      (kap2 * ptemp_wet / t_ob**2 + &
      kap3 * ptemp_wet / t_ob) * y_ad(i)

    Tob_ad = Tob_ad + &
      (-(kap1 * ptemp_dry * zcomp_dry_inv_ob / t_ob + 2.0_wp * &
      kap2 * ptemp_wet * zcomp_wet_inv_ob / t_ob**2 + kap3 * ptemp_wet * &
      zcomp_wet_inv_ob / t_ob) /t_ob) * y_ad(i)

    y_ad(i) = 0.0_wp

    IF (x%non_ideal) THEN
      CALL ropp_fm_compress_single_ad(t_ob,tob_ad,p_ob,pob_ad,q_ob,qob_ad,&
        zcomp_dry_inv_ob_ad,zcomp_wet_inv_ob_ad)
    END IF

    pob_ad = pob_ad + ptemp_dry_ad

    ptemp_wet_ad = ptemp_wet_ad - ptemp_dry_ad

    ptemp_dry_ad = 0.0_wp

    pob_ad = pob_ad + (ptemp_wet / p_ob) * ptemp_wet_ad

    qob_ad = qob_ad + &
      (p_ob * epsilon_water / (epsilon_water + (1.0_wp - epsilon_water) * &
      q_ob)**2) * ptemp_wet_ad

    ptemp_wet_ad = 0.0_wp

    IF ( x%check_qmin ) THEN

      Qbelow_temp_ad = Qbelow_temp_ad + (q_ob / q_below_temp) * Qob_ad

      alpha_ad = alpha_ad + (-(y%geop(i) - z_geop(j)) * q_ob) * Qob_ad

      geopbelow_ad = geopbelow_ad + (alpha * q_ob) * Qob_ad

      Qob_ad = 0.0_wp

      Qbelow_temp_ad = Qbelow_temp_ad + &
        ((1.0_wp / (z_geop(j+1) - z_geop(j))) / q_below_temp) * alpha_ad

      Qabove_temp_ad = Qabove_temp_ad + &
        (-(1.0_wp / (z_geop(j+1) - z_geop(j))) / q_above_temp) * alpha_ad

      geopbelow_ad = geopbelow_ad + &
        (alpha / (z_geop(j+1) - z_geop(j))) * alpha_ad

      geopabove_ad = geopabove_ad + &
        (-alpha / (z_geop(j+1) - z_geop(j))) * alpha_ad

      alpha_ad = 0.0_wp

      IF (x%shum(j)<0.0_wp) THEN
        qbelow_temp_ad = 0.0_wp
      END IF

      IF (x%shum(j+1)<0.0_wp) THEN
        qabove_temp_ad = 0.0_wp
      END IF

    ELSE

      Qbelow_temp_ad = Qbelow_temp_ad + Qob_ad

      alpha_ad = alpha_ad + (p_ob - x%pres(j)) * Qob_ad

      pbelow_ad = pbelow_ad - alpha * Qob_ad

      pob_ad = pob_ad + alpha * Qob_ad

      Qob_ad = 0.0_wp

      Qbelow_temp_ad = Qbelow_temp_ad + &
        (-1.0_wp / (x%pres(j+1) - x%pres(j))) * alpha_ad

      Qabove_temp_ad = Qabove_temp_ad + &
        (1.0_wp / (x%pres(j+1) - x%pres(j))) * alpha_ad

      pbelow_ad = pbelow_ad + &
        (alpha / (x%pres(j+1) - x%pres(j))) * alpha_ad

      pabove_ad = pabove_ad + &
        (-alpha / (x%pres(j+1) - x%pres(j))) * alpha_ad

      alpha_ad = 0.0_wp

    ENDIF

    Qbelow_ad = qbelow_temp_ad

    Qabove_ad = qabove_temp_ad

    IF (ABS(x%temp(j+1) - x%temp(j)) < ropp_ZDTV) THEN

      IF (MIN(x%pres(j+1),x%pres(j)) <= 0.0_wp) THEN
        pgrad_ad = pgrad_ad + (y%geop(i) - z_geop(j)) * pob_ad

        pbelow_ad = pbelow_ad + (1.0_wp) * pob_ad

        geopbelow_ad = geopbelow_ad + (-pgrad) * pob_ad

        pob_ad = 0.0_wp

        pbelow_ad = pbelow_ad + (-1.0_wp / (z_geop(j+1)-z_geop(j))) * pgrad_ad

        pabove_ad = pabove_ad + ( 1.0_wp / (z_geop(j+1)-z_geop(j))) * pgrad_ad

        geopbelow_ad = geopbelow_ad + ( pgrad/(z_geop(j+1)-z_geop(j))) * pgrad_ad

        geopabove_ad = geopabove_ad + (-pgrad/(z_geop(j+1)-z_geop(j))) * pgrad_ad

        pgrad_ad = 0.0_wp
      ELSE
        Pbelow_ad = Pbelow_ad + &
          ((1.0_wp - ((y%geop(i) - z_geop(j))/(z_geop(j+1)- z_geop(j)))) * &
          EXP(LOG(x%pres(j+1)/x%pres(j))*((y%geop(i) - z_geop(j))/ &
          (z_geop(j+1)-z_geop(j))))) * pob_ad

        Pabove_ad = Pabove_ad + &
          ((((y%geop(i) - z_geop(j))/(z_geop(j+1)- z_geop(j))) * &
          (x%pres(j)/x%pres(j+1)))*EXP(LOG(x%pres(j+1)/x%pres(j))* &
          ((y%geop(i) - z_geop(j))/(z_geop(j+1)-z_geop(j))))) * pob_ad

        geopbelow_ad = geopbelow_ad + &
          (LOG(x%pres(j+1)/x%pres(j))*p_ob*(y%geop(i) - z_geop(j+1)) / &
          (z_geop(j+1)-z_geop(j))**2) * pob_ad

        geopabove_ad = geopabove_ad + &
          (LOG(x%pres(j+1)/x%pres(j))*p_ob*(z_geop(j) - y%geop(i)) / &
          (z_geop(j+1)-z_geop(j))**2) * pob_ad

        pob_ad = 0.0_wp
      END IF

    ELSE

      Pbelow_ad = Pbelow_ad + (p_ob / x%pres(j)) * pob_ad

      Tbelow_ad = Tbelow_ad + &
        (g_wmo/(r_dry*gamma) * x%pres(j)*t_ob/( x%temp(j)**2) * &
        (t_ob/x%temp(j))**(-g_wmo/(r_dry*gamma)-1.0_wp)) * pob_ad

      gamma_ad = gamma_ad + (g_wmo/(r_dry*gamma**2) * p_ob * &
        LOG(t_ob/x%temp(j))) * pob_ad

      tob_ad = tob_ad + &
        ((-g_wmo / (x%temp(j)*r_dry*gamma))*x%pres(j)*(t_ob / &
        x%temp(j))**(-g_wmo / (r_dry * gamma)-1.0_wp)) * pob_ad

      pob_ad = 0.0_wp

      Pabove_ad = Pabove_ad + ((g_wmo/r_dry)*LOG(x%temp(j+1)/x%temp(j))/ &
        (x%pres(j+1)*(LOG(x%pres(j+1)/x%pres(j)))**2)) * gamma_ad

      Pbelow_ad = Pbelow_ad + (-(g_wmo/r_dry)*LOG(x%temp(j+1)/x%temp(j))/ &
        (x%pres(j)*(LOG(x%pres(j+1)/x%pres(j)))**2)) * gamma_ad

      Tabove_ad = Tabove_ad + (-(g_wmo/r_dry)/(x%temp(j+1)* &
        LOG(x%pres(j+1)/x%pres(j)))) * gamma_ad

      Tbelow_ad = Tbelow_ad + ((g_wmo/r_dry)/(x%temp(j)* &
        LOG(x%pres(j+1)/x%pres(j)))) * gamma_ad

      gamma_ad = 0.0_wp

    END IF

    Tbelow_ad = Tbelow_ad + Tob_ad

    beta_ad = beta_ad + (y%geop(i) - z_geop(j)) * Tob_ad

    geopbelow_ad = geopbelow_ad - beta * Tob_ad

    Tob_ad = 0.0_wp

    Tbelow_ad = Tbelow_ad + (-1.0_wp / (z_geop(j+1) - z_geop(j))) * beta_ad

    Tabove_ad = Tabove_ad + (1.0_wp / (z_geop(j+1) - z_geop(j))) * beta_ad

    geopbelow_ad = geopbelow_ad + &
      (beta / (z_geop(j+1) - z_geop(j))) * beta_ad

    geopabove_ad = geopabove_ad + &
      (-beta / (z_geop(j+1) - z_geop(j))) * beta_ad

    beta_ad = 0.0_wp

! 3.9.3 Adjoint variables to return

    x_ad%temp(j) = x_ad%temp(j) + Tbelow_ad
    x_ad%temp(j+1) = x_ad%temp(j+1) + Tabove_ad
    x_ad%pres(j) = x_ad%pres(j) + Pbelow_ad
    x_ad%pres(j+1) = x_ad%pres(j+1) + Pabove_ad
    x_ad%shum(j) = x_ad%shum(j) + Qbelow_ad
    x_ad%shum(j+1) = x_ad%shum(j+1) + Qabove_ad
    z_geop_ad(j) = z_geop_ad(j) + geopbelow_ad
    z_geop_ad(j+1) = z_geop_ad(j+1) + geopabove_ad

  END DO

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


END SUBROUTINE ropp_fm_refrac_1d_new_ad
