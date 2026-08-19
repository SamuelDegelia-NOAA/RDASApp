! $Id: ropp_fm_iono_bangle_above_leo_ad.f90 5416 2017-11-29 16:12:58 sti $

!****s* Ionosphere/ropp_fm_iono_bangle_above_leo_ad *
!
! NAME
!   ropp_fm_iono_bangle_above_leo_ad - AD of forward model of ionospheric bending above LEO.
!
! SYNOPSIS
!   CALL ropp_fm_iono_bangle_above_leo_ad(Ne_max,r_max,h, r_leo, impact, bangle_above_leo_ad)
!
! DESCRIPTION
!   Estimate the AD of the ionospheric bending above the LEO assuming a Chapman layer.
!
! INPUTS
!  REAl(wp), INTENT(in)     :: Ne_max,r_max,h          ! Chapman params
!  REAl(wp), INTENT(in)     :: Ne_max_ad,r_max_ad,h_ad ! AD Chapman params
!  REAL(wp), INTENT(in)     :: r_leo                   ! Radius of LEO 
!  REAL(wp), INTENT(in)     :: impact(:)               ! Impact parameter
!
! INOUTPUTS
!   REAL(wp), INTENT(inout) :: bangle_above_leo_ad     ! AD bending angle above LEO
!
! NOTES
!  Bending calculated from (currently) the first 3 terms of the 'small gamma'
!  expansion, Eqn 4.18 of RSR 33.
!
! SEE ALSO
!   SAF/ROM/METO/REP/RSR/033 at http://www.romsaf.org/rsr.php.
!
! AUTHOR
!   Met Office, Exeter, UK and ECMWF, Reading UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

SUBROUTINE ropp_fm_iono_bangle_above_leo_ad(Ne_max, r_max, h, &
                                            Ne_max_ad, r_max_ad, h_ad, &
                                            r_leo, impact, bangle_above_leo_ad)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_iono_bangle_above_leo_ad

  IMPLICIT NONE

  REAL(wp), INTENT(in)    :: Ne_max,r_max,h          ! Chapman params
  REAL(wp), INTENT(inout) :: Ne_max_ad,r_max_ad,h_ad ! AD Chapman params
  REAL(wp), INTENT(in)    :: r_leo                   ! Radius of LEO
  REAL(wp), INTENT(in)    :: impact(:)               ! Impact parameter
  REAL(wp), INTENT(inout) :: bangle_above_leo_ad(:)  ! AD bending angle above LEO

! local

  INTEGER                 :: nobs                    ! Size of arrays

! for sum and error function

  REAL(wp)                :: gamma,gamma_over_two,gamma_power
  REAL(wp)                :: gamma_ad,gamma_over_two_ad,gamma_power_ad
  REAL(wp), ALLOCATABLE   :: cval(:),cval_ad(:)
  REAL(wp), ALLOCATABLE   :: bangle_above_leo(:)

! for error function

  REAL(wp), PARAMETER     :: a=0.3480242_wp
  REAL(wp), PARAMETER     :: b=0.0958798_wp
  REAL(wp), PARAMETER     :: c=0.7478556_wp

  INTEGER, PARAMETER      :: nterm = 3  ! guess

  INTEGER                 :: i, j

  REAL(wp)                :: anum,afac,aneg,epi
  REAL(wp)                :: series_term,series_sum
  REAL(wp)                :: series_term_ad,series_sum_ad
  REAL(wp)                :: erfcx_term,zt,t_val
  REAL(wp)                :: erfcx_term_ad,zt_ad,t_val_ad

!-------------------------------------------------------------------------------
! 2. Initialise adjoint variables
!-------------------------------------------------------------------------------

  epi = 4.0_wp * EXP(1.0_wp) * ATAN(1.0_wp)

  nobs = SIZE(impact)
  ALLOCATE (cval(nobs), cval_ad(nobs), bangle_above_leo(nobs))

  erfcx_term_ad = 0.0_wp
  zt_ad       = 0.0_wp
  t_val_ad    = 0.0_wp

  series_term_ad = 0.0_wp
  series_sum_ad  = 0.0_wp

  gamma_ad = 0.0_wp
  gamma_over_two_ad = 0.0_wp
  gamma_power_ad = 0.0_wp

  cval_ad(:) = 0.0_wp

!-------------------------------------------------------------------------------
! 3. Calculate bending angle
!-------------------------------------------------------------------------------

  gamma = EXP( (r_max-r_leo) / h )    ! (should be less than 1)

  gamma_over_two = 0.5_wp*gamma

  cval(:) = (r_leo - impact(:)) / h

  bangle_above_leo(:) = 0.0_wp

  DO i = 1,nobs

    anum =  0.0_wp

    afac =  1.0_wp

    aneg = -1.0_wp

    gamma_power = 1.0_wp

    series_sum = 0.0_wp

    DO j = 1, nterm  ! three terms at the moment.

      t_val =  SQRT((anum+0.5_wp)*cval(i))

      zt = 1.0_wp / (1.0_wp + 0.47047_wp*t_val)

      erfcx_term = (a-(b-c*zt)*zt) * zt  ! exp(x*x)*erfc(x)

      series_term = 2.0_wp*gamma_power*aneg/afac*SQRT(anum+0.5_wp)*erfcx_term

      series_sum  = series_sum + series_term

! update parameters for next term

      gamma_power = gamma_power*gamma_over_two

      aneg = - aneg

      anum = anum + 1.0_wp

      afac = afac * anum

    END DO

! factor 0.5, because we are are only computing bending for one side.

    bangle_above_leo(i) = 0.5_wp * impact(i) * ne_max * &
                          SQRT(epi*gamma/(h*(r_max+impact(i)))) * series_sum

!-------------------------------------------------------------------------------
! 4. Calculate adjoint part
!-------------------------------------------------------------------------------

    r_max_ad = r_max_ad - 0.5_wp*bangle_above_leo(i)* bangle_above_leo_ad(i)/(r_max+impact(i))
    h_ad     = h_ad     - 0.5_wp*bangle_above_leo(i)* bangle_above_leo_ad(i)/h
    gamma_ad = gamma_ad + 0.5_wp*bangle_above_leo(i)* bangle_above_leo_ad(i)/gamma

    series_sum_ad = series_sum_ad + bangle_above_leo(i)* bangle_above_leo_ad(i)/series_sum
    Ne_max_ad = Ne_max_ad + bangle_above_leo(i)* bangle_above_leo_ad(i)/Ne_max

    bangle_above_leo_ad(i) = 0.0_wp

    aneg = - aneg

    afac = afac/anum

    anum = anum - 1.0

    gamma_power = gamma_power/gamma_over_two

    DO j = nterm, 1, -1  ! three terms at the moment.

      IF (j == 1) THEN

        gamma_power = 1.0_wp

        gamma_power_ad = 0.0_wp

      ELSE

        gamma_power = gamma_over_two**(j-1)

        gamma_over_two_ad = gamma_over_two_ad + REAL(j-1)*(gamma_over_two**j)*gamma_power_ad

      END IF

      t_val =  SQRT((anum+0.5_wp)*cval(i))

      zt = 1.0_wp / (1.0_wp + 0.47047_wp*t_val)

      erfcx_term = (a-(b-c*zt)*zt) * zt  ! exp(x*x)*erfc(x)

! update parameters for next term

      series_term_ad = series_term_ad + series_sum_ad
      series_sum_ad  = series_sum_ad 

      gamma_power_ad = gamma_power_ad + 2.0_wp*aneg/afac*SQRT(anum+0.5_wp)*erfcx_term*series_term_ad
      erfcx_term_ad    = erfcx_term_ad +  2.0_wp*gamma_power*aneg/afac*SQRT(anum+0.5_wp)*series_term_ad
      series_term_ad = 0.0_wp

      zt_ad = zt_ad + (a-(2.0_wp*b-3.0_wp*c*zt)*zt)*erfcx_term_ad
      erfcx_term_ad = 0.0_wp

      t_val_ad = t_val_ad - 0.47047_wp*zt_ad/ (1.0_wp + 0.47047_wp*t_val)**2 
      zt_ad = 0.0_wp

      cval_ad(i) = cval_ad(i) + 0.5_wp*SQRT((anum+0.5_wp)/cval(i))*t_val_ad
      t_val_ad = 0.0_wp

      IF (j == 1) THEN

        gamma_power = 1.0_wp

        gamma_power_ad = 0.0_wp

      ELSE

        gamma_power = gamma_over_two**(j-1)

        gamma_over_two_ad = gamma_over_two_ad + REAL(j-1)*(gamma_over_two**(j-2))*gamma_power_ad

        gamma_power_ad = 0.0_wp

      END IF

      aneg = - aneg

      afac = afac / anum

      anum = anum - 1.0_wp

    END DO

    series_sum_ad = 0.0_wp

  END DO

  bangle_above_leo_ad(:) = 0.0_wp

  DO i = 1, nobs

    h_ad = h_ad - (r_leo - impact(i))*cval_ad(i)/h**2
    cval_ad(i) = 0.0_wp

  END DO

  gamma_ad = gamma_ad + 0.5_wp*gamma_over_two_ad

  gamma_over_two_ad = 0.0_wp

  gamma = EXP((r_max-r_leo)/h)    ! (should be less than 1)

  r_max_ad = r_max_ad + (gamma/h)*gamma_ad

  h_ad = h_ad - (gamma*(r_max-r_leo)/h**2)*gamma_ad

  gamma_ad = 0.0_wp

!-------------------------------------------------------------------------------
! 5. Clean up
!-------------------------------------------------------------------------------

  DEALLOCATE (bangle_above_leo, cval_ad, cval)

  RETURN

END SUBROUTINE ropp_fm_iono_bangle_above_leo_ad
