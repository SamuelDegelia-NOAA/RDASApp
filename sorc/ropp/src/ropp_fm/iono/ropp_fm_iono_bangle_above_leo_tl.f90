! $Id: ropp_fm_iono_bangle_above_leo_tl.f90 5469 2018-02-12 11:29:56 sti $

!****s* Ionosphere/ropp_fm_iono_bangle_above_leo_tl *
!
! NAME
!   ropp_fm_iono_bangle_above_leo_tl - TL of forward model of ionospheric bending above LEO.
!
! SYNOPSIS
!   CALL ropp_fm_iono_bangle_above_leo_tl(Ne_max,r_max,h, r_leo, impact, bangle_above_leo)
!
! DESCRIPTION
!   Estimate the TL of the ionospheric bending above the LEO assuming a Chapman layer.
!
! INPUTS
!  REAL(wp), INTENT(in)   :: Ne_max,r_max,h           ! Chapman params
!  REAL(wp), INTENT(in)   :: Ne_max_tl,r_max_tl,h_tl  ! TL Chapman params
!  REAL(wp), INTENT(in)   :: r_leo                    ! Radius of LEO
!  REAL(wp), INTENT(in)   :: impact(:)                ! Impact parameter
!
! OUTPUTS
!   REAL(wp), INTENT(out) :: bangle_above_leo_tl(:)   ! TL bending angle above LEO
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

SUBROUTINE ropp_fm_iono_bangle_above_leo_tl(Ne_max, r_max, h, &
                                            Ne_max_tl, r_max_tl, h_tl, &
                                            r_leo, impact, bangle_above_leo_tl)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_iono_bangle_above_leo_tl

  IMPLICIT NONE

  REAL(wp), INTENT(in)  :: Ne_max,r_max,h          ! Chapman params
  REAL(wp), INTENT(in)  :: Ne_max_tl,r_max_tl,h_tl ! TL Chapman params
  REAL(wp), INTENT(in)  :: r_leo                   ! Radius of LEO
  REAL(wp), INTENT(in)  :: impact(:)               ! Impact parameter
  REAL(wp), INTENT(out) :: bangle_above_leo_tl(:)  ! TL bending angle above LEO

! local

  INTEGER               :: nobs                    ! Size of arrays

! for sum and error function

  REAL(wp)              :: gamma,gamma_over_two,gamma_power
  REAL(wp)              :: gamma_tl,gamma_over_two_tl,gamma_power_tl
  REAL(wp), ALLOCATABLE :: cval(:),cval_tl(:)
  REAL(wp), ALLOCATABLE :: bangle_above_leo(:)

! for error function

  REAL(wp), PARAMETER   :: a=0.3480242_wp
  REAL(wp), PARAMETER   :: b=0.0958798_wp
  REAL(wp), PARAMETER   :: c=0.7478556_wp

  INTEGER, PARAMETER    :: nterm = 3  ! guess

  INTEGER               :: i, j

  REAL(wp)              :: anum,afac,aneg,epi
  REAL(wp)              :: series_term,series_sum
  REAL(wp)              :: series_term_tl,series_sum_tl
  REAL(wp)              :: erfcx_term,zt,t_val
  REAL(wp)              :: erfcx_term_tl,zt_tl,t_val_tl

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  epi = 4.0_wp * EXP(1.0_wp) * ATAN(1.0_wp)

  nobs = SIZE(impact)
  ALLOCATE (cval(nobs), cval_tl(nobs), bangle_above_leo(nobs))

!-------------------------------------------------------------------------------
! 3. Calculate bending angle and its TL
!-------------------------------------------------------------------------------

  gamma = EXP( (r_max-r_leo) / h )    ! (should be less than 1)

  gamma_tl = gamma*(r_max_tl/h - (r_max-r_leo)*h_tl/h**2)

  gamma_over_two = 0.5_wp*gamma

  gamma_over_two_tl =  0.5_wp*gamma_tl

  cval(:) = (r_leo - impact(:)) / h

  cval_tl(:) = -(r_leo - impact(:)) * h_tl / h**2

  bangle_above_leo(:)    = 0.0_wp

  bangle_above_leo_tl(:) = 0.0_wp

  DO i = 1,nobs

    anum =  0.0_wp

    afac =  1.0_wp

    aneg = -1.0_wp

    series_sum = 0.0_wp

    series_sum_tl = 0.0_wp

    DO j = 1, nterm  ! three terms at the moment.

      IF (j == 1) THEN

        gamma_power = 1.0_wp

        gamma_power_tl = 0.0_wp

      ELSE

        gamma_power = gamma_over_two**(j-1)

        gamma_power_tl = REAL(j-1)*(gamma_over_two**(j-2))*gamma_over_two_tl

      END IF

      t_val =  SQRT((anum+0.5_wp)*cval(i))

      t_val_tl = 0.5_wp*SQRT((anum+0.5_wp)/cval(i))*cval_tl(i)

      zt = 1.0_wp / (1.0_wp + 0.47047_wp*t_val)

      zt_tl = -0.47047_wp*t_val_tl/ (1.0_wp + 0.47047_wp*t_val)**2

      erfcx_term = (a-(b-c*zt)*zt) * zt  ! exp(x*x)*erfc(x)

      erfcx_term_tl = (a-(2.0_wp*b-3.0_wp*c*zt)*zt)*zt_tl ! delta exp(x*x)*erfc(x)

      series_term = 2.0_wp*gamma_power*aneg/afac*SQRT(anum+0.5_wp)*erfcx_term

      series_term_tl = 2.0_wp*gamma_power_tl*aneg/afac*SQRT(anum+0.5_wp)*erfcx_term    + &
                       2.0_wp*gamma_power   *aneg/afac*SQRT(anum+0.5_wp)*erfcx_term_tl

      series_sum  = series_sum + series_term

      series_sum_tl  = series_sum_tl + series_term_tl

! update parameters for next term

      aneg = - aneg

      anum = anum + 1.0_wp

      afac = afac * anum

    END DO

! factor 0.5, because we are are only computing bending for one side.

    bangle_above_leo(i)    = 0.5_wp * impact(i) * ne_max * &
                             SQRT(epi*gamma/(h*(r_max+impact(i)))) * series_sum

    bangle_above_leo_tl(i) = bangle_above_leo(i) * &
                             ( ne_max_tl/ne_max + &
                               series_sum_tl/series_sum + &
                               0.5_wp * &
                               (gamma_tl/gamma - h_tl/h - r_max_tl/(r_max+impact(i))) )

  END DO

!-------------------------------------------------------------------------------
! 4. Clean up
!-------------------------------------------------------------------------------

  DEALLOCATE (bangle_above_leo, cval_tl, cval)

  RETURN

END SUBROUTINE ropp_fm_iono_bangle_above_leo_tl
