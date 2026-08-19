! $Id: ropp_fm_iono_bangle_above_leo.f90 5412 2017-11-24 17:36:06 sti $

!****s* Ionosphere/ropp_fm_iono_bangle_above_leo *
!
! NAME
!   ropp_fm_iono_bangle_above_leo - Forward model of ionospheric bending above LEO.
!
! SYNOPSIS
!   CALL ropp_fm_iono_bangle_above_leo(Ne_max, r_max, h, r_leo, impact, bangle_above_leo)
!
! DESCRIPTION
!   Estimate the ionospheric bending above the LEO assuming a Chapman layer.
!
! INPUTS
!  REAL(wp), INTENT(in)  :: Ne_max,r_max,h      ! Chapman params
!  REAL(wp), INTENT(in)  :: r_leo               ! Radius of LEO
!  REAL(wp), INTENT(in)  :: impact(:)           ! Impact parameter
!
! OUTPUTS
!  REAL(wp), INTENT(out) :: bangle_above_leo(:) ! Bending angle above LEO
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

SUBROUTINE ropp_fm_iono_bangle_above_leo(Ne_max, r_max, h, r_leo, impact, bangle_above_leo)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_iono_bangle_above_leo

  IMPLICIT NONE

  REAl(wp), INTENT(in)  :: Ne_max,r_max,h       ! Chapman params
  REAL(wp), INTENT(in)  :: r_leo                ! Radius of LEO
  REAL(wp), INTENT(in)  :: impact(:)            ! Impact parameter
  REAL(wp), INTENT(out) :: bangle_above_leo(:)  ! Updated bending angles

! local

  INTEGER               :: nobs                 ! Size of arrays

! for sum and error function

  REAL(wp)              :: gamma,gamma_over_two,gamma_power
  REAL(wp), ALLOCATABLE :: cval(:)

! for error function

  REAL(wp), PARAMETER   :: a=0.3480242_wp
  REAL(wp), PARAMETER   :: b=0.0958798_wp
  REAL(wp), PARAMETER   :: c=0.7478556_wp

  INTEGER, PARAMETER    :: nterm = 3  ! guess

  INTEGER               :: i, j

  REAL(wp)              :: anum,afac,aneg,epi
  REAL(wp)              :: series_term,series_sum
  REAL(wp)              :: erfcx_term,zt,t_val

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  epi = 4.0_wp * EXP(1.0_wp) * ATAN(1.0_wp)

  nobs = SIZE(impact)
  ALLOCATE (cval(nobs))

!-------------------------------------------------------------------------------
! 3. Calculate bending angle
!-------------------------------------------------------------------------------

  gamma = EXP((r_max-r_leo)/h)    ! (should be less than 1)

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
                          SQRT( epi*gamma / ( h*(r_max+impact(i)) ) ) * &
                          series_sum

  END DO

!-------------------------------------------------------------------------------
! 4. Clean up
!-------------------------------------------------------------------------------

  DEALLOCATE (cval)

  RETURN

END SUBROUTINE ropp_fm_iono_bangle_above_leo
