! $Id: ropp_fm_iono_bangle_ad.f90 4010 2014-01-10 11:07:40Z idculv $

!****s* Ionosphere/ropp_fm_iono_bangle_ad *
!
! NAME
!   ropp_fm_iono_bangle_ad - Adjoint of forward model bending caused by ionosphere.
!
! SYNOPSIS
!   CALL ropp_fm_iono_bangle_ad( &
!                               Ne_max,    R_peak,    H_width, R_leo, &
!                               Ne_max_ad, R_peak_ad, H_width_ad, &
!                               n_L1, impact, bangle, bangle_ad)
!
! DESCRIPTION
!   AD of forward model of ionospheric bending for the L1 and L2 bending angles
!   assuming Chapman layer profiles. Should only be called if
!   x%direct_ion=.TRUE.
!
! INPUTS
!   REAL(wp)               :: Ne_max     ! Peak electron density (m-3)
!   REAL(wp)               :: R_peak     ! Radius of peak value (m)
!   REAL(wp)               :: H_width    ! Width of Chapman Layer (m)
!   REAL(wp)               :: R_leo      ! Radius of LEO satellite (m)
!   INTEGER,               :: n_L1       ! No. of L1 bending angles in array
!   REAL(wp), DIMENSION(:) :: impact     ! Impact parameters
!   REAL(wp), DIMENSION(:) :: bangle     ! Updated bending angles inc. ionospheric bending
!
! INOUTPUTS
!   REAL(wp)               :: Ne_max_ad   ! AD Peak electron density (m-3)
!   REAL(wp)               :: R_peak_ad   ! AD Radius of peak value (m)
!   REAL(wp)               :: H_width_ad  ! AD Width of Chapman Layer (m)
!   REAL(wp), DIMENSION(:) :: bangle_ad   ! AD total bending angle
!
! NOTES
!  Correction applied to bending angles to match derived bending angles.
!  See Eqn 4.4 of RSR 33.
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

SUBROUTINE ropp_fm_iono_bangle_ad( &
                                  Ne_max,    R_peak,    H_width, R_leo, &
                                  Ne_max_ad, R_peak_ad, H_width_ad, &
                                  n_L1, impact, bangle, bangle_ad)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm,   not_this => ropp_fm_iono_bangle_ad
  USE ropp_utils, ONLY: ropp_MDTV
  USE ropp_fm_constants, ONLY: k4, f_L1,f_L2

  IMPLICIT NONE

  REAL(wp), INTENT(in)                  :: Ne_max      ! Peak electron density (m-3)
  REAL(wp), INTENT(in)                  :: R_peak      ! Radius of peak value (m)
  REAL(wp), INTENT(in)                  :: H_width     ! Width of Chapman Layer (m)
  REAL(wp), INTENT(in)                  :: R_leo       ! Radius of LEO satellite (m)

  REAL(wp), INTENT(inout)               :: Ne_max_ad   ! Grad peak electron density (m-3)
  REAL(wp), INTENT(inout)               :: R_peak_ad   ! Grad radius of peak value (m)
  REAL(wp), INTENT(inout)               :: H_width_ad  ! Grad width of Chapman Layer (m)
  INTEGER                               :: n_L1        ! Number of L1 bending angles
  REAL(wp), DIMENSION(:), INTENT(in)    :: impact      ! Impact parameters
  REAL(wp), DIMENSION(:), INTENT(in)    :: bangle      ! Updated bending angles
  REAL(wp), DIMENSION(:), INTENT(inout) :: bangle_ad   ! AD total bending angles

  INTEGER                               :: n_chap, n_impact, i
  REAL(wp)                              :: const_L1, const_L2

  REAL(wp)                              :: Ne_at_leo,uval
  REAL(wp)                              :: Ne_at_leo_ad,uval_ad

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: lg,    z,    q,   &
                                           lg_ad, z_ad, q_ad, &
                                           bangle_ion_ad, bangle_leo_ad

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  n_chap = 1  ! For now
  n_impact = SIZE(impact)

  ALLOCATE(   lg(n_impact),    z(n_impact),    q(n_impact))
  ALLOCATE(lg_ad(n_impact), z_ad(n_impact), q_ad(n_impact))

  ALLOCATE(bangle_ion_ad(n_impact), bangle_leo_ad(n_impact))

!-------------------------------------------------------------------------------
! 3. Initialise local adjoint variables
!--------------------------------------------------------------------------------

  lg_ad(:)         = 0.0_wp
  z_ad(:)          = 0.0_wp
  bangle_ion_ad(:) = 0.0_wp
  bangle_leo_ad(:) = 0.0_wp
  q_ad(:)          = 0.0_wp
  uval_ad          = 0.0_wp
  Ne_at_leo_ad     = 0.0_wp

!--------------------------------------------------------------------------------
! 4. Total bending = (neutral + ionospheric)
!--------------------------------------------------------------------------------  

! 4.1 terms associated with refractive index at LEO

  const_L1 = k4 / f_L1**2
  const_L2 = k4 / f_L2**2

  uval = (R_leo - R_peak) / H_width    ! convenient variable for Chapman

  Ne_at_leo = Ne_max * EXP( 0.5_wp * (1.0_wp-uval-EXP(-uval)) )

  DO i = n_impact, 1, -1  ! order should not matter

    IF (bangle(i) < ropp_MDTV) CYCLE ! neutral is missing

    IF (i <= n_L1) THEN ! the L1 bending angles

      Ne_at_leo_ad = Ne_at_leo_ad - &
                     bangle_ad(i) * const_L1 * impact(i) / &
                     SQRT(R_leo**2-impact(i)**2)

      bangle_leo_ad(i) = bangle_leo_ad(i) - const_L1 * bangle_ad(i)

      bangle_ion_ad(i) = bangle_ion_ad(i) + const_L1 * bangle_ad(i)

    ELSE ! the L2 bending angles

      Ne_at_leo_ad = Ne_at_leo_ad - &
                     bangle_ad(i) * const_L2 * impact(i) / &
                     SQRT(R_leo**2-impact(i)**2)

      bangle_leo_ad(i) = bangle_leo_ad(i) - const_L2 * bangle_ad(i)

      bangle_ion_ad(i) = bangle_ion_ad(i) + const_L2 * bangle_ad(i)

    END IF

  END DO

! 4.2 terms associated with bending above LEO

  CALL ropp_fm_iono_bangle_above_leo_ad(Ne_max,   R_peak,   H_width,&
                                        Ne_max_ad,R_peak_ad,H_width_ad, &
                                        R_leo,impact,bangle_leo_ad)

!--------------------------------------------------------------------------------
! 5. Adjoint of electron density at the LEO using the Chapman layer
!--------------------------------------------------------------------------------  

  Ne_max_ad = Ne_max_ad + Ne_at_leo_ad * EXP( 0.5_wp*(1.0_wp-uval-EXP(-uval)) )

  uval_ad = uval_ad + 0.5_wp * Ne_at_leo * (EXP(-uval)-1.0_wp) * Ne_at_leo_ad

  Ne_at_leo_ad = 0.0_wp

  R_peak_ad = R_peak_ad - uval_ad/H_width

  H_width_ad = H_width_ad - (uval/H_width)*uval_ad

  uval_ad = 0.0_wp

!--------------------------------------------------------------------------------
! 6. Adjoint of bending with zorro function
!--------------------------------------------------------------------------------

! State (needed for adjoint)

  lg = (R_peak - impact) / H_width   ! what if impact missing?

  z = ropp_fm_zorro(lg)

  q = 2.0_wp * EXP(0.5_wp) * impact * R_peak / &
      SQRT(H_width*(R_peak + impact)**3)

! Adjoint of bangle_ion_tl

  Ne_max_ad = Ne_max_ad + DOT_PRODUCT(q*z , bangle_ion_ad)

  q_ad = q_ad + Ne_max*z*bangle_ion_ad

  z_ad = z_ad + Ne_max*q*bangle_ion_ad

! Adjoint of q_tl

  H_width_ad = H_width_ad - 0.5_wp*DOT_PRODUCT((q/H_width), q_ad)

  R_peak_ad = R_peak_ad + &
              DOT_PRODUCT(q*(impact-0.5_wp*R_peak)/R_peak/(R_peak+impact), q_ad)

  q_ad = 0.0_wp

! Adjoint of z_tl

  lg_ad = lg_ad + ropp_fm_dzorro_dlg(lg)*z_ad

  z_ad = 0.0_wp

! Adjoint of lg_tl

  R_peak_ad = R_peak_ad + SUM(lg_ad)/H_width

  H_width_ad = H_width_ad - DOT_PRODUCT(lg/H_width, lg_ad)

  lg_ad = 0.0_wp

  bangle_ion_ad = 0.0_wp

  bangle_leo_ad = 0.0_wp

!--------------------------------------------------------------------------------
! 6. Deallocate/tidy up
!--------------------------------------------------------------------------------  

  DEALLOCATE(lg, z, q)
  DEALLOCATE(lg_ad, z_ad, q_ad)
  DEALLOCATE(bangle_ion_ad, bangle_leo_ad)

  RETURN

END SUBROUTINE ropp_fm_iono_bangle_ad
