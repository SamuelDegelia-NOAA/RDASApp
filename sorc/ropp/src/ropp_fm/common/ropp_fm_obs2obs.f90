! $Id$

!****s* Copying/ropp_fm_obs2obs *
!
! NAME
!    ropp_fm_obs2obs - Copy observation vectors.
!
! SYNOPSIS
!    USE ropp_fm
!       ...
!    new_obs = old_obs
!
! DESCRIPTION
!    This subroutines are used for overloading the assign operator (=)
!    for observation vectors.
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
! 1. 1D Bending angles
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_obs2obs_1dbangle(from_obs, to_obs)

! 1.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(Obs1dBangle),    INTENT(in)    :: from_obs
  TYPE(Obs1dBangle),    INTENT(inout) :: to_obs

! 1.2 Copy contents
! -----------------

  to_obs % lon        = from_obs % lon
  to_obs % lat        = from_obs % lat
  to_obs % time       = from_obs % time
  to_obs % g_sfc      = from_obs % g_sfc
  to_obs % r_earth    = from_obs % r_earth
  to_obs % r_curve    = from_obs % r_curve
  to_obs % r_gns      = from_obs % r_gns
  to_obs % r_leo      = from_obs % r_leo
  to_obs % f1         = from_obs % f1
  to_obs % f2         = from_obs % f2
  to_obs % undulation = from_obs % undulation
  to_obs % azimuth    = from_obs % azimuth
  to_obs % nobs       = from_obs % nobs
  to_obs % n_L1       = from_obs % n_L1

  CALL copy_alloc(from_obs % bangle,  to_obs % bangle)
  CALL copy_alloc(from_obs % impact,  to_obs % impact)
  CALL copy_alloc(from_obs % weights, to_obs % weights)
   IF (ASSOCIATED(from_obs%rtan)) &
      CALL copy_alloc(from_obs % rtan,    to_obs % rtan)
   IF (ASSOCIATED(from_obs%a_path)) &
      CALL copy_alloc(from_obs % a_path,  to_obs % a_path)

  to_obs % obs_ok      = from_obs % obs_ok
  to_obs % cov_ok      = from_obs % cov_ok
  to_obs % diff_bangle = from_obs % diff_bangle

! 1.3 Copy covariance (manually)
! ------------------------------

  CALL copy_alloc(from_obs % cov % d, to_obs % cov % d)

  IF(ASSOCIATED(from_obs%cov%e))THEN
    CALL copy_alloc(from_obs % cov % e, to_obs % cov % e)
  ELSE
    IF (ASSOCIATED(to_obs % cov % e)) DEALLOCATE(to_obs % cov % e)
  END IF

  IF(ASSOCIATED(from_obs%cov%f))THEN
    CALL copy_alloc(from_obs % cov % f, to_obs % cov % f)
  ELSE
    IF (ASSOCIATED(to_obs % cov % f)) DEALLOCATE(to_obs % cov % f)
  END IF

  IF(ASSOCIATED(from_obs%cov%s))THEN
    CALL copy_alloc(from_obs % cov % s, to_obs % cov % s)
  ELSE
    IF (ASSOCIATED(to_obs % cov % s)) DEALLOCATE(to_obs % cov % s)
  END IF

  to_obs % cov % fact_chol = from_obs % cov % fact_chol
  to_obs % cov % equi_chol = from_obs % cov % equi_chol

END SUBROUTINE ropp_fm_obs2obs_1dbangle


!-------------------------------------------------------------------------------
! 2. 1D Bending angles (assignment)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_assign_obs_1dbangle(to_obs, from_obs)

! 2.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(Obs1dBangle),    INTENT(inout) :: to_obs
  TYPE(Obs1dBangle),    INTENT(in)    :: from_obs

! 2.2 Copy contents
! -----------------

  to_obs % lon          = from_obs % lon
  to_obs % lat          = from_obs % lat
  to_obs % time         = from_obs % time
  to_obs % g_sfc        = from_obs % g_sfc
  to_obs % r_earth      = from_obs % r_earth
  to_obs % r_curve      = from_obs % r_curve
  to_obs % r_gns        = from_obs % r_gns
  to_obs % r_leo        = from_obs % r_leo
  to_obs % f1           = from_obs % f1
  to_obs % f2           = from_obs % f2
  to_obs % undulation   = from_obs % undulation
  to_obs % azimuth      = from_obs % azimuth
  to_obs % nobs         = from_obs % nobs
  to_obs % n_L1         = from_obs % n_L1

  CALL copy_alloc(from_obs % bangle,  to_obs % bangle)
  CALL copy_alloc(from_obs % impact,  to_obs % impact)
  CALL copy_alloc(from_obs % weights, to_obs % weights)
  IF (ASSOCIATED(from_obs%rtan)) &
    CALL copy_alloc(from_obs % rtan,    to_obs % rtan)
  IF (ASSOCIATED(from_obs%a_path)) &
    CALL copy_alloc(from_obs % a_path,  to_obs % a_path)

  to_obs % obs_ok      = from_obs % obs_ok
  to_obs % cov_ok      = from_obs % cov_ok
  to_obs % diff_bangle = from_obs % diff_bangle

! 2.3 Copy covariance (manually)
! ------------------------------

  CALL copy_alloc(from_obs % cov % d, to_obs % cov % d)

  IF(ASSOCIATED(from_obs%cov%e))THEN
    CALL copy_alloc(from_obs % cov % e, to_obs % cov % e)
  ELSE
    IF (ASSOCIATED(to_obs % cov % e)) DEALLOCATE(to_obs % cov % e)
  END IF

  IF(ASSOCIATED(from_obs%cov%f))THEN
    CALL copy_alloc(from_obs % cov % f, to_obs % cov % f)
  ELSE
    IF (ASSOCIATED(to_obs % cov % f)) DEALLOCATE(to_obs % cov % f)
  END IF

  IF(ASSOCIATED(from_obs%cov%s))THEN
    CALL copy_alloc(from_obs % cov % s, to_obs % cov % s)
  ELSE
    IF (ASSOCIATED(to_obs % cov % s)) DEALLOCATE(to_obs % cov % s)
  END IF

  to_obs % cov % fact_chol = from_obs % cov % fact_chol
  to_obs % cov % equi_chol = from_obs % cov % equi_chol

END SUBROUTINE ropp_fm_assign_obs_1dbangle


!-------------------------------------------------------------------------------
! 3. 1D Refractivity
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_obs2obs_1drefrac(from_obs, to_obs)

! 3.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(Obs1dRefrac),    INTENT(in)    :: from_obs
  TYPE(Obs1dRefrac),    INTENT(inout) :: to_obs

! 3.3 Copy contents
! -----------------

  to_obs % lon = from_obs % lon
  to_obs % lat = from_obs % lat
  to_obs % time = from_obs % time

  CALL copy_alloc(from_obs % refrac,  to_obs % refrac)
  CALL copy_alloc(from_obs % geop,    to_obs % geop)
  CALL copy_alloc(from_obs % weights, to_obs % weights)

  to_obs % obs_ok = from_obs % obs_ok
  to_obs % cov_ok = from_obs % cov_ok

! 3.3 Copy covariance (manually)
! ------------------------------

  CALL copy_alloc(from_obs % cov % d, to_obs % cov % d)

  IF(ASSOCIATED(from_obs%cov%e))THEN
    CALL copy_alloc(from_obs % cov % e, to_obs % cov % e)
  ELSE
    IF (ASSOCIATED(to_obs % cov % e)) DEALLOCATE(to_obs % cov % e)
  END IF

  IF(ASSOCIATED(from_obs%cov%f))THEN
    CALL copy_alloc(from_obs % cov % f, to_obs % cov % f)
  ELSE
    IF (ASSOCIATED(to_obs % cov % f)) DEALLOCATE(to_obs % cov % f)
  END IF

  IF(ASSOCIATED(from_obs%cov%s))THEN
    CALL copy_alloc(from_obs % cov % s, to_obs % cov % s)
  ELSE
    IF (ASSOCIATED(to_obs % cov % s)) DEALLOCATE(to_obs % cov % s)
  END IF

  to_obs % cov % fact_chol = from_obs % cov % fact_chol
  to_obs % cov % equi_chol = from_obs % cov % equi_chol

END SUBROUTINE ropp_fm_obs2obs_1drefrac


!-------------------------------------------------------------------------------
! 4. 1D Refractivity (assigment)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_assign_obs_1drefrac(to_obs, from_obs)

! 4.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(Obs1dRefrac),    INTENT(inout) :: to_obs
  TYPE(Obs1dRefrac),    INTENT(in)    :: from_obs

! 4.3 Copy contents
! -----------------

  to_obs % lon = from_obs % lon
  to_obs % lat = from_obs % lat
  to_obs % time = from_obs % time

  CALL copy_alloc(from_obs % refrac,  to_obs % refrac)
  CALL copy_alloc(from_obs % geop,    to_obs % geop)
  CALL copy_alloc(from_obs % weights, to_obs % weights)

  to_obs % obs_ok = from_obs % obs_ok
  to_obs % cov_ok = from_obs % cov_ok

! 4.3 Copy covariance (manually)
! ------------------------------

  CALL copy_alloc(from_obs % cov % d, to_obs % cov % d)
    
  IF(ASSOCIATED(from_obs%cov%e))THEN
    CALL copy_alloc(from_obs % cov % e, to_obs % cov % e)
  ELSE
    IF (ASSOCIATED(to_obs % cov % e)) DEALLOCATE(to_obs % cov % e)
  END IF

  IF(ASSOCIATED(from_obs%cov%f))THEN
    CALL copy_alloc(from_obs % cov % f, to_obs % cov % f)
  ELSE
    IF (ASSOCIATED(to_obs % cov % f)) DEALLOCATE(to_obs % cov % f)
  END IF

  IF(ASSOCIATED(from_obs%cov%s))THEN
    CALL copy_alloc(from_obs % cov % s, to_obs % cov % s)
  ELSE
    IF (ASSOCIATED(to_obs % cov % s)) DEALLOCATE(to_obs % cov % s)
  END IF

  to_obs % cov % fact_chol = from_obs % cov % fact_chol
  to_obs % cov % equi_chol = from_obs % cov % equi_chol

END SUBROUTINE ropp_fm_assign_obs_1drefrac

