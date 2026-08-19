! $Id$

!****s* Copying/ropp_fm_state2state *
!
! NAME
!    ropp_assign_state - Copy state vectors.
!
! SYNOPSIS
!    use ropp_fm
!      ...
!    new_state = old_state
!
! DESCRIPTION
!    This subroutine is used for overloading the assign operator (=)
!    for state vectors.
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
! 1. 1D background data
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_state2state_1d(from_state, to_state)

! 1.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(State1dFM), INTENT(in)    :: from_state
  TYPE(State1dFM), INTENT(inout) :: to_state

! 1.2 Copy contents
! -----------------

  to_state % lon      = from_state % lon
  to_state % lat      = from_state % lat
  to_state % time     = from_state % time
  to_state % n_lev    = from_state % n_lev
  to_state % ispline  = from_state % ispline
  to_state % geop_sfc = from_state % geop_sfc
  to_state % ne_max   = from_state % ne_max
  to_state % h_peak   = from_state % h_peak
  to_state % h_width  = from_state % h_width
  to_state % n_chap   = from_state % n_chap

  CALL copy_alloc(from_state % state, to_state % state)
  CALL copy_alloc(from_state % temp,  to_state % temp)
  CALL copy_alloc(from_state % shum,  to_state % shum)
  CALL copy_alloc(from_state % pres,  to_state % pres)
  CALL copy_alloc(from_state % geop,  to_state % geop)

  IF (ASSOCIATED(from_state % ak)) THEN
    CALL copy_alloc(from_state % ak, to_state % ak)
    CALL copy_alloc(from_state % bk, to_state % bk)
  ENDIF

  to_state % state_ok      = from_state % state_ok
  to_state % cov_ok        = from_state % cov_ok
  to_state % use_logp      = from_state % use_logp
  to_state % use_logq      = from_state % use_logq
  to_state % non_ideal     = from_state % non_ideal
  to_state % spline_int    = from_state % spline_int
  to_state % check_qsat    = from_state % check_qsat
  to_state % check_qmin    = from_state % check_qmin
  to_state % direct_ion    = from_state % direct_ion
  to_state % new_ref_op    = from_state % new_ref_op
  to_state % new_bangle_op = from_state % new_bangle_op

! 1.3 Copy covariance (manually)
! ------------------------------

! Note: One really should use the copy operators from the matrix class,
!       BUT ropp_fm does not rely on having them - they are only
!       contained in ropp_1dvar...

  CALL copy_alloc(from_state % cov % d, to_state % cov % d)

  IF(ASSOCIATED(from_state%cov%e))THEN
    CALL copy_alloc(from_state % cov % e, to_state % cov % e)
  ELSE
    IF (ASSOCIATED(to_state % cov % e)) DEALLOCATE(to_state % cov % e)
  ENDIF

  IF(ASSOCIATED(from_state%cov%f))THEN
    CALL copy_alloc(from_state % cov % f, to_state % cov % f)
  ELSE
    IF (ASSOCIATED(to_state % cov % f)) DEALLOCATE(to_state % cov % f)
  ENDIF

  IF(ASSOCIATED(from_state%cov%s))THEN
    CALL copy_alloc(from_state % cov % s, to_state % cov % s)
  ELSE
    IF (ASSOCIATED(to_state % cov % s)) DEALLOCATE(to_state % cov % s)
  ENDIF

  to_state % cov % fact_chol = from_state % cov % fact_chol
  to_state % cov % equi_chol = from_state % cov % equi_chol

END SUBROUTINE ropp_fm_state2state_1d


!-------------------------------------------------------------------------------
! 2. 1D Background levels (assignment)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_assign_state_1d(to_state, from_state)

! 2.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(State1dFM), INTENT(inout) :: to_state
  TYPE(State1dFM), INTENT(in)    :: from_state
  
! 2.2 Copy contents
! -----------------

  to_state % lon      = from_state % lon
  to_state % lat      = from_state % lat
  to_state % time     = from_state % time
  to_state % n_lev    = from_state % n_lev
  to_state % ispline  = from_state % ispline
  to_state % geop_sfc = from_state % geop_sfc
  to_state % ne_max   = from_state % ne_max
  to_state % h_peak   = from_state % h_peak
  to_state % h_width  = from_state % h_width
  to_state % n_chap   = from_state % n_chap

  CALL copy_alloc(from_state % state, to_state % state)
  CALL copy_alloc(from_state % temp,  to_state % temp)
  CALL copy_alloc(from_state % shum,  to_state % shum)
  CALL copy_alloc(from_state % pres,  to_state % pres)
  CALL copy_alloc(from_state % geop,  to_state % geop)

  IF(ASSOCIATED(from_state%ak))THEN
    CALL copy_alloc(from_state % ak, to_state % ak)
    CALL copy_alloc(from_state % bk, to_state % bk)
  ENDIF

  to_state % state_ok      = from_state % state_ok
  to_state % cov_ok        = from_state % cov_ok
  to_state % use_logp      = from_state % use_logp
  to_state % use_logq      = from_state % use_logq
  to_state % non_ideal     = from_state % non_ideal
  to_state % spline_int    = from_state % spline_int
  to_state % check_qsat    = from_state % check_qsat
  to_state % check_qmin    = from_state % check_qmin
  to_state % direct_ion    = from_state % direct_ion
  to_state % new_ref_op    = from_state % new_ref_op
  to_state % new_bangle_op = from_state % new_bangle_op

! 2.3 Copy covariance (manually)
! ------------------------------

  CALL copy_alloc(from_state % cov % d, to_state % cov % d)

  IF(ASSOCIATED(from_state%cov%e))THEN
    CALL copy_alloc(from_state % cov % e, to_state % cov % e)
  ELSE
    IF (ASSOCIATED(to_state % cov % e)) DEALLOCATE(to_state % cov % e)
  ENDIF

  IF(ASSOCIATED(from_state%cov%f))THEN
    CALL copy_alloc(from_state % cov % f, to_state % cov % f)
  ELSE
    IF (ASSOCIATED(to_state % cov % f)) DEALLOCATE(to_state % cov % f)
  ENDIF

  IF(ASSOCIATED(from_state%cov%s))THEN
    CALL copy_alloc(from_state % cov % s, to_state % cov % s)
  ELSE
    IF (ASSOCIATED(to_state % cov % s)) DEALLOCATE(to_state % cov % s)
  ENDIF

  to_state % cov % fact_chol = from_state % cov % fact_chol
  to_state % cov % equi_chol = from_state % cov % equi_chol

END SUBROUTINE ropp_fm_assign_state_1d

!-------------------------------------------------------------------------------
! 3. 0D background data
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_state2state_0d(from_state, to_state)

! 3.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(State0dFM), INTENT(in)    :: from_state
  TYPE(State0dFM), INTENT(inout) :: to_state

! 3.2 Copy contents
! -----------------

  to_state % lon      = from_state % lon
  to_state % lat      = from_state % lat
  to_state % time     = from_state % time
  to_state % n_lay    = from_state % n_lay

  CALL copy_alloc(from_state % state,   to_state % state)
  CALL copy_alloc(from_state % ne_peak, to_state % ne_peak)
  CALL copy_alloc(from_state % r_peak,  to_state % r_peak)
  CALL copy_alloc(from_state % h_zero,  to_state % h_zero)
  CALL copy_alloc(from_state % h_grad,  to_state % h_grad)

  to_state % state_ok      = from_state % state_ok
  to_state % cov_ok        = from_state % cov_ok

! 3.3 Copy covariance (manually)
! ------------------------------

! Note: One really should use the copy operators from the matrix class,
!       BUT ropp_fm does not rely on having them - they are only
!       contained in ropp_1dvar...

  CALL copy_alloc(from_state % cov % d, to_state % cov % d)

  IF(ASSOCIATED(from_state%cov%e))THEN
    CALL copy_alloc(from_state % cov % e, to_state % cov % e)
  ELSE
    IF (ASSOCIATED(to_state % cov % e)) DEALLOCATE(to_state % cov % e)
  ENDIF

  IF(ASSOCIATED(from_state%cov%f))THEN
    CALL copy_alloc(from_state % cov % f, to_state % cov % f)
  ELSE
    IF (ASSOCIATED(to_state % cov % f)) DEALLOCATE(to_state % cov % f)
  ENDIF

  IF(ASSOCIATED(from_state%cov%s))THEN
    CALL copy_alloc(from_state % cov % s, to_state % cov % s)
  ELSE
    IF (ASSOCIATED(to_state % cov % s)) DEALLOCATE(to_state % cov % s)
  ENDIF

  to_state % cov % fact_chol = from_state % cov % fact_chol
  to_state % cov % equi_chol = from_state % cov % equi_chol

END SUBROUTINE ropp_fm_state2state_0d


!-------------------------------------------------------------------------------
! 4. 0D background levels (assignment)
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_assign_state_0d(to_state, from_state)

! 4.1 Declarations
! ----------------

  USE arrays
  USE ropp_fm_types

  IMPLICIT NONE

  TYPE(State0dFM), INTENT(inout) :: to_state
  TYPE(State0dFM), INTENT(in)    :: from_state
  
! 4.2 Copy contents
! -----------------
  to_state % lon      = from_state % lon
  to_state % lat      = from_state % lat
  to_state % time     = from_state % time
  to_state % n_lay    = from_state % n_lay

  CALL copy_alloc(from_state % state,   to_state % state)
  CALL copy_alloc(from_state % ne_peak, to_state % ne_peak)
  CALL copy_alloc(from_state % r_peak,  to_state % r_peak)
  CALL copy_alloc(from_state % h_zero,  to_state % h_zero)
  CALL copy_alloc(from_state % h_grad,  to_state % h_grad)

  to_state % state_ok      = from_state % state_ok
  to_state % cov_ok        = from_state % cov_ok

! 4.3 Copy covariance (manually)
! ------------------------------

  CALL copy_alloc(from_state % cov % d, to_state % cov % d)

  IF(ASSOCIATED(from_state%cov%e))THEN
    CALL copy_alloc(from_state % cov % e, to_state % cov % e)
  ELSE
    IF (ASSOCIATED(to_state % cov % e)) DEALLOCATE(to_state % cov % e)
  ENDIF

  IF(ASSOCIATED(from_state%cov%f))THEN
    CALL copy_alloc(from_state % cov % f, to_state % cov % f)
  ELSE
    IF (ASSOCIATED(to_state % cov % f)) DEALLOCATE(to_state % cov % f)
  ENDIF

  IF(ASSOCIATED(from_state%cov%s))THEN
    CALL copy_alloc(from_state % cov % s, to_state % cov % s)
  ELSE
    IF (ASSOCIATED(to_state % cov % s)) DEALLOCATE(to_state % cov % s)
  ENDIF

  to_state % cov % fact_chol = from_state % cov % fact_chol
  to_state % cov % equi_chol = from_state % cov % equi_chol

END SUBROUTINE ropp_fm_assign_state_0d
