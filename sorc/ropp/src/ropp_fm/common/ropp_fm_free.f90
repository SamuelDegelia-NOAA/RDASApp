! $Id: ropp_fm_free.f90 2045 2009-04-06 10:08:37Z frhl $

!****s* ForwardModels/ropp_fm_free *
!
! NAME
!    ropp_fm_free - Free arrays within State1dFM, Obs1dRefrac or Obs1dBangle
!                   derived types
!
! SYNOPSIS
!    USE ropp_fm
!    TYPE(State1dFM)   :: state
!    TYPE(Obs1dRefrac) :: refrac
!    TYPE(Obs1dBangle) :: bangle
!      ...
!    CALL ropp_fm_free(state)
!    CALL ropp_fm_free(refrac)
!    CALL ropp_fm_free(bangle)
!
! DESCRIPTION
!   This subroutine frees memory from a previously initialised data structures
!   used within ropp_fm
!
! SEE ALSO
!    ropp_fm_types
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
! 1. Observation vector for 1d refractivity
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_free_obs1drefrac(var)

! 1.1 Declarations
! ----------------

  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm_types, ONLY: Obs1dRefrac
  IMPLICIT NONE

  TYPE(Obs1dRefrac), INTENT(inout) :: var

! 1.2 Deallocate memory for all structure elements
! ------------------------------------------------

  IF (ASSOCIATED(var%refrac))  DEALLOCATE(var%refrac)
  IF (ASSOCIATED(var%geop))    DEALLOCATE(var%geop)
  IF (ASSOCIATED(var%weights)) DEALLOCATE(var%weights)

  var%lon  = ropp_MDFV
  var%lat  = ropp_MDFV
  var%time = ropp_MDFV

  IF (ASSOCIATED(var%cov%d)) DEALLOCATE(var%cov%d)
  IF (ASSOCIATED(var%cov%e)) DEALLOCATE(var%cov%e)
  IF (ASSOCIATED(var%cov%f)) DEALLOCATE(var%cov%f)
  IF (ASSOCIATED(var%cov%s)) DEALLOCATE(var%cov%s)

  var%obs_ok = .FALSE.
  var%cov_ok = .FALSE.

END SUBROUTINE ropp_fm_free_obs1drefrac

!-------------------------------------------------------------------------------
! 2. Observation vector for 1d bending angles
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_free_obs1dbangle(var)

! 2.1 Declarations
! ----------------

  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm_types, ONLY: Obs1dBangle
  IMPLICIT NONE

  TYPE(Obs1dBangle), INTENT(inout) :: var

! 2.2 Deallocate memory for all structure elements
! ------------------------------------------------

  IF (ASSOCIATED(var%bangle))  DEALLOCATE(var%bangle)
  IF (ASSOCIATED(var%impact))  DEALLOCATE(var%impact)
  IF (ASSOCIATED(var%weights)) DEALLOCATE(var%weights)
  IF (ASSOCIATED(var%rtan))    DEALLOCATE(var%rtan)
  IF (ASSOCIATED(var%a_path))  DEALLOCATE(var%a_path)

  var%lon        = ropp_MDFV
  var%lat        = ropp_MDFV
  var%time       = ropp_MDFV
  var%g_sfc      = ropp_MDFV
  var%r_earth    = ropp_MDFV
  var%r_curve    = ropp_MDFV
  var%r_gns      = ropp_MDFV
  var%r_leo      = ropp_MDFV
  var%f1         = ropp_MDFV
  var%f2         = ropp_MDFV
  var%undulation = ropp_MDFV
  var%azimuth    = ropp_MDFV

  IF (ASSOCIATED(var%cov%d)) DEALLOCATE(var%cov%d)
  IF (ASSOCIATED(var%cov%e)) DEALLOCATE(var%cov%e)
  IF (ASSOCIATED(var%cov%f)) DEALLOCATE(var%cov%f)
  IF (ASSOCIATED(var%cov%s)) DEALLOCATE(var%cov%s)

  var%obs_ok      = .FALSE.
  var%cov_ok      = .FALSE.
  var%diff_bangle = .FALSE.

END SUBROUTINE ropp_fm_free_obs1dbangle

!-------------------------------------------------------------------------------
! 3. Generic zero dimensional state vector data type
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_free_state0dfm(var)

! 3.1 Declarations
! ----------------

  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm_types, ONLY: State0dFM
  IMPLICIT NONE

  TYPE(State0dFM), INTENT(inout) :: var

! 3.2 Deallocate memory for all structure elements
! ------------------------------------------------

  IF (ASSOCIATED(var%state))   DEALLOCATE(var%state)
  IF (ASSOCIATED(var%ne_peak)) DEALLOCATE(var%ne_peak)
  IF (ASSOCIATED(var%r_peak))  DEALLOCATE(var%r_peak)
  IF (ASSOCIATED(var%h_zero))  DEALLOCATE(var%h_zero)
  IF (ASSOCIATED(var%h_grad))  DEALLOCATE(var%h_grad)

  var%lon      = ropp_MDFV
  var%lat      = ropp_MDFV
  var%time     = ropp_MDFV

  var%n_lay    = 0

  IF (ASSOCIATED(var%cov%d)) DEALLOCATE(var%cov%d)
  IF (ASSOCIATED(var%cov%e)) DEALLOCATE(var%cov%e)
  IF (ASSOCIATED(var%cov%f)) DEALLOCATE(var%cov%f)
  IF (ASSOCIATED(var%cov%s)) DEALLOCATE(var%cov%s)

  var%state_ok   = .FALSE.
  var%cov_ok     = .FALSE.

END SUBROUTINE ropp_fm_free_state0dfm

!-------------------------------------------------------------------------------
! 4. Generic one dimensional state vector data type
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_free_state1dfm(var)

! 4.1 Declarations
! ----------------

  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm_types, ONLY: State1dFM
  IMPLICIT NONE

  TYPE(State1dFM), INTENT(inout) :: var

! 4.2 Deallocate memory for all structure elements
! ------------------------------------------------

  IF (ASSOCIATED(var%state)) DEALLOCATE(var%state)
  IF (ASSOCIATED(var%temp))  DEALLOCATE(var%temp)
  IF (ASSOCIATED(var%shum))  DEALLOCATE(var%shum)
  IF (ASSOCIATED(var%pres))  DEALLOCATE(var%pres)
  IF (ASSOCIATED(var%geop))  DEALLOCATE(var%geop)
  IF (ASSOCIATED(var%ak))    DEALLOCATE(var%ak)
  IF (ASSOCIATED(var%bk))    DEALLOCATE(var%bk)

  var%lon      = ropp_MDFV
  var%lat      = ropp_MDFV
  var%time     = ropp_MDFV

  var%geop_sfc = ropp_MDFV

  var%Ne_max   = ropp_MDFV
  var%H_peak   = ropp_MDFV
  var%H_width  = ropp_MDFV
  var%n_lev    = 0
  var%n_chap   = 0
  var%ispline = 2

  IF (ASSOCIATED(var%cov%d)) DEALLOCATE(var%cov%d)
  IF (ASSOCIATED(var%cov%e)) DEALLOCATE(var%cov%e)
  IF (ASSOCIATED(var%cov%f)) DEALLOCATE(var%cov%f)
  IF (ASSOCIATED(var%cov%s)) DEALLOCATE(var%cov%s)

  var%state_ok   = .FALSE.
  var%cov_ok     = .FALSE.
  var%use_logp   = .FALSE.
  var%use_logq   = .FALSE.
  var%non_ideal  = .FALSE.
  var%spline_int = .FALSE.
  var%direct_ion = .FALSE.
  var%check_qsat = .FALSE.
  var%check_qmin = .TRUE.

END SUBROUTINE ropp_fm_free_state1dfm

!-------------------------------------------------------------------------------
! 5. Generic two dimensional state vector data type
!-------------------------------------------------------------------------------

SUBROUTINE ropp_fm_free_state2dfm(var)

! 5.1 Declarations
! ----------------

  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm_types, ONLY: State2dFM
  IMPLICIT NONE

  TYPE(State2dFM), INTENT(inout) :: var

! 5.2 Deallocate memory for all structure elements
! ------------------------------------------------

  IF (ASSOCIATED(var%temp))      DEALLOCATE(var%temp)
  IF (ASSOCIATED(var%shum))      DEALLOCATE(var%shum)
  IF (ASSOCIATED(var%pres))      DEALLOCATE(var%pres)
  IF (ASSOCIATED(var%geop_sfc))  DEALLOCATE(var%geop_sfc)
  IF (ASSOCIATED(var%pres_sfc))  DEALLOCATE(var%pres_sfc)
  IF (ASSOCIATED(var%geop))      DEALLOCATE(var%geop)
  IF (ASSOCIATED(var%ak))        DEALLOCATE(var%ak)
  IF (ASSOCIATED(var%bk))        DEALLOCATE(var%bk)
  IF (ASSOCIATED(var%lat))       DEALLOCATE(var%lat)
  IF (ASSOCIATED(var%lon))       DEALLOCATE(var%lon)
  IF (ASSOCIATED(var%refrac))    DEALLOCATE(var%refrac)
  IF (ASSOCIATED(var%nr))        DEALLOCATE(var%nr)

  var%n_lev      = 0
  var%n_horiz    = 0
  var%dtheta     = ropp_MDFV
  var%time       = ropp_MDFV
  var%state_ok   = .FALSE.
  var%check_qsat = .FALSE.
  var%check_qmin = .TRUE.

END SUBROUTINE ropp_fm_free_state2dfm

