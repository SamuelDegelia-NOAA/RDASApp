! $Id$

SUBROUTINE ropp_fm_bangle_1d_grad(x, y, K)

!****s* BendingAngle/ropp_fm_bangle_1d_grad *
!
! NAME
!     ropp_fm_bangle_1d_grad - Full gradient of the bending angle 
!                              forward model.
!
! SYNOPSIS
!    call ropp_fm_bangle_1d_grad(x, y, K)
!
! DESCRIPTION
!    This routine calculates the gradient of the bending angle forward model
!    The tangent linear and adjoint of the bending angle forward model can be 
!    computed to check their consistency.
!
! INPUTS
!    type(State1dFM)          :: x            ! State vector structure
!    type(Obs1dBangle)        :: y            ! Bending angle observation 
!
! OUTPUT
!    real(wp), dimension(:,:) :: gradient     ! Gradient K_ji = dH_j/dx_i
!
! NOTES
!    The obs vector is required only for the observation's geopotential levels;
!    no forward simulated bending angle profile is returned.
!
!    The shape of gradient must be consistent with the length of the
!    state and observation vector, respectively. Thus, if the state has
!    N elements, and there are M observations, the dimension of gradient
!    must be M x N.
!
! SEE ALSO
!   ropp_fm_bangle_1d
!   ropp_fm_bangle_1d_tl
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
  USE ropp_fm,   not_this => ropp_fm_bangle_1d_grad
  USE ropp_fm_types
  USE messages

  IMPLICIT NONE

  TYPE(State1dFM),          INTENT(in)    :: x    ! State vector
  TYPE(Obs1dBangle),        INTENT(in)    :: y    ! Observation vector
  REAL(wp), DIMENSION(:,:), INTENT(inout) :: K    ! Gradient: K_ji = dH_j/dx_i

  TYPE(State1dFM)                         :: x_tl ! State vector perturbation
  REAL(wp), DIMENSION(:), ALLOCATABLE     :: grad ! Gradient: grad(i)_j = dH_j/dx_i
  INTEGER                                 :: i    ! Counter

!-------------------------------------------------------------------------------
! 2. Check arguments
!-------------------------------------------------------------------------------

  IF (SIZE(K, 1) /= SIZE(y%bangle) .OR. SIZE(K, 2) /= SIZE(x%state)) THEN
     CALL message(msg_fatal, &
       "The dimensions of K are not in agreement with obs and/or state vector")
  END IF

!-------------------------------------------------------------------------------
! 3. Allocate state vector perturbation variables
!-------------------------------------------------------------------------------

  ALLOCATE(grad(SIZE(y%bangle)))

  ALLOCATE(x_tl%state(SIZE(x%state)))
  ALLOCATE(x_tl%pres(x%n_lev))
  ALLOCATE(x_tl%temp(x%n_lev))
  ALLOCATE(x_tl%shum(x%n_lev))
  ALLOCATE(x_tl%geop(x%n_lev))

  x_tl%n_lev = x%n_lev

!-------------------------------------------------------------------------------
! 4. Calculate the gradient of bending angle forward model
!-------------------------------------------------------------------------------

  DO i = 1, SIZE(x%state)

! 4.1 Initialise

     x_tl%state(:) = 0.0_wp
     x_tl%state(i) = 1.0_wp

     K(:, i) = 0.0_wp

! 4.2  Model-specific update to p, q, T variables

     IF(ASSOCIATED(x%ak))THEN
        CALL ropp_fm_state2state_ecmwf_tl(x, x_tl)
     ELSE
        CALL ropp_fm_state2state_meto_tl(x, x_tl)
        x_tl%geop(:) = 1.0_wp
     END IF

! 4.3 Compute tangent linear

     IF (x%direct_ion) THEN
       x_tl%Ne_max  = x_tl%state(SIZE(x%state)-2)
       x_tl%H_peak  = x_tl%state(SIZE(x%state)-1)
       x_tl%H_width = x_tl%state(SIZE(x%state))
     END IF

     CALL ropp_fm_bangle_1d_tl( x, x_tl, y, grad )
    
     ! The levels with missing values are silenced in order
     ! to make K consistent with the forward model:
     K(:,i) = grad*y%weights

  END DO

!-------------------------------------------------------------------------------
! 5. Clean up
!-------------------------------------------------------------------------------

  DEALLOCATE(grad)

  CALL ropp_fm_free(x_tl)

END SUBROUTINE ropp_fm_bangle_1d_grad
