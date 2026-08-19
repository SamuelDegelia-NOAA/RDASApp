! $Id$

SUBROUTINE ropp_fm_refrac_1d_grad(x, y, K)

!****s* Refractivity/ropp_fm_refrac_1d_grad *
!
! NAME
!    ropp_fm_refrac_1d_grad - Full gradient of the refractivity forward model.
!
! SYNOPSIS
!    call ropp_fm_refrac_1d_grad(x, y, K)
! 
! DESCRIPTION
!    This routine calculates the gradient of the refractivity forward model.
!    The tangent linear and adjoint of the refractivity forward model can be 
!    computed to check their consistency.
!
! INPUTS
!    type(State1dFM)          :: x       ! State vector structure   
!    type(Obs1dRefrac)        :: y       ! Observation vector structure
!
! OUTPUT
!    real(wp), dimension(:,:) :: K       ! Gradient K_ij = dH[x_i]/dy_j
!
! NOTES
!    The obs vector is required only for the observation's geopotential 
!    height levels; no forward simulated refractivity profile is returned.
!
!    The shape of gradient must be consistent with the length of the
!    state and observation vector, respectively. Thus, if the state has
!    N elements, and there are M observations, the dimension of gradient
!    must be M x N.
!
! SEE ALSO
!    ropp_fm_refrac
!    ropp_fm_refrac_tl
!    ropp_fm_refrac_new
!    ropp_fm_refrac_new_tl
!
! REFERENCES
!
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
  USE ropp_fm,   not_this => ropp_fm_refrac_1d_grad
  USE ropp_fm_types
  USE messages

  IMPLICIT NONE

  TYPE(State1dFM),          INTENT(in)    :: x    ! State vector
  TYPE(Obs1dRefrac),        INTENT(inout) :: y    ! Observation vector
  REAL(wp), DIMENSION(:,:), INTENT(inout) :: K    ! Gradient

  TYPE(State1dFM)                         :: x_tl ! State vector perturbation
  REAL(wp), DIMENSION(:), ALLOCATABLE     :: grad ! Gradient dH[x_i]/dy
  INTEGER                                 :: i    ! Counter

!-------------------------------------------------------------------------------
! 2. Check arguments
!-------------------------------------------------------------------------------

  IF (SIZE(K, 1) /= SIZE(y%refrac) .OR. SIZE(K, 2) /= SIZE(x%state)) THEN
     CALL message(msg_fatal, &
       "The dimensions of K are not in agreement with obs and/or state vector")
  ENDIF

!-------------------------------------------------------------------------------
! 3. Allocate state vector perturbation variables
!-------------------------------------------------------------------------------

  ALLOCATE(grad(SIZE(y%refrac)))
  ALLOCATE(x_tl%state(SIZE(x%state)))
  ALLOCATE(x_tl%pres(x%n_lev))
  ALLOCATE(x_tl%temp(x%n_lev))
  ALLOCATE(x_tl%shum(x%n_lev))
  ALLOCATE(x_tl%geop(x%n_lev))

!-------------------------------------------------------------------------------
! 4. Calculate the gradient of refractivity forward model
!-------------------------------------------------------------------------------

  DO i = 1, SIZE(x%state)

! 4.1 Initialise

     x_tl%state(:) = 0.0_wp   
     x_tl%state(i) = 1.0_wp

     K(:,i) = 0.0_wp

! 4.2  Model-specific update to p, q, T variables

     IF(ASSOCIATED(x%ak))THEN 
        CALL ropp_fm_state2state_ecmwf_tl(x, x_tl)
     ELSE
        CALL ropp_fm_state2state_meto_tl(x, x_tl)
        x_tl%geop(:) = 0.0_wp
     ENDIF

! 4.3 Compute tangent linear

     IF (x%new_ref_op) THEN
       CALL ropp_fm_refrac_1d_new_tl( x, x_tl, y, grad )
     ELSE
       CALL ropp_fm_refrac_1d_tl( x, x_tl, y, grad )
     END IF
     
     ! The levels with missing values are silenced in order
     ! to make K consistent with the forward model:
     K(:,i) = grad*y%weights
     
  ENDDO
  
!-------------------------------------------------------------------------------
! 5. Clean up
!-------------------------------------------------------------------------------
  
  DEALLOCATE(grad)
  CALL ropp_fm_free(x_tl)

END SUBROUTINE ropp_fm_refrac_1d_grad
