! $Id: ropp_fm_dzorro_dlg.f90 4010 2014-01-10 11:07:40Z idculv $

!****f* Ionosphere/ropp_fm_dzorro_dlg
!
! NAME
!   ropp_fm_dzorro_dlg  - compute the derivative of the ionospheric bending with the "Zorro" function
!
! SYNOPSIS
!   dz_by_dlg = ropp_fm_dzorro_dlg(lg)
!
! DESCRIPTION
!   The ionosphere bending function is estimated with a Pade approximation.
!
! INPUTS
!   lg = (R_peak - impact_param) / H_width
!
! OUTPUT
!   Derivative of Zorro function at lg.
!
! NOTES
!   This function evaluates the derivative wrt log(gamma) of the Zorro function,
!   which is a key function in the ionospheric bending produced by a Chapman layer. 
!   Zorro is estimated by a Pade approximation, thus:
!
!   Zorro = sqrt(2*pi*theta) * num / den
!   where
!   num = (p1 + p2*theta + p3*theta^2 + p4*theta^3)
!   den = (q1 + q2*theta + q3*theta^2 + q4*theta^3 + q5*theta^4 + q6*theta^5)
!   and
!   theta = asinh(gamma/2).
!
! SEE ALSO
!   ropp_fm_zorro
!   ropp_fm_iono_bangle
!
! REFERENCES
!
! AUTHOR
!   Met Office, Exeter, UK and ECMWF, Reading, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

FUNCTION ropp_fm_dzorro_dlg(lg) RESULT(dz_by_dl)

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm, not_this => ropp_fm_dzorro_dlg
  USE ropp_fm_constants, ONLY: pi

  IMPLICIT NONE

  REAL(wp), DIMENSION(:)                :: lg
  REAL(wp), DIMENSION(SIZE(lg))         :: dz_by_dl, theta, num, den, num1, den1
  INTEGER, PARAMETER                    :: n_num=4, n_den=6
  REAL(wp), PARAMETER, DIMENSION(n_num) :: p_coeff=(/ -1.41421360_wp, &
                                                       2.32540970_wp, &
                                                      -1.11628850_wp, &
                                                       0.23605387_wp /)
  REAL(wp), PARAMETER, DIMENSION(n_den) :: q_coeff=(/  1.00000000_wp, &
                                                       0.15210651_wp, &
                                                      -0.76649105_wp, &
                                                       1.26080520_wp, &
                                                      -0.84687066_wp, &
                                                       0.23605387_wp /)
  INTEGER                               :: i

!-------------------------------------------------------------------------------
! 2. Calculations
!-------------------------------------------------------------------------------

! Define theta = asinh(gamma/2)

  theta = ropp_fm_asinh(EXP(lg)/2.0_wp)
  
  num(:) = p_coeff(n_num)
  DO i=n_num-1,1,-1
    num = num*theta + p_coeff(i)
  END DO

  num1(:) = (n_num-1)*p_coeff(n_num)
  DO i=n_num-1,2,-1
    num1 = num1*theta + (i-1)*p_coeff(i)
  END DO

  den(:) = q_coeff(n_den)
  DO i=n_den-1,1,-1
    den = den*theta + q_coeff(i)
  END DO

  den1(:) = (n_den-1)*q_coeff(n_den)
  DO i=n_den-1,2,-1
    den1 = den1*theta + (i-1)*q_coeff(i)
  END DO

  dz_by_dl = TANH(theta) * SQRT(2.0_wp*pi*theta) * &
             (0.5_wp*num*den + theta*(num1*den - num*den1)) / (theta*den*den)

END FUNCTION ropp_fm_dzorro_dlg
