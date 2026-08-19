! $Id: impact_parameter.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/impact_parameter
!
! NAME
!    impact parameter - Determine the impact parameter
!
! SYNOPSIS
!    impact =  impact_parameter(r_leo, r_gns, bangle)
! 
! DESCRIPTION
!    This function calculates the impact parameter connecting two given points
!    assuming spherical symmetry using trigonometric formulae.
!    With no knowledge of the bending angle (i.e. default processing), this 
!    routine returns the straight line tangent altitude.
!
! INPUTS
!    r_leo         cartesian LEO position vector (relative to ECF frame)
!    r_gns         cartesian GPS position vector (relative to ECF frame)
!    bangle        bending angle
!
! OUTPUT
!    impact        impact parameter
!
! SEE ALSO
!
! REFERENCES
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
! 1. Double, scalar arguments 
!-------------------------------------------------------------------------------

function impact_parameter_0d(r_leo, r_gns, bangle) result(impact)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: Pi, vector_angle

  implicit none

  real(wp), dimension(3), intent(in) :: r_leo   ! LEO position vector (ECF)
  real(wp), dimension(3), intent(in) :: r_gns   ! GPS position vector (ECF)
  real(wp), optional, intent(in)     :: bangle  ! Bending angle
  real(wp)                           :: impact  ! Impact parameter

  real(wp)               :: r0       ! Length of r_leo
  real(wp)               :: r1       ! Length of r_gns
  real(wp)               :: omega    ! Complementary to r_leo^r_gns - bangle
  real(wp)               :: talpha   ! Tan(r_leo^(r_leo-r_gns))

! 1.2 Length of vectors r_leo and r_gns
! -------------------------------------

  r0 = Sqrt(Dot_Product(r_leo, r_leo))
  r1 = Sqrt(Dot_Product(r_gns, r_gns))

! 1.3 Find vector angle between r_leo and r_gns
! ---------------------------------------------

  omega = Pi - vector_angle(r_leo, r_gns)

  if (present(bangle)) then
     omega = omega + bangle
  endif

! 1.4 Determine impact parameter by trigonometry
! ----------------------------------------------

  talpha = r1*Sin(omega) / (r0 + r1*Cos(omega))

  impact = r0*talpha / Sqrt(1.0_wp + talpha**2)

end function impact_parameter_0d

!-------------------------------------------------------------------------------
! 2. Double, array argument for position
!-------------------------------------------------------------------------------

function impact_parameter_1d(r_leo, r_gns, bangle) result(impact)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: impact_parameter

  implicit none

  real(wp), dimension(:,:), intent(in) :: r_leo   ! LEO position (ECF)
  real(wp), dimension(:,:), intent(in) :: r_gns   ! GPS position (ECF)
  real(wp), dimension(:), optional, intent(in) :: bangle   ! Bending angle
  real(wp), dimension(size(r_leo,1))   :: impact   ! Impact parameter

  integer :: i
  
! 2.2 Compute impact parameter
! ----------------------------

  do i=1,size(r_leo,1)
     if (present(bangle)) then
        impact(i) =  impact_parameter(r_leo(i,:), r_gns(i,:), bangle(i))
     else
        impact(i) =  impact_parameter(r_leo(i,:), r_gns(i,:))
     endif
  enddo
  
end function impact_parameter_1d



