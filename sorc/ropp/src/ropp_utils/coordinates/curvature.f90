! $Id: curvature.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/curvature
!
! NAME
!    curvature - Determine the section curvature, its centre for the Earth
!                reference ellipsoid, and the curvature radius
!
! SYNOPSIS
!    call curvature(lat, lon, theta, r_coc, roc)
! 
! DESCRIPTION
!    This subroutine calculates the cartesian coordinates of the local centre
!    of curvature and the radius for the reference ellipsoid.
!
! INPUTS
!    lat           latitude of surface point
!    lon           longitude of surface point
!    theta         azimuth direction of the cross-section
!
! OUTPUT
!    r_coc         cartesian centre of curvature vector (relative to ECF frame)
!    roc           radius of curvature value           
!
! NOTES
!    Uses theorems of Meusnier and Euler
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

subroutine curvature(lat, lon, theta, r_coc, roc)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: geod2cart, deg2rad, Rp, Re

  implicit none

  real(wp), intent(in)                :: lat                 ! Surface point latitude
  real(wp), intent(in)                :: lon                 ! Surface point longitude
  real(wp), intent(in)                :: theta               ! Azimuth direction
  real(wp), dimension(3), intent(out) :: r_coc               ! Centre of curvature
  real(wp),               intent(out) :: roc                 ! Radius of curvature

  real(wp), dimension(3)              :: x                   ! Cartesian coordinates
  real(wp), dimension(3)              :: n                   ! Surface normal
  real(wp)                            :: rlat, rlon          ! lat, lon in radians

  real(wp)                            :: r_NS                ! NS radius of curvature
  real(wp)                            :: r_EW                ! EW radius of curvature
  real(wp)                            :: cphi2, sphi2 ! cos^2 and sin^2 of geodetic latitude
  real(wp)                            :: atoc2, xy2, z2      ! Working constants

! 1.2 Initialisation
! ------------------

  rlat = lat * deg2rad
  rlon = lon * deg2rad

  call geod2cart(lat, lon, 0.0_wp, x)

! 1.3 Calculate radius of curvature
! ---------------------------------

  atoc2 = (Re/Rp)**2

  xy2 = x(1)**2 + x(2)**2

  z2 = (atoc2*x(3))**2

  cphi2 = xy2 / (xy2 + z2)

  sphi2 = z2  / (xy2 + z2)

  r_EW = 1.0_wp / SQRT( cphi2 + (sphi2/atoc2) )  ! normalised wrt Re

  r_NS = r_EW**3 / atoc2                         ! normalised wrt Re

  roc = Re / ( (COS(theta)**2/r_NS) + (SIN(theta)**2/r_EW) )

! 1.4 Calculate curvature centre
! ------------------------------
  
  ! Normal to surface
  
  n(1) = Cos(rlat)*Cos(rlon)
  n(2) = Cos(rlat)*Sin(rlon)
  n(3) = Sin(rlat)

  r_coc(:) = x(:) - roc*n(:)

end subroutine curvature



