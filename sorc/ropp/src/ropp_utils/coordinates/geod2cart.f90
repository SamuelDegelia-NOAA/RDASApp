! $Id: geod2cart.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/geod2cart
!
! NAME
!    geod2cart - Transform occultation geodetic latitude, longitude and height 
!                data to geometry in cartesian coordinates.
!
! SYNOPSIS
!    call geod2cart(lat, lon, height, cart)
! 
! DESCRIPTION
!    This subroutine calculates a cartesian vector relative to the
!    ECF frame from occultation latitude, longitude and height 
!    relative to the WGS-84 ellipsoid.
!
! INPUTS
!    lat           latitude (deg)
!    lon           longitude (deg)
!    height        geometric height relative to WGS-84 ellipsoid (m)
!
! OUTPUT
!    cart          cartesian position vector (relative to ECF frame)
!
! NOTES
!
! SEE ALSO
!   cart2geod
!
! REFERENCES
!    Escobal, Methods of orbit determination
!    1965, Wiley & sons, Inc. Pp. 27 - 29.
!
!    Mahoney, M.J., A discussion of various measures of altitudes,
!       URL: http://mtp.jpl.nasa.gov/notes/altitude/altitude.html,
!       cited 13/01/2009.
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

subroutine geod2cart_0d(lat, lon, height, cart)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: deg2rad, Re, f

  implicit none

  real(wp), intent(in)                :: lat      ! latitude (deg)
  real(wp), intent(in)                :: lon      ! longitude (deg)
  real(wp), intent(in)                :: height   ! height (m)
  real(wp), dimension(3), intent(out) :: cart     ! position vector (ECF)

  real(wp)                            :: rlat, rlon
  real(wp)                            :: gd  ! Parallel curvature of ellipsoid 
  real(wp)                            :: flatfn, funsq

! 1.2 Initialisation
! ------------------

  rlat = lat * deg2rad
  rlon = lon * deg2rad

  flatfn = (2.0_wp - f) * f
  funsq = (1.0_wp - f)**2
  gd = Re/Sqrt(1.0_wp - flatfn*Sin(rlat)**2)

! 1.3 Transform to cartesian elements
! -----------------------------------

  cart(1) = Cos(rlat)*Cos(rlon)*(gd + height)
  cart(2) = Cos(rlat)*Sin(rlon)*(gd + height)
  cart(3) = Sin(rlat)*(gd*funsq + height)

end subroutine geod2cart_0d


!-------------------------------------------------------------------------------
! 2. Double, array argument for position
!-------------------------------------------------------------------------------

subroutine geod2cart_1d(lat, lon, height, cart)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: geod2cart
  
  implicit none

  real(wp), dimension(:), intent(in)     :: lat    ! latitude
  real(wp), dimension(:), intent(in)     :: lon    ! longitude
  real(wp), dimension(:), intent(in)     :: height ! height
  real(wp), dimension(:,:), intent(out)  :: cart   ! position (ECF)

  integer :: i
  
! 2.2 Transform to cartesian vector
! ---------------------------------

  do i=1,size(lat)
     call geod2cart(lat(i), lon(i), height(i), cart(i,:)) 
  enddo 
  
end subroutine geod2cart_1d



