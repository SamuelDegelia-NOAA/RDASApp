! $Id: cart2geod.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/cart2geod
!
! NAME
!    cart2geod - Transform occultation geometry in cartesian coordinates
!                to geodetic latitude, longitude and height data.
!
! SYNOPSIS
!    call cart2geod(cart, lat, lon, height)
! 
! DESCRIPTION
!    This subroutine calculates occultation latitude, longitude and height 
!    relative to the WGS-84 ellipsoid from a cartesian vector relative to the
!    ECF frame. 
!
! INPUTS
!    cart        cartesian position vector (relative to ECF frame)

!
! OUTPUT
!    lat           latitude (deg)
!    lon           longitude (deg)
!    height        geometric height relative to WGS-84 ellipsoid (m)
!
! NOTES
!    Fast approximate solution
!
! SEE ALSO
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

subroutine cart2geod_0d(cart, lat, lon, height)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: rad2deg, Re, Rp, e

  implicit none

  real(wp), dimension(3), intent(in) :: cart     ! position vector (ECF)
  real(wp), intent(out)              :: lat      ! latitude (deg)
  real(wp), intent(out)              :: lon      ! longitude (deg)
  real(wp), intent(out)              :: height   ! height (m)

  real(wp)                           :: rlat
  real(wp)                           :: Rxy, Theta, CosPhi, SinPhi

! 1.2 Initialisation
! ------------------

  Rxy = Sqrt(Sum(cart(1:2)**2))
  Theta = Atan2(cart(3)*Re, Rxy*Rp)

! 1.3 Calculate longitude
! -----------------------

  lon = Atan2(cart(2), cart(1)) * rad2deg

! 1.4 Approximate calculation of latitude
! ----------------------------------------

  rlat = Atan2(cart(3) + e*Rp*Sin(Theta)**3, Rxy - e*Re*Cos(Theta)**3)
  lat = rlat * rad2deg

  CosPhi = Cos(rlat)
  SinPhi = Sin(rlat)

! 1.5 Calculate height from longitude
! -----------------------------------

  height = -(-CosPhi*Rp**2*Rxy - Re**2*SinPhi*cart(3) +  &
              Re*Rp*Sqrt(CosPhi**2*Rp**2 +               &
              Re**2*SinPhi**2 - SinPhi**2*Rxy**2 +    &
              2*CosPhi*SinPhi*Rxy*cart(3) -            &
              CosPhi**2*cart(3)**2))/                  &
              (CosPhi**2*Rp**2 + Re**2*SinPhi**2)

end subroutine cart2geod_0d


!-------------------------------------------------------------------------------
! 2. Double, array argument for position
!-------------------------------------------------------------------------------

subroutine cart2geod_1d(cart, lat, lon, height)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: cart2geod
  
  implicit none

  real(wp), dimension(:,:), intent(in)           :: cart   ! position (ECF)
  real(wp), dimension(size(cart,1)), intent(out) :: lat    ! latitude
  real(wp), dimension(size(cart,1)), intent(out) :: lon    ! longitude
  real(wp), dimension(size(cart,1)), intent(out) :: height ! height

  integer :: i
  
! 2.2 Transform to geodetic variables
! -----------------------------------

  do i=1,size(cart,1)
     call cart2geod(cart(i,:), lat(i), lon(i), height(i)) 
  enddo 
  
end subroutine cart2geod_1d



