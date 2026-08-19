! $Id$

!****f* Geodesy/great_circle_distance *
!
! NAME
!    great_circle_distance - Calculate great circle distance between two
!                              points on Earth.
!
! SYNOPSIS
!    dist = great_circle_distance(lon1, lat1, lon2, lat2)
! 
! DESCRIPTION
!    This function calculates the great circle distance between two points
!    on a sphere, given by their (geodetic) longitude and latitude coordinates,
!    respectively
!
! INPUTS
!    lon1   Longitude of first point on the sphere / Earth (in degree east).
!    lat1   Latitude of the first point (in degree north).
!    lon2   Longitude of the second point (in degree east).
!    lat2   Latitude of the second point (in degree north).
!
! OUTPUT
!    dist   Great circle distance between (lon1, lat1) and (lon2, lat2) (in meter).
!
! NOTES
!    It is assumed that Earth is a sphere with an average great-circle radius
!    of 6372.795 km. No corrections are applied for the ellipsoidal shape of
!    the Earth.
!
! EXAMPLE
!
!
! SEE ALSO
!
!
! REFERENCES
!    Formulation and default values were taken from
!
!       http://en.wikipedia.org/wiki/Great-circle_distance
!
!    cited in December 2006.
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
! COPYRIGHT
!    Copyright (c) 2006 Christian Marquardt        <christian@marquardt.sc>
!
!    All rights reserved.
!
!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:
!
!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software as well
!    as in supporting documentation.
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
!    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
!    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!****

!-------------------------------------------------------------------------------
! 1. Scalar version
!-------------------------------------------------------------------------------

function great_circle_distance_sca(lon1, lat1, lon2, lat2) result(distance)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal

  implicit none

  real(wp), intent(in) :: lon1
  real(wp), intent(in) :: lat1
  real(wp), intent(in) :: lon2
  real(wp), intent(in) :: lat2
  real(wp)             :: distance

  real(wp), parameter  :: r_earth = 6372.795e3_wp    ! earth radius
  real(wp), parameter  :: deg2rad = 0.0174532925_wp  ! degrees to radians

  real(wp)             :: lon1_rad, lat1_rad
  real(wp)             :: lon2_rad, lat2_rad
  real(wp)             :: delta_lon, x, y
  real(wp)             :: delta_phi

! 1.2 Convert arguments to radians
! --------------------------------

  lon1_rad = lon1 * deg2rad  ;  lat1_rad = lat1 * deg2rad
  lon2_rad = lon2 * deg2rad  ;  lat2_rad = lat2 * deg2rad

! 1.3 Calculate great circle angle
! --------------------------------

  delta_lon = lon1_rad - lon2_rad

  y = sqrt((cos(lat2_rad)*sin(delta_lon))**2  &
             + (cos(lat1_rad)*sin(lat2_rad) - sin(lat1_rad)*cos(lat2_rad)*cos(delta_lon))**2)
  x = sin(lat1_rad)*sin(lat2_rad) + cos(lat1_rad)*cos(lat2_rad)*cos(delta_lon)

  delta_phi = atan2(y, x)

! 1.4 Calculate great circle distance
! -----------------------------------

  distance = delta_phi * r_earth

end function great_circle_distance_sca


!-------------------------------------------------------------------------------
! 2. Array version
!-------------------------------------------------------------------------------

function great_circle_distance_arr(lon1, lat1, lon2, lat2) result(distance)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: lon1
  real(wp), dimension(:), intent(in) :: lat1
  real(wp), dimension(:), intent(in) :: lon2
  real(wp), dimension(:), intent(in) :: lat2
  real(wp), dimension(size(lon1))    :: distance

  real(wp), parameter  :: r_earth = 6372.795e3_wp    ! earth radius
  real(wp), parameter  :: deg2rad = 0.0174532925_wp  ! degrees to radians

  real(wp), dimension(size(lon1))    :: lon1_rad, lat1_rad
  real(wp), dimension(size(lon1))    :: lon2_rad, lat2_rad
  real(wp), dimension(size(lon1))    :: delta_lon, x, y
  real(wp), dimension(size(lon1))    :: delta_phi

! 2.2 Convert arguments to radians
! --------------------------------

  lon1_rad = lon1 * deg2rad  ;  lat1_rad = lat1 * deg2rad
  lon2_rad = lon2 * deg2rad  ;  lat2_rad = lat2 * deg2rad

! 2.3 Calculate great circle angle
! --------------------------------

  delta_lon = lon1_rad - lon2_rad

  y = sqrt((cos(lat2_rad)*sin(delta_lon))**2  &
             + (cos(lat1_rad)*sin(lat2_rad) - sin(lat1_rad)*cos(lat2_rad)*cos(delta_lon))**2)
  x = sin(lat1_rad)*sin(lat2_rad) + cos(lat1_rad)*cos(lat2_rad)*cos(delta_lon)

  delta_phi = atan2(y, x)

! 2.4 Calculate great circle distance
! -----------------------------------

  distance = delta_phi * r_earth

end function great_circle_distance_arr
