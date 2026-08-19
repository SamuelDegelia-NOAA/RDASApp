! $Id: coordinates.f90 1958 2008-11-13 10:33:57Z frhl $

module coordinates

!****m* Coordinates/coordinates *
!
! NAME
!    coordinates - Module interfacing various geometric routines and functions.
!
! SYNOPSIS
!    use coordinates
!
! DESCRIPTION
!    This module provides interfaces to some geometric funtions and
!    subroutines of the ropp_utils library.
!
! SEE ALSO
!    ecf2eci
!    eci2ecf
!    eci2eci
!    impact_parameter
!    occ_point
!    tangent_point
!    plane_coordinates
!    rotate
!    earth
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

  use typesizes, only: wp => EightByteReal

!****p* Mathematics/Pi *
!
! NAME
!    Pi - Mathematical constant
!
! SOURCE
!
  real(wp), parameter :: Pi = 3.141592653589793238_wp
!
!****

!****p* Coordinates/deg2rad *
!
! NAME
!    deg2rad - Degrees to radians conversion
!    rad2deg - Radians to degrees conversion
!
! SOURCE
!
  real(wp), parameter  :: deg2rad = Pi / 180.0_wp

  real(wp), parameter  :: rad2deg = 180.0_wp / Pi
!
!****

!****p* Coordinates/global *
!
! NAME
!    Re - Equatorial semiaxis (m)
!    Rp - Polar semiaxis (m)
!    f  - Flatness
!    e  - Squared eccentricity
!
! REFERENCES
!   http://earth-info.nga.mil/GandG/wgs84/gravitymod/index.html
!
! SOURCE
!
  real(wp), parameter  :: Re = 6378137.0_wp

  real(wp), parameter  :: f  = 1.0_wp / 298.2572235630_wp

  real(wp), parameter  :: Rp = Re * (1.0_wp - f)

  real(wp), parameter  :: e  = (Re**2 - Rp**2)/(Re**2)
!
!****

!-------------------------------------------------------------------------------
! 1. Position vector transformations (ECF to ECI, ECI to ECF)
!-------------------------------------------------------------------------------

  interface ecf2eci
     function ecf2eci_0d(year, month, day, hour, minute, sec, dsec, r_ecf, SOFA, CIO) result(r_eci)
       use typesizes,  only: wp => EightByteReal
       integer, intent(inout)             :: year
       integer, intent(inout)             :: month
       integer, intent(inout)             :: day
       integer, intent(in)                :: hour
       integer, intent(in)                :: minute
       integer, intent(in)                :: sec
       real(wp), intent(in)               :: dsec
       real(wp), dimension(3), intent(in) :: r_ecf
       logical,  optional                 :: SOFA
       logical,  optional                 :: CIO
       real(wp), dimension(3)             :: r_eci
     end function ecf2eci_0d
     function ecf2eci_1d(year, month, day, hour, minute, sec, dsec, r_ecf, SOFA, CIO) result(r_eci)
       use typesizes,  only: wp => EightByteReal
       integer, intent(inout)               :: year
       integer, intent(inout)               :: month
       integer, intent(inout)               :: day
       integer, intent(in)                  :: hour
       integer, intent(in)                  :: minute
       integer, intent(in)                  :: sec
       real(wp), dimension(:),   intent(in) :: dsec
       real(wp), dimension(:,:), intent(in) :: r_ecf
       logical,  optional                   :: SOFA
       logical,  optional                   :: CIO
       real(wp), dimension(size(r_ecf,1),size(r_ecf,2)) :: r_eci
     end function ecf2eci_1d
  end interface

  interface eci2ecf
     function eci2ecf_0d(year, month, day, hour, minute, sec, dsec, r_eci, SOFA, CIO) result(r_ecf)
       use typesizes,  only: wp => EightByteReal
       integer, intent(inout)             :: year
       integer, intent(inout)             :: month
       integer, intent(inout)             :: day
       integer, intent(in)                :: hour
       integer, intent(in)                :: minute
       integer, intent(in)                :: sec
       real(wp), intent(in)               :: dsec
       real(wp), dimension(3), intent(in) :: r_eci
       logical,  optional                 :: SOFA
       logical,  optional                 :: CIO
       real(wp), dimension(3)             :: r_ecf
     end function eci2ecf_0d
     function eci2ecf_1d(year, month, day, hour, minute, sec, dsec, r_eci, SOFA, CIO) result(r_ecf)
       use typesizes,  only: wp => EightByteReal
       integer, intent(inout)               :: year
       integer, intent(inout)               :: month
       integer, intent(inout)               :: day
       integer, intent(in)                  :: hour
       integer, intent(in)                  :: minute
       integer, intent(in)                  :: sec
       real(wp), dimension(:),   intent(in) :: dsec
       real(wp), dimension(:,:), intent(in) :: r_eci
       logical,  optional                   :: SOFA
       logical,  optional                   :: CIO
       real(wp), dimension(size(r_eci,1),size(r_eci,2)) :: r_ecf
     end function eci2ecf_1d
  end interface

!-------------------------------------------------------------------------------
! 2. Position vector transformation (ECI at J2000 to intermediate ECI frame)
!-------------------------------------------------------------------------------

  interface eci2eci
     function eci2eci_0d(year, month, day, hour, minute, sec, dsec, r_eci_from, SOFA, CIO) result(r_eci_to)
       use typesizes,  only: wp => EightByteReal
       integer, intent(inout)             :: year
       integer, intent(inout)             :: month
       integer, intent(inout)             :: day
       integer, intent(in)                :: hour
       integer, intent(in)                :: minute
       integer, intent(in)                :: sec
       real(wp), intent(in)               :: dsec
       real(wp), dimension(3), intent(in) :: r_eci_from
       logical, optional                  :: SOFA
       logical, optional                  :: CIO
       real(wp), dimension(3)             :: r_eci_to
     end function eci2eci_0d
     function eci2eci_1d(year, month, day, hour, minute, sec, dsec, r_eci_from, SOFA, CIO) result(r_eci_to)
       use typesizes,  only: wp => EightByteReal
       integer, intent(inout)               :: year
       integer, intent(inout)               :: month
       integer, intent(inout)               :: day
       integer, intent(in)                  :: hour
       integer, intent(in)                  :: minute
       integer, intent(in)                  :: sec
       real(wp), dimension(:),   intent(in) :: dsec
       real(wp), dimension(:,:), intent(in) :: r_eci_from
       logical, optional                    :: SOFA
       logical, optional                    :: CIO
       real(wp), dimension(size(r_eci_from,1),size(r_eci_from,2)) :: r_eci_to
     end function eci2eci_1d
  end interface

!-------------------------------------------------------------------------------
! 3. Position vector transformation (Cartesian to Geodetic)
!-------------------------------------------------------------------------------

  interface cart2geod
     subroutine cart2geod_0d(cart, lat, lon, height)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(3), intent(in) :: cart
       real(wp), intent(out)              :: lat
       real(wp), intent(out)              :: lon
       real(wp), intent(out)              :: height
     end subroutine cart2geod_0d
     subroutine cart2geod_1d(cart, lat, lon, height)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(:,:),          intent(in)  :: cart
       real(wp), dimension(size(cart,1)), intent(out) :: lat
       real(wp), dimension(size(cart,1)), intent(out) :: lon
       real(wp), dimension(size(cart,1)), intent(out) :: height
     end subroutine cart2geod_1d
  end interface

!-------------------------------------------------------------------------------
! 4. Position vector transformation (Geodetic to Cartesian)
!-------------------------------------------------------------------------------

  interface geod2cart
     subroutine geod2cart_0d(lat, lon, height, cart)
       use typesizes,  only: wp => EightByteReal
       real(wp), intent(in)                :: lat
       real(wp), intent(in)                :: lon
       real(wp), intent(in)                :: height
       real(wp), dimension(3), intent(out) :: cart
     end subroutine geod2cart_0d
     subroutine geod2cart_1d(lat, lon, height, cart)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(:), intent(in)     :: lat
       real(wp), dimension(:), intent(in)     :: lon
       real(wp), dimension(:), intent(in)     :: height
       real(wp), dimension(:,:), intent(out)  :: cart
     end subroutine geod2cart_1d
  end interface

!-------------------------------------------------------------------------------
! 5. Compute impact parameter
!-------------------------------------------------------------------------------
  interface impact_parameter
     function impact_parameter_0d(r_leo, r_gns, bangle) result(impact)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(3), intent(in) :: r_leo
       real(wp), dimension(3), intent(in) :: r_gns
       real(wp), optional, intent(in)     :: bangle
       real(wp)                           :: impact
     end function impact_parameter_0d
     function impact_parameter_1d(r_leo, r_gns, bangle) result(impact)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(:,:), intent(in) :: r_leo
       real(wp), dimension(:,:), intent(in) :: r_gns
       real(wp), dimension(:), optional, intent(in) :: bangle
       real(wp), dimension(size(r_leo,1))   :: impact
     end function impact_parameter_1d
  end interface

!-------------------------------------------------------------------------------
! 6. Compute occultation point
!-------------------------------------------------------------------------------
  interface
     subroutine occ_point(dtime, r_leo, v_leo, r_gns, v_gns, lat, lon, &
                          r_coc, roc, azimuth, undulation, &
                          leo_pos, leo_vel, gns_pos, gns_vel, &
                          time_offset, cfile, efile)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(:),   intent(in)      :: dtime
       real(wp), dimension(:,:), intent(in)      :: r_leo
       real(wp), dimension(:,:), intent(in)      :: v_leo
       real(wp), dimension(:,:), intent(in)      :: r_gns
       real(wp), dimension(:,:), intent(in)      :: v_gns
       real(wp), intent(out)                     :: lat
       real(wp), intent(out)                     :: lon
       real(wp), dimension(size(r_leo,2)), intent(out) :: r_coc
       real(wp), intent(out)                     :: roc
       real(wp), intent(out)                     :: azimuth
       real(wp), intent(out)                     :: undulation
       real(wp), dimension(3), intent(out)       :: leo_pos
       real(wp), dimension(3), intent(out)       :: leo_vel
       real(wp), dimension(3), intent(out)       :: gns_pos
       real(wp), dimension(3), intent(out)       :: gns_vel
       real(wp), intent(out)                     :: time_offset
       character(len=*), optional, intent(in)    :: cfile
       character(len=*), optional, intent(in)    :: efile
     end subroutine occ_point
  end interface

!-------------------------------------------------------------------------------
! 7. Compute tangent points
!-------------------------------------------------------------------------------
  interface
     subroutine tangent_point(r_leo, r_gns, lat_tp, lon_tp, azimuth_tp, r_coc, ip)
       use typesizes,  only: wp => EightByteReal
       real(wp), dimension(:,:), intent(in)         :: r_leo
       real(wp), dimension(:,:), intent(in)         :: r_gns
       real(wp), dimension(:), intent(in)           :: r_coc
       real(wp), dimension(:), intent(out)          :: lat_tp
       real(wp), dimension(:), intent(out)          :: lon_tp
       real(wp), dimension(:), intent(out)          :: azimuth_tp
       real(wp), dimension(:), intent(in), optional :: ip
     end subroutine tangent_point
  end interface

!-------------------------------------------------------------------------------
! 8. Curvature
!-------------------------------------------------------------------------------
  interface
     subroutine curvature(lat, lon, theta, r_coc, roc)
       use typesizes,  only: wp => EightByteReal
       real(wp), intent(in)                :: lat
       real(wp), intent(in)                :: lon
       real(wp), intent(in)                :: theta
       real(wp), dimension(3), intent(out) :: r_coc
       real(wp), intent(out)               :: roc
     end subroutine curvature
  end interface

!-------------------------------------------------------------------------------
! 9. Vector rotation
!-------------------------------------------------------------------------------
  interface
     function rotate(X, A, Phi) result(R)
       use typesizes, only: wp => EightByteReal
         real(wp), dimension(3), intent(in) :: X
         real(wp), dimension(3), intent(in) :: A
         real(wp),               intent(in) :: phi
         real(wp), dimension(3)             :: R
       end function rotate
    end interface

!-------------------------------------------------------------------------------
! 10. Vector product
!-------------------------------------------------------------------------------
    interface
       function vector_product(X, Y) result(product)
         use typesizes,  only: wp => EightByteReal
         real(wp), dimension(3), intent(in) :: X
         real(wp), dimension(3), intent(in) :: Y
         real(wp), dimension(3)             :: product
       end function vector_product
    end interface

!-------------------------------------------------------------------------------
! 11. Vector angle
!-------------------------------------------------------------------------------
    interface
       function vector_angle(X, Y, A) result(angle)
         use typesizes,  only: wp => EightByteReal
         real(wp), dimension(3), intent(in) :: X
         real(wp), dimension(3), intent(in) :: Y
         real(wp), dimension(3), optional, intent(in) :: A
         real(wp)                           :: angle
       end function vector_angle
    end interface

!-------------------------------------------------------------------------------
! 12. Plane coordinates
!-------------------------------------------------------------------------------

    interface
       subroutine plane_coordinates(r_leo, r_gns, r_coc, roc, xleo, yleo, xgns, ygns, ax, ay)
         use typesizes,  only: wp => EightByteReal
         real(wp), dimension(:,:), intent(in) :: r_leo  ! LEO position vector
         real(wp), dimension(:,:), intent(in) :: r_gns  ! GPS position vector
         real(wp), dimension(:),   intent(in) :: r_coc  ! Centre curvature
         real(wp),                 intent(in) :: roc    ! Radius of curvature
         real(wp), dimension(:),  intent(out) :: xleo   ! X coordinates of LEO
         real(wp), dimension(:),  intent(out) :: yleo   ! Y coordinates of LEO
         real(wp), dimension(:),  intent(out) :: xgns   ! X coordinates of GPS
         real(wp), dimension(:),  intent(out) :: ygns   ! Y coordinates of GPS
         real(wp), dimension(:),  intent(out) :: ax     ! Occ X basis vector
         real(wp), dimension(:),  intent(out) :: ay     ! Occ Y basis vector
       end subroutine plane_coordinates
    end interface

end module coordinates
