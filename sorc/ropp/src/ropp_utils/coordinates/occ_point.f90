! $Id: occ_point.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/occ_point
!
! NAME
!    occ_point - Determine the occultation point
!
! SYNOPSIS
!    call occ_point(dtime, r_leo, v_leo, r_gns, v_gns, lat, lon,
!                   r_coc, roc, azimuth, undulation,
!                   leo_pos, leo_vel, gns_pos, gns_vel,
!                   time_offset, cfile, efile)
!
! DESCRIPTION
!    This subroutine calculates the lowest occultation perigree point projected
!    to the Earth's surface
!
! INPUTS
!    dtime         delta time vector
!    r_leo         cartesian LEO position vector (relative to ECF frame)
!    v_leo         cartesian LEO velocity vector (relative to ECI frame)
!    r_gns         cartesian GPS position vector (relative to ECF frame)
!    v_gns         cartesian GPS velocity vector (relative to ECI frame)
!    cfile         path to geoid potential coefficients file (optional)
!    efile         path to geoid potential corrections file (optional)
!
! OUTPUT
!    lat           Occultation point latitude
!    lon           Occultation point longitude
!    r_coc         Cartesian centre of curvature vector for occ point (ECF)
!    roc           Radius of curvature value for occultation point
!    azimuth       GPS to LEO azimuth direction wrt true North (deg)
!    undulation    Difference between ellipsoid (WGS-84) and EGM-96 geoid (m)
!    leo_pos       LEO position for georeferencing (ECF)
!    leo_vel       LEO velocity for georeferencing (ECI aligned to ECF)
!    gns_pos       GPS position for georeferencing (ECF)
!    gns_vel       GPS velocity for georeferencing (ECI aligned to ECF)
!    time_offset   Time offset for georeferencing
!
! SEE ALSO
!    tangent_point
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

subroutine occ_point(dtime, r_leo, v_leo, r_gns, v_gns, lat, lon, &
                     r_coc, roc, azimuth, undulation, &
                     leo_pos, leo_vel, gns_pos, gns_vel, &
                     time_offset, cfile,efile)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, not_this => occ_point
  use EarthMod, only: datum_hmsl

  implicit none

  real(wp), dimension(:),   intent(in)   :: dtime          ! Delta time vector
  real(wp), dimension(:,:), intent(in)   :: r_leo          ! LEO position vector (ECF)
  real(wp), dimension(:,:), intent(in)   :: v_leo          ! LEO velocity vector (ECI)
  real(wp), dimension(:,:), intent(in)   :: r_gns          ! GPS position vector (ECF)
  real(wp), dimension(:,:), intent(in)   :: v_gns          ! GPS velocity vector (ECI)
  real(wp), intent(out)                  :: lat            ! Occultation point latitude
  real(wp), intent(out)                  :: lon            ! Occultation point longitude
  real(wp), dimension(size(r_leo,2)), intent(out) :: r_coc ! Centre curvature
  real(wp), intent(out)                  :: roc            ! Radius curvature
  real(wp), intent(out)                  :: azimuth        ! Azimuth (deg)
  real(wp), intent(out)                  :: undulation     ! Undulation
  real(wp), dimension(3), intent(out)    :: leo_pos        ! LEO position
  real(wp), dimension(3), intent(out)    :: leo_vel        ! LEO velocity
  real(wp), dimension(3), intent(out)    :: gns_pos        ! GNSS position
  real(wp), dimension(3), intent(out)    :: gns_vel        ! GNSS velocity
  real(wp), intent(out)                  :: time_offset    ! Time offset
  character(len=*), optional, intent(in) :: cfile          ! Coefficient file path
  character(len=*), optional, intent(in) :: efile          ! Corrections file path

  real(wp), allocatable, dimension(:,:)  :: perigee
  real(wp)                               :: slta           ! Straight line tangent ht
  real(wp)                               :: ro             ! Length of r_leo
  real(wp)                               :: alpha          ! Angle r_leo and perigee
  real(wp)                               :: theta          ! Cross section azimuth
  real(wp), allocatable, dimension(:)    :: height_per
  real(wp), allocatable, dimension(:)    :: lat_per
  real(wp), allocatable, dimension(:)    :: lon_per
  real(wp), dimension(3), parameter      :: pa = (/0,0,1/) ! Polar axis
  real(wp), dimension(3)                 :: n              ! Surface normal
  real(wp), dimension(3)                 :: perigee_coc    ! Perigee relative to CoC

  integer:: i, iocc, npoints, nxyz

  npoints = size(r_leo, 1)
  nxyz    = size(r_leo, 2)

  allocate ( perigee(npoints, nxyz) )
  allocate ( height_per(npoints) )
  allocate ( lat_per(npoints) )
  allocate ( lon_per(npoints) )

  do i=1,size(r_leo,1)

! 1.2 Determine ray tangent points
! --------------------------------

    slta = impact_parameter(r_leo(i,:), r_gns(i,:))
    ro = Sqrt(Dot_Product(r_leo(i,:), r_leo(i,:)))
    alpha = acos(slta/ro)

    perigee(i,:) = rotate(r_leo(i,:), vector_product(r_leo(i,:), r_gns(i,:)), alpha) * (slta/ro)

  enddo

! 1.3 Convert cartesian to geodetic points
! ----------------------------------------

  call cart2geod(perigee, lat_per, lon_per, height_per)

! 1.4 Find the lowest perigee
! ---------------------------

  iocc = Sum(MinLoc(Abs(height_per)))

! 1.5 Define occultation point latitude and longitude
! ---------------------------------------------------

! 1.5.1: First approximation

  lat = lat_per(iocc)
  lon = lon_per(iocc)

! 1.5.2 Re-iterate using a rough approximation for the center of curvature
! to get correct latitude and longitude in all cases.  The correct latitude
! and longitude is the point on the GNSS-LEO line touching to the ellipsoidal
! surface.  The GNSS-LEO line is aproximated by the one with the iocc index
! (closest approach).  The radius of curvature is approximated by Re
! (equatorial radius).  These approximations are unimportant compared to the
! mistake of not doing this lat,lon correction.
! Stig Syndergaard, 2015-12-14.

  n(1) = Cos(deg2rad*lat)*Cos(deg2rad*lon)
  n(2) = Cos(deg2rad*lat)*Sin(deg2rad*lon)
  n(3) = Sin(deg2rad*lat)

  r_coc(:) = perigee(iocc,:) - Re*n(:)

  slta = impact_parameter(r_leo(iocc,:)-r_coc, r_gns(iocc,:)-r_coc)
  ro = Sqrt(Dot_Product(r_leo(iocc,:)-r_coc, r_leo(iocc,:)-r_coc))
  alpha = acos(slta/ro)

  perigee_coc(:) = rotate( r_leo(iocc,:)-r_coc, &
                           vector_product(r_leo(iocc,:)-r_coc, r_gns(iocc,:)-r_coc), &
                           alpha ) * (slta/ro)

  lat = 90.0_wp - rad2deg*acos(perigee_coc(3)/sqrt(sum(perigee_coc(:)**2)))
  lon = rad2deg*atan2(perigee_coc(2),perigee_coc(1))

! 1.6 Determine occultation point centre of curvature and radius
! --------------------------------------------------------------

  ! 1.6.1 Cross-section azimuth

  theta = vector_angle( vector_product(perigee_coc(:), pa), &
                        vector_product(r_gns(iocc,:)-r_coc, r_leo(iocc,:)-r_coc), &
                        -perigee_coc(:) )

  azimuth = theta * rad2deg

  if ( azimuth < 0.0_wp ) azimuth = azimuth + 360.0_wp

  ! 1.6.2 Compute curvature

  call curvature(lat, lon, theta, r_coc, roc)

! 1.7 Find undulation - height difference between local ellipsoid and geoid
! -------------------------------------------------------------------------

  if (present(cfile) .and. present(efile)) then
    call datum_hmsl("WGS84", (/lat, lon, 0.0_wp/), undulation, cfile, efile)
  else
    call datum_hmsl("WGS84", (/lat, lon, 0.0_wp/), undulation)
  endif
  if (undulation > -999999.000) undulation = -1.0_wp * undulation

! 1.8 Reference coordinates and time offset
! -----------------------------------------

  leo_pos = r_leo(iocc,:)
  leo_vel = v_leo(iocc,:)
  gns_pos = r_gns(iocc,:)
  gns_vel = v_gns(iocc,:)
  time_offset = dtime(iocc)

! 1.9 Clean up
! ------------

  deallocate ( lon_per )
  deallocate ( lat_per )
  deallocate ( height_per )
  deallocate ( perigee )

end subroutine occ_point



