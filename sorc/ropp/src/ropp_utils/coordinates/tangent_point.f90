! $Id: occ_point.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/tangent_point
!
! NAME
!    tangent_point - Determine tangent point coordinates
!
! SYNOPSIS
!    call tangent_point(r_leo, r_gns, lat_tp, lon_tp, azimuth_tp, r_coc, ip)
!
! DESCRIPTION
!    This subroutine calculates the latitude, longitude and azimuth of
!    each tangent point for an occultation, assuming the rays travel in straight
!    lines between the satellites.  If ip is provided, take account
!    of the bending of the ray when calculating the tangent point.
!
! INPUTS
!    r_leo         cartesian LEO position vector (relative to ECF frame)
!    r_gns         cartesian GPS position vector (relative to ECF frame)
!    r_coc         coordinates of center of curvature (relative to ECF frame)
!    ip            impact parameter at each point (optional)
!
! OUTPUT
!    lat_tp        tangent point latitude
!    lon_tp        tangent point longitude
!    azimuth_tp    GPS to LEO azimuth direction wrt true North at tangent (deg)
!
! SEE ALSO
!
! REFERENCES
!    Foelsche et al. AMT, 2011.
!    doi:10.5194/amt-4-189-2011
!
! AUTHOR
!    Met Office, Exeter, UK.
!    Any comments on this software should be given via the ROM SAF
!    Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!    (c) EUMETSAT. All rights reserved.
!    For further details please refer to the file COPYRIGHT
!    which you should have received as part of this distribution.
!
!****

subroutine tangent_point(r_leo, r_gns, lat_tp, lon_tp, azimuth_tp, r_coc, ip)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, not_this => tangent_point

  implicit none

  real(wp), dimension(:,:), intent(in)             :: r_leo   ! LEO position vector (ECF)
  real(wp), dimension(:,:), intent(in)             :: r_gns   ! GPS position vector (ECF)
  real(wp), dimension(:), intent(in)               :: r_coc   ! Centre of Curvature (ECF)
  real(wp), dimension(:), intent(out)              :: lat_tp  ! Tangent point latitude
  real(wp), dimension(:), intent(out)              :: lon_tp  ! Tangent point longitude
  real(wp), dimension(:), intent(out)              :: azimuth_tp  ! Tangent point azimuth
  real(wp), dimension(:), intent(in), optional     :: ip      ! Impact parameter 

  real(wp), allocatable, dimension(:,:)            :: perigee
  real(wp)                                         :: slta    ! Straight line tangent ht
  real(wp)                                         :: ro      ! Length of r_leo
  real(wp)                                         :: alpha   ! Angle r_leo and perigee
  real(wp)                                         :: theta   ! Cross section azimuth
  real(wp), dimension(3), parameter                :: pa = (/0,0,1/)   ! Polar axis

  real(wp), dimension(size(r_leo,2))               :: n ! Unit vect perp to occ plane
  real(wp), dimension(size(r_leo,2))               :: r_gns_new ! Vect to GNSS from CoC
  real(wp), dimension(size(r_leo,2))               :: r_leo_new ! Vect to LEO from CoC
  real(wp), dimension(size(r_leo,2))               :: r_gns_unit ! Unit vect to GNSS from CoC
  real(wp), dimension(size(r_leo,2))               :: r_leo_unit ! Unit vect to LEO from CoC
  real(wp), dimension(size(r_leo,2))               :: a_gns, a_leo ! GNSS and LEO IP vectors
  real(wp), dimension(size(r_leo,2))               :: rt ! Unit vector to TP from CoC

  real(wp)                                         :: r_gns_abs, r_leo_abs ! Distance to sats from CoC
  real(wp)                                         :: D_gns, D_leo ! Distances from satellites to defined IP locations

  integer:: i, npoints, nxyz

  npoints = SIZE(r_leo, 1)
  nxyz    = SIZE(r_leo, 2)

  ALLOCATE ( perigee(npoints, nxyz) )

! 1.2 Determine ray tangent points.  Include bending if ip is available
! ---------------------------------------------------------------------

  IF ( PRESENT(ip) ) THEN

    DO i=1, SIZE(r_leo,1)

      ! Following Foelsche et al. AMT 2011 notation
      r_leo_new = r_leo(i,:) - r_coc
      r_gns_new = r_gns(i,:) - r_coc

      n = vector_product(r_gns_new, r_leo_new)
      n = n / SQRT(SUM(n**2))
      r_leo_abs = SQRT(SUM(r_leo_new**2))
      r_leo_unit = r_leo_new / r_leo_abs
      r_gns_abs = SQRT(SUM(r_gns_new**2))
      r_gns_unit = r_gns_new / r_gns_abs

      D_leo = SQRT(r_leo_abs**2 - ip(i)**2)
      D_gns = SQRT(r_gns_abs**2 - ip(i)**2)

      a_leo = ip(i) * ((ip(i)*r_leo_unit - D_leo*vector_product(n,r_leo_unit))/r_leo_abs)
      a_gns = ip(i) * ((ip(i)*r_gns_unit + D_gns*vector_product(n,r_gns_unit))/r_gns_abs)

      rt = (a_gns + a_leo) / SQRT(SUM((a_gns + a_leo)**2))

      perigee(i,:) = ip(i) * rt   ! not quite the TP of ray, but a little above

      lat_tp(i) = 90.0_wp - rad2deg * acos(perigee(i,3)/sqrt(sum(perigee(i,:)**2)))

      lon_tp(i) = rad2deg * atan2(perigee(i,2), perigee(i,1))

    ENDDO

  ELSE

    DO i=1, SIZE(r_leo,1)

      ! Assuming straight lines (no bending)
      r_leo_new = r_leo(i,:) - r_coc
      r_gns_new = r_gns(i,:) - r_coc

      slta = impact_parameter(r_leo_new, r_gns_new)

      ro = Sqrt(Dot_Product(r_leo_new, r_leo_new))
      alpha = acos(slta/ro)

      perigee(i,:) = rotate(r_leo_new, vector_product(r_leo_new, r_gns_new), alpha) * (slta/ro)

      lat_tp(i) = 90.0_wp - rad2deg * acos(perigee(i,3)/sqrt(sum(perigee(i,:)**2)))

      lon_tp(i) = rad2deg * atan2(perigee(i,2), perigee(i,1))

    ENDDO

  ENDIF

! 1.3 Cross-section azimuth at tangent point
! ------------------------------------------

  DO i=1, SIZE(r_leo,1)

    theta = vector_angle( vector_product(perigee(i,:), pa), &
                          vector_product(r_gns(i,:)-r_coc, r_leo(i,:)-r_coc), &
                          -perigee(i,:) )

    azimuth_tp(i) = theta * rad2deg

    IF (azimuth_tp(i) < 0.0_wp ) azimuth_tp(i) = azimuth_tp(i) + 360.0_wp

  ENDDO

! 1.4 Clean up
! ------------

  DEALLOCATE ( perigee )

end subroutine tangent_point



