! $Id: plane_coordinates.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/plane_coordinates
!
! NAME
!    plane_coordinates - Calculation of occultation plane coordinates of GSP
!                        and LEO
!
! SYNOPSIS
!    call plane_coordinates(r_leo, r_gns, r_coc, roc, xleo, yleo, xgns, ygns, ax, ay)
! 
! DESCRIPTION
!    This subroutine calculates the GPS and LEO coordinates in GPS-O-LEO plane
!    transformed to unmoving GPS.
!
! INPUTS
!    r_leo         cartesian LEO position vector (m)
!    r_gns         cartesian GPS position vector (m)
!    r_coc         Cartesian centre of curvature vector (m) 
!    roc           Local radius of curvature (m)
!
! OUTPUT
!    xleo          X coordinates of LEO (m)
!    yleo          Y coordinates of LEO (m)
!    xgns          X coordinates of GPS (m)
!    ygns          Y coordinates of GPS (m)
!    ax            Occultation plane X basis vector
!    ay            Occultation plane Y basis vector
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

subroutine plane_coordinates(r_leo, r_gns, r_coc, roc, xleo, yleo, xgns, ygns, ax, ay)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, not_this => plane_coordinates

  implicit none

  real(wp), dimension(:,:), intent(in) :: r_leo  ! LEO position vector (ECF)
  real(wp), dimension(:,:), intent(in) :: r_gns  ! GPS position vector (ECF)
  real(wp), dimension(:),   intent(in) :: r_coc  ! Centre curvature
  real(wp),                 intent(in) :: roc    ! Local radius of curvature
  real(wp), dimension(:),  intent(out) :: xleo   ! X coordinates of LEO
  real(wp), dimension(:),  intent(out) :: yleo   ! Y coordinates of LEO
  real(wp), dimension(:),  intent(out) :: xgns   ! X coordinates of GPS
  real(wp), dimension(:),  intent(out) :: ygns   ! Y coordinates of GPS
  real(wp), dimension(:),  intent(out) :: ax     ! Occ plane X basis vector
  real(wp), dimension(:),  intent(out) :: ay     ! Occ plane Y basis vector

  real(wp), ALLOCATABLE, dimension(:,:) :: perigee
  real(wp)                             :: impact  ! Impact parameter
  real(wp)                             :: ro      ! Length of r_leo
  real(wp)                             :: alpha   ! Angle r_leo and perigee
  real(wp), ALLOCATABLE, dimension(:)  :: height_per
  real(wp), ALLOCATABLE, dimension(:)  :: lat_per
  real(wp), ALLOCATABLE, dimension(:)  :: lon_per
  
  real(wp), ALLOCATABLE, dimension(:,:) :: axs   ! Current X basis 
  real(wp), ALLOCATABLE, dimension(:,:) :: ays   ! Current Y basis 
  real(wp), dimension(3) :: ng, nl   ! orthonormalized (RGPS, RLEO) basis
  real(wp), dimension(3) :: vec
  real(wp)               :: ct, st   ! Cos(r_leo^AY), Sin(r_leo^AY)
  real(wp)               :: func     ! Coordinate invariant
  integer                :: i, iocc, npoints, nxyz

  npoints = size(r_leo,1)
  nxyz    = size(r_leo,2)

  ALLOCATE(perigee(npoints, nxyz))
  ALLOCATE(axs(npoints, nxyz))
  ALLOCATE(ays(npoints, nxyz))
  ALLOCATE(height_per(npoints))
  ALLOCATE(lat_per(npoints))
  ALLOCATE(lon_per(npoints))

  do i=1,npoints

! 1.2 Determine ray perigrees
! ---------------------------
     
     impact = impact_parameter(r_leo(i,:), r_gns(i,:))
     ro = Sqrt(Dot_Product(r_leo(i,:), r_leo(i,:)))   
     alpha = acos(impact/ro)
     
     perigee(i,:) = rotate(r_leo(i,:), vector_product(r_leo(i,:), r_gns(i,:)), alpha) * (impact/ro)

  enddo
  
! 1.3 Convert cartesian to geodetic points
! ---------------------------------------- 

  call cart2geod(perigee, lat_per, lon_per, height_per)

! 1.4 Find the lowest perigee
! ---------------------------
 
  iocc = Sum(MinLoc(Abs(height_per)))

! 1.5 Transform to coordinates in occultation plane
! ---------------------------------------------------

  do i=1,npoints
     
     ng(:) = (r_gns(i,:)-r_coc(:))/Sqrt(Sum((r_gns(i,:)-r_coc(:))**2)) 
     vec(:) = r_leo(i,:)-r_coc(:)-(Dot_Product(r_leo(i,:)-r_coc(:),ng)*ng(:))
     nl(:) = vec(:)/Sqrt(Sum(vec(:)**2))
     
     ct = roc / Sqrt(Sum((r_gns(i,:)-r_coc(:))**2)) 
     st = Sqrt(1.0_wp - ct**2)

     axs(i,:) = ct*nl(:) - st*ng(:)
     ays(i,:) = st*nl(:) + ct*ng(:)

     xleo(i) = Dot_Product(axs(i,:), r_leo(i,:)-r_coc(:))
     yleo(i) = Dot_Product(ays(i,:), r_leo(i,:)-r_coc(:))
     xgns(i) = Dot_Product(axs(i,:), r_gns(i,:)-r_coc(:))
     ygns(i) = Dot_Product(ays(i,:), r_gns(i,:)-r_coc(:))

  enddo
  
  ax(:) = axs(iocc,:)
  ay(:) = ays(iocc,:)

! 1.6 Transform to unmoving GPS frame
! --------------------------------------------------------------

  do i=1,npoints
     func = xleo(i)*xgns(i)/(xleo(i)-xgns(i))
     xgns(i) = xgns(iocc)
     xleo(i) = func*xgns(i)/(func - xgns(i))
  enddo
  
  DEALLOCATE(perigee)
  DEALLOCATE(axs)
  DEALLOCATE(ays)
  DEALLOCATE(height_per)
  DEALLOCATE(lat_per)
  DEALLOCATE(lon_per)
    

end subroutine plane_coordinates



