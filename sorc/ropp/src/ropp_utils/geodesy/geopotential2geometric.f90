! $Id$

!****f* Geodesy/geopotential2geometric
!
! NAME
!    geopotential2geometric - Calculate geometric altitude 
!                             from geopotential height using expressions 
!                             derived from the Somigliana equation
!                   
! SYNOPSIS
!    h = geopotential2geometric(lat, geop)
! 
! DESCRIPTION
!    This subroutine calculates geometric altitude
!    from geopotential height 
!    [see http://mtp.jpl.nasa.gov/notes/altitude/altitude.html for details]
!
! INPUTS
!    lat           geographical latitude (in degrees north).
!    geopotential  geopotential height (in geopotential metres).
!
! OUTPUT
!    h             altitude above reference ellipsoid (in m).
!
! NOTES
!    Note Somigliana's Equation is defined for normal gravity on the surface 
!    of an ellipsoid while geopotential height is strictly defined relative to
!    an equi-potential surface for the geopotential (i.e. the geoid). 
!    These routines preserve the datum used for the input height. 
!      i.e.
! 
!        If INPUT: Z wrt ellipsoid -> OUTPUT: h wrt ellipsoid
!        Typically, geometric altitude is expressed relative to a reference 
!        geoid. To compute the geometric height relative to 
!        the EGM96 geoid, it is necessary for users to subtract the undulation
!        from the output geometric height in this case.
!
!        If INPUT: Z wrt geoid -> OUTPUT: h wrt geoid
!        In this case, the output is a geometric height relative to the 
!        geoid. Note Somagliana's equation is strictly based on computations
!        relative to a reference ellipsoid, but the error in assuming these 
!        apply for conversions wrt geoid is small.
!
! SEE ALSO
!    R_eff
!    gravity
!    geopotential2geometric
!
! REFERENCES
!    Mahoney, M.J., A discussion of various measures of altitudes,
!       URL: http://mtp.jpl.nasa.gov/notes/altitude/altitude.html,
!       cited 29/06/2004.
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

function geopotential2geometric_00d(lat, geop) result(h)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp), intent(in) :: lat                    ! latitude
  real(wp), intent(in) :: geop                   ! geopotential height
  real(wp)             :: h                      ! geometric height

  real(wp), parameter  :: g_wmo = 9.80665_wp     ! reference gravity
  real(wp)             :: g_lat                  ! gravity at latitude
  real(wp)             :: R_lat                  ! earth radius

! 1.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 1.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 1.4 Calculate altitude
! ----------------------

  IF (geop > -9999.0_wp) THEN
    h    = R_lat * geop / (g_lat / g_wmo * R_lat - geop)
  ELSE
    h    = -99999000.0_wp
  END IF

end function geopotential2geometric_00d


!-------------------------------------------------------------------------------
! 2. Double, array argument for altitude
!-------------------------------------------------------------------------------

function geopotential2geometric_01d(lat, geop) result(h)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp),               intent(in) :: lat          ! latitude
  real(wp), dimension(:), intent(in) :: geop         ! geopotential height
  real(wp), dimension(size(geop))    :: h            ! altitude

  real(wp),            parameter  :: g_wmo = 9.80665_wp  ! reference gravity
  real(wp)                        :: g_lat               ! gravity at latitude
  real(wp)                        :: R_lat               ! earth radius

! 2.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 2.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 2.4 Calculate geopotential
! --------------------------

  WHERE (geop > -9999.0_wp)
    h    = R_lat * geop / (g_lat / g_wmo * R_lat - geop)
  ELSEWHERE
    h    = -99999000.0_wp
  END WHERE

end function geopotential2geometric_01d


!-------------------------------------------------------------------------------
! 3. Double, array argument for latitude
!-------------------------------------------------------------------------------

function geopotential2geometric_10d(lat, geop) result(h)

! 3.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp), dimension(:), intent(in) :: lat               ! latitude
  real(wp),               intent(in) :: geop              ! geopotential height
  real(wp), dimension(size(lat))     :: h                 ! geometric height

  real(wp),               parameter :: g_wmo = 9.80665_wp ! reference gravity
  real(wp), dimension(size(lat))    :: g_lat              ! gravity at latitude
  real(wp), dimension(size(lat))    :: R_lat              ! earth radius


! 3.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 3.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 3.4 Calculate geopotential
! --------------------------

  IF (geop > -9999.0_wp) THEN
    h    = R_lat * geop / (g_lat / g_wmo * R_lat - geop)
  ELSE
    h    = -99999000.0_wp
  END IF

end function geopotential2geometric_10d


!-------------------------------------------------------------------------------
! 4. Double, array arguments for latitude and altitude
!-------------------------------------------------------------------------------

function geopotential2geometric_11d(lat, geop) result(h)

! 3.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp), dimension(:),         intent(in) :: lat      ! latitude
  real(wp), dimension(:),         intent(in) :: geop     ! geopotential height
  real(wp), dimension(size(lat), size(geop)) :: h        ! geometric height

  real(wp),                       parameter  :: g_wmo = 9.80665_wp
  real(wp), dimension(size(lat))             :: g_lat    ! gravity at latitude
  real(wp), dimension(size(lat))             :: R_lat    ! earth radius

  integer                                    :: l

! 3.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 3.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 3.4 Calculate geopotential
! --------------------------

  ! This seems odd. Why would anyone want to get a two-dimensional height array out of this?
  ! It does not seem to be used anywhere in ROPP.
  do l = 1, size(h)
     h(:,l) = R_lat * geop(l) / (g_lat / g_wmo * R_lat - geop(l))
  enddo

end function geopotential2geometric_11d
