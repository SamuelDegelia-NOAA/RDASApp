! $Id$

!****f* Geodesy/geometric2geopotential
!
! NAME
!    geometric2geopotential - Calculate geopotential height as function of 
!                             altitude using the expressions derived from 
!                             the Somigliana equation
!
! SYNOPSIS
!    geop = geometric2geopotential(lat, h)
! 
! DESCRIPTION
!    This subroutine calculates geopotential height as
!    function of altitude.
!
! INPUTS
!    lat           geographical latitude (in degree north).
!    h             altitude (in m).
!
! OUTPUT
!    geopotential  geopotential height (in geopotential metres).
!
! NOTES
!    Note Somigliana's Equation is defined for normal gravity on the surface 
!    of an ellipsoid while geopotential height is strictly defined relative to
!    an equi-potential surface for the geopotential (i.e. the geoid). 
!    These routines preserve the datum used for the input height. 
!      i.e.
! 
!        If INPUT: h wrt ellipsoid -> OUTPUT: Z wrt ellipsoid
!        The output is not strictly a geopotential height (relative to an equi-
!        potential surface). To compute the geopotential height relative to 
!        the EGM96 geoid, it is necessary for users to subtract the undulation
!        from the output 'geopotential height'.
!
!        If INPUT: h wrt geoid -> OUTPUT: Z wrt geoid
!        In this case, the output is a geopotential height relative to the 
!        geoid. Note Somagliana's equation is strictly based on computations
!        relative to a reference ellipsoid, but the error in assuming these 
!        apply for conversions wrt geoid is small.
!
! SEE ALSO
!    R_eff
!    gravity_msl
!    geopotential2geometric
!
! REFERENCES
!    Mahoney, M.J., A discussion of various measures of altitudes,
!       URL: http://mtp.jpl.nasa.gov/notes/altitude/altitude.html,
!       cited 29/06/2004.
!
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

function geometric2geopotential_00d(lat, h) result(geop)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp), intent(in) :: lat                  ! latitude
  real(wp), intent(in) :: h                    ! geometric height
  real(wp)             :: geop                 ! geopotential height

  real(wp), parameter  :: g_wmo = 9.80665_wp   ! reference gravity
  real(wp)             :: g_lat                ! gravity at latitude
  real(wp)             :: R_lat                ! earth radius at latitude

! 1.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 1.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 1.3 Calculate geopotential height
! ---------------------------------

  IF (h > -9999.0_wp) THEN
    geop = (g_lat / g_wmo) * ( R_lat * h / (R_lat + h))
  ELSE
    geop = -99999000.0_wp
  END IF

end function geometric2geopotential_00d


!-------------------------------------------------------------------------------
! 2. Double, array argument for altitude
!-------------------------------------------------------------------------------

function geometric2geopotential_01d(lat, h) result(geop)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp),               intent(in) :: lat          ! latitude
  real(wp), dimension(:), intent(in) :: h            ! geometric height
  real(wp), dimension(size(h))       :: geop         ! geopotential height

  real(wp),               parameter  :: g_wmo = 9.80665_wp
  real(wp)                           :: g_lat        ! gravity at latitude
  real(wp)                           :: R_lat        ! earth radius at latitude

! 2.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 2.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 2.4 Calculate geopotential height
! ---------------------------------

  WHERE (h > -9999.0_wp)
    geop = (g_lat / g_wmo) * ( R_lat * h / (R_lat + h))
  ELSEWHERE
    geop = -99999000.0_wp
  END WHERE

end function geometric2geopotential_01d


!-------------------------------------------------------------------------------
! 3. Double, array argument for latitude
!-------------------------------------------------------------------------------

function geometric2geopotential_10d(lat, h) result(geop)

! 3.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp), dimension(:), intent(in) :: lat      ! latitude
  real(wp),               intent(in) :: h        ! geometric height
  real(wp), dimension(size(lat))     :: geop     ! geopotential height

  real(wp),               parameter  :: g_wmo = 9.80665_wp
  real(wp), dimension(size(lat))     :: g_lat    ! gravity at latitude
  real(wp), dimension(size(lat))     :: R_lat    ! earth radius

! 3.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 3.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 3.4 Calculate geopotential height
! ---------------------------------

  IF (h > -9999.0_wp) THEN
    geop = (g_lat / g_wmo) * ( R_lat * h / (R_lat + h))
  ELSE
    geop = -99999000.0_wp
  END IF

end function geometric2geopotential_10d


!-------------------------------------------------------------------------------
! 4. Double, array arguments for latitude and altitude
!-------------------------------------------------------------------------------

function geometric2geopotential_11d(lat, h) result(geop)

! 3.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use geodesy,   only: gravity, R_eff

  implicit none

  real(wp), dimension(:), intent(in)      :: lat        ! latitude
  real(wp), dimension(:), intent(in)      :: h          ! geometric height
  real(wp), dimension(size(lat), size(h)) :: geop       ! geopotential height

  real(wp),               parameter  :: g_wmo = 9.80665_wp  ! reference g
  real(wp), dimension(size(lat))     :: g_lat           ! gravity at latitude
  real(wp), dimension(size(lat))     :: R_lat           ! earth radius

  integer                            :: l

! 3.2 Normal gravity
! ------------------

  g_lat = gravity(lat)

! 3.3 Effective Earth radius
! --------------------------

  R_lat = R_eff(lat)

! 3.4 Calculate geopotential height
! ---------------------------------

  ! This seems odd. Why would anyone want to get a two-dimensional height array out of this?
  ! It does not seem to be used anywhere in ROPP.
  do l = 1, size(h)
     geop(:,l) = (g_lat / g_wmo) * ( R_lat * h(l) / (R_lat + h(l)))
  enddo

end function geometric2geopotential_11d
