! $Id: ecf2eci_nosofa.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/ecf2eci_nosofa
!
! NAME
!    ecf2eci - Transform coordinates from the ECF frame to an 
!              intermediate ECI frame.
!
! SYNOPSIS
!    r_eci = ecf2eci(year, month, day, hour, minute, sec, dsec, r_ecf, SOFA, CIO)
! 
! DESCRIPTION
!    This subroutine calculates coordinates relative to an intermediate
!    ECI frame, given coordinates relative to the ECF frame.
!    The output ECI frame can be either the classical ECI 'true-of-date' frame
!    or the CIO-based frame (see IERS Technical Note 36 for more details).
!
!    Specify time in UT1 for highest accuracy. UTC gives a lower accuracy.
!
! INPUTS
!    year          occultation start year   (UT)
!    month         occultation start month  (UT)
!    day           occultation start day    (UT)
!    hour          occultation start hour   (UT)
!    minute        occultation start minute (UT)
!    sec           occultation start second (UT)
!    dsec          time since occultation start in seconds
!    r_ecf         cartesian position vector (relative to ECF frame)
!    SOFA          get rotation matrices from the SOFA library (default: false)
!    CIO           if SOFA then use CIO-based transformation (default: false)
!
! OUTPUT
!    r_eci         cartesian position vector (relative to an intermediate ECI frame)  
!
! NOTES
!    This version is suitable for users who have not installed the SOFA library.
!    It may still be called with SOFA = .TRUE., but it will always use the 
!    Astronomical Almanac formulae.  A warning message will be issued in this case.
!
! SEE ALSO
!    eci2eci, eci2ecf
!
! REFERENCES
!    IERS Technical Note No. 36, 2010.
!    Astronomical Almanac, 1993.
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

function ecf2eci_0d(year, month, day, hour, minute, sec, dsec, r_ecf, SOFA, CIO) result(r_eci)

! 1.1 Declarations
! ----------------

  use typesizes,     only: wp => EightByteReal
  use datetimeprogs, only: CalToJul
  use coordinates,   only: rotate, pi
  use messages

  implicit none

  integer,  intent(inout)            :: year     ! occultation start year   (UT)
  integer,  intent(inout)            :: month    ! occultation start month  (UT)
  integer,  intent(inout)            :: day      ! occultation start day    (UT)
  integer,  intent(in)               :: hour     ! occultation start hour   (UT)
  integer,  intent(in)               :: minute   ! occultation start minute (UT)
  integer,  intent(in)               :: sec      ! occultation start second (UT)
  real(wp), intent(in)               :: dsec     ! time since occultation start in seconds
  real(wp), dimension(3), intent(in) :: r_ecf    ! position vector (ECF)
  logical,  optional                 :: SOFA     ! get rotation matrices from the SOFA library (default: false)
  logical,  optional                 :: CIO      ! if SOFA then use CIO-based transformation (default: false)
  real(wp), dimension(3)             :: r_eci    ! position vector (ECI)

  logical                            :: xSOFA = .false. ! local copy of SOFA
  logical                            :: xCIO  = .false. ! local copy of CIO

  integer,  dimension(8)             :: cdt      ! date/time array
  real(wp)                           :: jdf      ! Julian Day & fraction
  real(wp)                           :: tu       ! Julian centuries since J2000.0
  real(wp), parameter                :: jdf2000 = 2451545.0_wp ! Julian day of 0Z 01/01/2000
  real(wp)                           :: phi
  real(wp)                           :: utc
  real(wp), dimension(3), parameter  :: pa = (/0.0_wp, 0.0_wp, 1.0_wp/)  ! Polar axis
  real(wp)                           :: gmst0, gmst

! 1.2 Time variables
! ------------------

  cdt = (/year, month, day, 0, 0, 0, 0, 0/)
  call CalToJul(cdt, jdf, 1)                  ! fractional Julian day at 00:00:00 UTC
  utc = hour*3600.0_wp + minute*60.0_wp + &   ! seconds since 00:00:00 UTC
        sec*1.0_wp + dsec

! 1.3 Rotation angle, from ECF to intermediate ECI frame
! ------------------------------------------------------

  if (present(SOFA)) xSOFA = SOFA
  if (present(CIO))  xCIO  = CIO

  if ( xSOFA ) then
    call message (msg_warn, "ecf2eci called with SOFA = .TRUE., " // &
                            "but SOFA library is unavailable. " // &
                            "Will use Astronomical Almanac formulae instead.")
  endif

  !--- Simplified earth rotation

  tu = (jdf - jdf2000) / 36525.0_wp
  gmst0 = 24110.54841_wp + 8640184.812866_wp*tu + 0.093104_wp*tu**2 - 6.2e-6_wp*tu**3
  gmst  = modulo(gmst0 + utc*1.0027379093_wp, 86400.0_wp)
  phi = gmst*2.0_wp*pi/86400.0_wp

! 1.4 Frame rotation
! ------------------

  r_eci = rotate(r_ecf, pa, phi)     !! N.B. ECF -> ECI, positive phi

end function ecf2eci_0d
 

!-------------------------------------------------------------------------------
! 2. Double, array argument for position
!-------------------------------------------------------------------------------

function ecf2eci_1d(year, month, day, hour, minute, sec, dsec, r_ecf, SOFA, CIO) result(r_eci)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: ecf2eci

  implicit none

  integer,  intent(inout)              :: year    ! occultation start year   (UT)
  integer,  intent(inout)              :: month   ! occultation start month  (UT)
  integer,  intent(inout)              :: day     ! occultation start day    (UT)
  integer,  intent(in)                 :: hour    ! occultation start hour   (UT)
  integer,  intent(in)                 :: minute  ! occultation start minute (UT)
  integer,  intent(in)                 :: sec     ! occultation start second (UT)
  real(wp), dimension(:),   intent(in) :: dsec    ! time since occultation start in seconds
  real(wp), dimension(:,:), intent(in) :: r_ecf   ! position (ECF)
  logical,  optional                   :: SOFA    ! get rotation matrices from the SOFA library (default: false)
  logical,  optional                   :: CIO     ! if SOFA then use CIO-based transformation (default: false)
  real(wp), dimension(size(r_ecf,1),size(r_ecf,2)) :: r_eci   ! position (ECI)

  integer :: i

! 2.2 Frame transformation
! ------------------------

  do i=1,size(dsec)
    r_eci(i,:) =  ecf2eci(year, month, day, hour, minute, sec, dsec(i), r_ecf(i,:), SOFA, CIO)
  enddo

end function ecf2eci_1d



