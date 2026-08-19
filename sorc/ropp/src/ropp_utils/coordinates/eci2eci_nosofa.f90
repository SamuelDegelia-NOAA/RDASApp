! $Id: eci2eci_nosofa.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/eci2eci_nosofa
!
! NAME
!    eci2eci - Transform Earth Centered Inertial (ECI) coordinates from the 
!              standard epoch J2000.0 to an intermediate frame, either the
!              classical ECI 'true-of-date' frame or the CIO-based frame.
!
! SYNOPSIS
!    r_eci_to = eci2eci(year, month, day, hour, minute, sec, dsec, r_eci_from, SOFA, CIO)
!
! DESCRIPTION
!    This subroutine transforms coordinates from the standard epoch J2000 to
!    an intermediate ECI frame at another epoch, by applying a 3x3 rotation
!    matrix R that primarily includes the effects of precession and nutation:
!
!       [ECI_to] = R*[ECI_from]
!
!    The frame [ECI_to] can be either the classical ECI 'true-of-date' frame
!    or the CIO-based frame (see IERS Technical Note 36 for more details).
!
!    Specify time in UT1 for highest accuracy. UTC gives a lower accuracy.
!
! INPUTS
!    year          occultation start year    (UTC)
!    month         occultation start month   (UTC)
!    day           occultation start day     (UTC)
!    hour          occultation start hour    (UTC)
!    minute        occultation start minute  (UTC)
!    sec           occultation start second  (UTC)
!    dsec          time since occultation start in seconds
!    r_eci_from    cartesian position vector (relative to an ECI frame)
!    SOFA          get rotation matrices from the SOFA library (default: false)
!    CIO           if SOFA then use CIO-based transformation (default: false)
!
! OUTPUT
!    r_eci_to      cartesian position vector (relative to another ECI frame)
!
! NOTES
!    This version is suitable for users who have not installed the SOFA library.
!    It may still be called with SOFA = .TRUE., but it will always use the 
!    Hoffman-Wellenhof formulae.  A warning message will be issued in this case.
!
! SEE ALSO
!    eci2ecf, ecf2eci
!
! REFERENCES
!    IERS Technical Note No. 36, 2010.
!    Hoffman-Wellenhof et al., Springer, 2008.
!    Astronomical Almanac, 1993.
!
! AUTHOR
!   Met Office, Exeter, UK  &  DMI, Copenhagen
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

function eci2eci_0d(year, month, day, hour, minute, sec, dsec, r_eci_from, SOFA, CIO) result(r_eci_to)

! 1.1 Declarations
! ----------------

  use typesizes,     only: wp => EightByteReal
  use datetimeprogs, only: CalToJul
  use coordinates,   only: pi
  use messages

  implicit none

  integer,  intent(inout)            :: year        ! occultation start year   (UTC)
  integer,  intent(inout)            :: month       ! occultation start month  (UTC)
  integer,  intent(inout)            :: day         ! occultation start day    (UTC)
  integer,  intent(in)               :: hour        ! occultation start hour   (UTC)
  integer,  intent(in)               :: minute      ! occultation start minute (UTC)
  integer,  intent(in)               :: sec         ! occultation start second (UTC)
  real(wp), intent(in)               :: dsec        ! time since occ start in seconds
  real(wp), dimension(3), intent(in) :: r_eci_from  ! position vector (ECI at standard epoch J2000.0)
  logical,  optional                 :: SOFA        ! get rotation matrices from the SOFA library (default: false)
  logical,  optional                 :: CIO         ! if SOFA then use CIO-based transformation (default: false)
  real(wp), dimension(3)             :: r_eci_to    ! position vector (ECI at the specified date and time)

  logical                            :: xSOFA = .false. ! local copy of SOFA
  logical                            :: xCIO  = .false. ! local copy of CIO

  integer,  dimension(8)             :: cdt         ! date/time array
  real(wp)                           :: jdf         ! Julian Day & fraction
  real(wp)                           :: tu          ! Julian centuries since J2000.0
  real(wp), parameter                :: jdf2000 = 2451545.0_wp ! Julian day of 0Z 01/01/2000

  real(wp), dimension(3,3)           :: Qt
  real(wp)                           :: zet, z, ups, szet, czet, sz, cz, sups, cups
  real(wp)                           :: dpsi, deps, epsa, X, Y, s
  real(wp), dimension(3,3)           :: RB, RP, RBP, RN, RBPN, RC
  
! 1.2 Time variables
! ------------------

  cdt = (/year, month, day, 0, 0, 0, 0, 0/)
  call CalToJul(cdt, jdf, 1)                  ! fractional Julian day of 00:00:00 UTC

! 1.3 Rotation matrix
!--------------------

  if (present(SOFA)) xSOFA = SOFA
  if (present(CIO))  xCIO  = CIO

  if ( xSOFA ) then
    call message (msg_warn, "eci2eci called with SOFA = .TRUE., " // &
                            "but SOFA library is unavailable. " // &
                            "Will use Hoffman-Wellenhof formulae instead.")
  endif

  !--- Formula for precession from Hoffman-Wellenhof et al., 2008.

  tu  = (jdf - jdf2000) / 36525.0_wp
  zet = (2306.2181_wp*tu + 0.30188_wp*tu**2 + 0.017998_wp*tu**3) * pi/648000.0_wp
  z   = (2306.2181_wp*tu + 1.09468_wp*tu**2 + 0.018203_wp*tu**3) * pi/648000.0_wp
  ups = (2004.3109_wp*tu - 0.42665_wp*tu**2 - 0.041833_wp*tu**3) * pi/648000.0_wp
  szet = sin(zet)
  czet = cos(zet)
  sz   = sin(z)
  cz   = cos(z)
  sups = sin(ups)
  cups = cos(ups)
  RP = reshape( (/ cz*cups*czet-sz*szet , -1.0_wp*cz*cups*szet-sz*czet , -1.0_wp*cz*sups , &
                   sz*cups*czet+cz*szet , -1.0_wp*sz*cups*szet+cz*czet , -1.0_wp*sz*sups , &
                   sups*czet            , -1.0_wp*sups*szet            ,         cups      /), &
                 shape(RP), order=(/2,1/) )
  Qt = RP

! 1.4 Frame rotation
! ------------------

  r_eci_to = matmul(Qt, r_eci_from)

end function eci2eci_0d
 

!-------------------------------------------------------------------------------
! 2. Double, array argument for position
!-------------------------------------------------------------------------------

function eci2eci_1d(year, month, day, hour, minute, sec, dsec, r_eci_from, SOFA, CIO) result(r_eci_to)

! 2.1 Declarations
! ----------------

  use typesizes,     only: wp => EightByteReal
  use datetimeprogs, only: CalToJul
  use coordinates,   only: pi
  use messages

  implicit none

  integer,  intent(inout)               :: year        ! occultation start year   (UTC)
  integer,  intent(inout)               :: month       ! occultation start month  (UTC)
  integer,  intent(inout)               :: day         ! occultation start day    (UTC)
  integer,  intent(in)                  :: hour        ! occultation start hour   (UTC)
  integer,  intent(in)                  :: minute      ! occultation start minute (UTC)
  integer,  intent(in)                  :: sec         ! occultation start second (UTC)
  real(wp), dimension(:), intent(in)    :: dsec        ! time since occ start in seconds
  real(wp), dimension(:,:), intent(in)  :: r_eci_from  ! position (ECI at standard epoch J2000.0)
  logical,  optional                    :: SOFA        ! get rotation matrices from the SOFA library (default: false)
  logical,  optional                    :: CIO         ! if SOFA then use CIO-based transformation (default: false)
  real(wp), dimension(size(r_eci_from,1),size(r_eci_from,2)) :: r_eci_to  ! position (ECI at the specified date and time)

  logical                               :: xSOFA, xCIO ! local copies of SOFA and CIO

  integer,  dimension(8)                :: cdt         ! date/time array
  real(wp)                              :: jdf         ! Julian Day & fraction
  real(wp)                              :: tu          ! Julian centuries since J2000.0
  real(wp), parameter                   :: jdf2000 = 2451545.0_wp ! Julian day of 0Z 01/01/2000

  real(wp), dimension(3,3)              :: Qt
  real(wp)                              :: zet, z, ups, szet, czet, sz, cz, sups, cups
  real(wp)                              :: dpsi, deps, epsa, X, Y, s
  real(wp), dimension(3,3)              :: RB, RP, RBP, RN, RBPN, RC

  integer                               :: i

! 1.2 Time variables
! ------------------

  cdt = (/year, month, day, 0, 0, 0, 0, 0/)
  call CalToJul(cdt, jdf, 1)                  ! fractional Julian day of 00:00:00 UTC

! 1.3 Rotation matrix
!--------------------

  xSOFA = .false.
  xCIO  = .false.
  if (present(SOFA)) xSOFA = SOFA
  if (present(CIO))  xCIO  = CIO

  if ( xSOFA ) then
    call message (msg_warn, "eci2eci called with SOFA = .TRUE., " // &
                            "but SOFA library is unavailable. " // &
                            "Will use Hoffman-Wellenhof formulae instead.")
  endif

  !--- Formula for precession from Hoffman-Wellenhof et al., 2008.

  tu  = (jdf - jdf2000) / 36525.0_wp
  zet = (2306.2181_wp*tu + 0.30188_wp*tu**2 + 0.017998_wp*tu**3) * pi/648000.0_wp
  z   = (2306.2181_wp*tu + 1.09468_wp*tu**2 + 0.018203_wp*tu**3) * pi/648000.0_wp
  ups = (2004.3109_wp*tu - 0.42665_wp*tu**2 - 0.041833_wp*tu**3) * pi/648000.0_wp
  szet = sin(zet)
  czet = cos(zet)
  sz   = sin(z)
  cz   = cos(z)
  sups = sin(ups)
  cups = cos(ups)
  RP = reshape( (/ cz*cups*czet-sz*szet , -1.0_wp*cz*cups*szet-sz*czet , -1.0_wp*cz*sups , &
                   sz*cups*czet+cz*szet , -1.0_wp*sz*cups*szet+cz*czet , -1.0_wp*sz*sups , &
                   sups*czet            , -1.0_wp*sups*szet            ,         cups      /), &
                 shape(RP), order=(/2,1/) )
  Qt = RP

! 1.4 Frame rotation
! ------------------

  do i=1,size(dsec)
    r_eci_to(i,:) = matmul(Qt, r_eci_from(i,:))
  enddo

end function eci2eci_1d



