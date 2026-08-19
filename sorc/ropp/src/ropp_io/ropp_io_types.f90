! $Id$

MODULE ropp_io_types

!****m* Modules/ropp_io_types *
!
! NAME
!    ropp_io_types - Type declarations for the ROPP IO library.
!
! SYNOPSIS
!    use ropp_io
!
! DESCRIPTION
!    This Fortran module supports the ROPP input / output library and provides
!    derived data types used by the ropp_io library. It must be loaded for all
!    applications using the ropp_io library / package. Note that loading the
!    ropp_io module includes the ropp_io_types module
!
! SEE ALSO
!    ropp_io_types
!    ropp_io_init
!    ropp_io_free
!    ropp_io_read
!    ropp_io_write
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
! 1. Other modules
!-------------------------------------------------------------------------------

  USE typesizes, wp => EightByteReal
  USE ropp_utils, ONLY: ropp_MDFV, ropp_ZERO, ropp_MIFV

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

!****ip* Initialisation/ThisFmtVer
!
! NAME
!    ThisFmtVer - Parameter describing the current implemented ROPP
!                 data format version (NOT the current software
!                 version of ROPP).
!
! NOTES
!    This parameter is an internal parameter to the ropp_io library and not
!    intended to be used by users. Update the value only if the (text) file
!    format specification changes (see Ref).
!
! REFERENCE
!    ROPP Interface File Format. SAF/GRAS/METO/FMT/ROPP/001
!
! SOURCE
!
  CHARACTER (len=*), PARAMETER :: ThisFmtVer = "ROPP I/O V1.1"
!
!****

!****ip* Initialisation/ropp_io_ncopensep
!
! NAME
!    ropp_io_ncopensep - Internal global variable keeping track if netCDF data
!                           files have been opened separately (as opposed to
!                           be opened from ropp_io_read() or ropp_io_write()).
!
! NOTES
!    This variable is an internal variable to the ropp_io library and not
!    intended to be used by users.
!
! SOURCE
!
  LOGICAL                      :: ropp_io_ncopensep = .FALSE.
!
!****

!-------------------------------------------------------------------------------
! 3. PCD flags
!-------------------------------------------------------------------------------

!****p* Initialisation/PCD_parametres
!
! NAME
!    PCD parametres - Parametres for setting and testing the Product Confidence
!                     Data (PCD) element of the ROprof structure.
!
! SOURCE
!
  INTEGER, PARAMETER :: PCD_summary      =  0  ! Nominal / non-nominal quality
  INTEGER, PARAMETER :: PCD_offline      =  1  ! NRT / offline product
  INTEGER, PARAMETER :: PCD_rising       =  2  ! Setting / rising occultation
  INTEGER, PARAMETER :: PCD_phase        =  3  ! Phase processing      nominal / non-nominal
  INTEGER, PARAMETER :: PCD_bangle       =  4  ! Bending angle proc.   nominal / non-nominal
  INTEGER, PARAMETER :: PCD_refrac       =  5  ! Refractivity proc.    nominal / non-nominal
  INTEGER, PARAMETER :: PCD_met          =  6  ! Meteorological. proc. nominal / non-nominal
  INTEGER, PARAMETER :: PCD_open_loop    =  7  ! Open Loop data used   no / yes
  INTEGER, PARAMETER :: PCD_reflections  =  8  ! Surface reflections detected no / yes
  INTEGER, PARAMETER :: PCD_l2_signal    =  9  ! L2 GPS signal used    L2P / L2C
  INTEGER, PARAMETER :: PCD_reserved_11  = 10  ! Reserved
  INTEGER, PARAMETER :: PCD_reserved_12  = 11  ! Reserved
  INTEGER, PARAMETER :: PCD_reserved_13  = 12  ! Reserved
  INTEGER, PARAMETER :: PCD_bg           = 13  ! Background profile nominal / non-nominal
  INTEGER, PARAMETER :: PCD_occultation  = 14  ! Occultation / background profile
  INTEGER, PARAMETER :: PCD_missing      = 15  ! PCD missing
!
!****

!-------------------------------------------------------------------------------
! 4. Date and time
!-------------------------------------------------------------------------------

!****d* Datatypes/DT7units
!
! NAME
!    DT7units - A sub-structure of the DT7type structure, defining units.
!
! SOURCE
!
  TYPE DT7units
    CHARACTER(len = 64) :: year     = "years"
    CHARACTER(len = 64) :: month    = "months"
    CHARACTER(len = 64) :: day      = "days"
    CHARACTER(len = 64) :: hour     = "hours"
    CHARACTER(len = 64) :: minute   = "minutes"
    CHARACTER(len = 64) :: second   = "seconds"
    CHARACTER(len = 64) :: msec     = "milliseconds"
  END TYPE DT7units
!
!****

!****d* Datatypes/DT7range
!
! NAME
!    DT7range - A sub-structure of the DT7type structure, setting valid ranges.
!
! SOURCE
!
  TYPE DT7range
    INTEGER, DIMENSION(2) :: year   = (/1995, 2099/)
    INTEGER, DIMENSION(2) :: month  = (/  01,   12/)
    INTEGER, DIMENSION(2) :: day    = (/  01,   31/)
    INTEGER, DIMENSION(2) :: hour   = (/  00,   23/)
    INTEGER, DIMENSION(2) :: minute = (/  00,   59/)
    INTEGER, DIMENSION(2) :: second = (/  00,   59/)
    INTEGER, DIMENSION(2) :: msec   = (/  00,  999/)
  END TYPE DT7range
!
!****

!****d* Datatypes/DT7type
!
! NAME
!    DT7types - A sub-structure of the ROprofs structure for date information.
!
! SOURCE
!
  TYPE DT7type
    INTEGER        :: year   = 9999
    INTEGER        :: month  =   99
    INTEGER        :: day    =   99
    INTEGER        :: hour   =   99
    INTEGER        :: minute =   99
    INTEGER        :: second =   99
    INTEGER        :: msec   = 9999
    TYPE(DT7units) :: units
    TYPE(DT7range) :: range
  END TYPE DT7type
!
!****

!-------------------------------------------------------------------------------
! 5. Georeferencing of the profile
!-------------------------------------------------------------------------------

!****d* Datatypes/GEOunits
!
! NAME
!    GEOunits - A sub-structure of the GEOtype structure, setting units.
!
! SOURCE
!
  TYPE GEOunits
    CHARACTER(len = 64) :: time_offset = "seconds"
    CHARACTER(len = 64) :: lat         = "degrees_north"
    CHARACTER(len = 64) :: lon         = "degrees_east"
    CHARACTER(len = 64) :: roc         = "metres"
    CHARACTER(len = 64) :: r_coc       = "metres"
    CHARACTER(len = 64) :: azimuth     = "degrees_T"
    CHARACTER(len = 64) :: undulation  = "metres"
    CHARACTER(len = 64) :: r_leo       = "metres"
    CHARACTER(len = 64) :: v_leo       = "metres / seconds"
    CHARACTER(len = 64) :: r_gns       = "metres"
    CHARACTER(len = 64) :: v_gns       = "metres / seconds"
  END TYPE GEOunits
!
!****

!****d* Datatypes/GEOrange
!
! NAME
!    GEOrange - A sub-structure of the GEOtype structure, setting valid ranges.
!
! SOURCE
!
  TYPE GEOrange
    REAL(wp), DIMENSION(2) :: time_offset = (/ -3600.0_wp,  3600.0_wp /)
    REAL(wp), DIMENSION(2) :: lat         = (/   -90.0_wp,    90.0_wp /)
    REAL(wp), DIMENSION(2) :: lon         = (/  -180.0_wp,   180.0_wp /)
    REAL(wp), DIMENSION(2) :: roc         = (/   6.2e6_wp,   6.6e6_wp /)
    REAL(wp), DIMENSION(2) :: r_coc       = (/  -5.0e4_wp,   5.0e4_wp /)
    REAL(wp), DIMENSION(2) :: azimuth     = (/     0.0_wp,   360.0_wp /)
    REAL(wp), DIMENSION(2) :: undulation  = (/  -150.0_wp,   150.0_wp /)
    REAL(wp), DIMENSION(2) :: r_leo       = (/ -10.0e6_wp,  10.0e6_wp /)
    REAL(wp), DIMENSION(2) :: v_leo       = (/  -1.0e4_wp,   1.0e4_wp /)
    REAL(wp), DIMENSION(2) :: r_gns       = (/ -43.0e6_wp,  43.0e6_wp /)
    REAL(wp), DIMENSION(2) :: v_gns       = (/  -1.0e4_wp,   1.0e4_wp /)
  END TYPE GEOrange
!
!****

!****d* Datatypes/GEOref
!
! NAME
!    GEOref - A sub-structure of the GEOtype structure, defining the reference
!             frame for the centre of curvature.
!
! SOURCE
!
  TYPE GEOref
    CHARACTER(len = 3)     :: r_coc = 'ECF'
  END TYPE GEOref
!
!****

!****d* Datatypes/PODref
!
! NAME
!    PODref - A sub-structure of the GEOtype structure, defining the reference
!             POD of a satellite.
!
! SOURCE
!
  TYPE PODref
    REAL(wp), DIMENSION(3) :: pos       = ropp_MDFV ! Reference position (m)
    REAL(wp), DIMENSION(3) :: vel       = ropp_MDFV ! Reference velocity (m/s)
    CHARACTER(len = 3)     :: pos_frame = 'ECF'     ! Reference frame for position
    CHARACTER(len = 3)     :: vel_frame = 'ECI'     ! Reference frame for velocity
  END TYPE PODref
!
!****

!****d* Datatypes/GEOtype
!
! NAME
!    GEOtype - A sub-structure of the ROprof structure, describing the georeferencing
!              a given profile.
!
! SOURCE
!
  TYPE GEOtype
    REAL(wp)               :: time_offset = ropp_MDFV ! Time since start (s)
    REAL(wp)               :: lat         = ropp_MDFV ! Latitude  (deg)
    REAL(wp)               :: lon         = ropp_MDFV ! Longitude (deg)
    REAL(wp)               :: roc         = ropp_MDFV ! RoC value (m)
    REAL(wp), DIMENSION(3) :: r_coc       = ropp_MDFV ! RoC offset X,Y,Z vector (m)
    REAL(wp)               :: azimuth     = ropp_MDFV ! GNSS->LEO line of sight angle (degT)
    REAL(wp)               :: undulation  = ropp_MDFV ! Geoid undulation (EGM96-WGS84) (m)
    TYPE(GEOunits)         :: units
    TYPE(GEOrange)         :: range
    TYPE(GEOref)           :: reference_frame
    TYPE(PODref)           :: leo_pod
    TYPE(PODref)           :: gns_pod
  END TYPE GEOtype
!
!****

!-------------------------------------------------------------------------------
! 6. Signal characteristics
!-------------------------------------------------------------------------------

!****d* Datatypes/SIGunits
!
! NAME
!    SIGunits - A sub-structure of the SIGtype structure, defining units.
!
! SOURCE
!
  TYPE SIGunits
    CHARACTER(len = 64) :: freq = "Hz"
  END TYPE SIGunits
!
!****

!****d* Datatypes/SIGrange
!
! NAME
!    SIGrange - A sub-structure of the SIGtype structure, setting valid ranges.
!
! SOURCE
!
  TYPE SIGrange
    REAL(wp), DIMENSION(2) :: freq = (/ 0.5E9_wp, 5.0E9_wp /)
  END TYPE SIGrange
!
!****

!****d* Datatypes/SIGtype
!
! NAME
!    SIGtype - A sub-structure of the ROprof structure, holding GNSS signal information.
!
! SOURCE
!
  TYPE SIGtype
    REAL(wp)               :: freq = ropp_MDFV ! Carrier frequency (Hz)
    TYPE(SIGunits)         :: units
    TYPE(SIGrange)         :: range
  END TYPE SIGtype
!
!****

!-------------------------------------------------------------------------------
! 7. Background meta data
!-------------------------------------------------------------------------------

!****d* Datatypes/BGunits
!
! NAME
!    BGunits - A sub-structure of the BGtype structure, defining units.
!
! SOURCE
!
  TYPE BGunits
    CHARACTER(len = 64) :: year     = "years"
    CHARACTER(len = 64) :: month    = "months"
    CHARACTER(len = 64) :: day      = "days"
    CHARACTER(len = 64) :: hour     = "hours"
    CHARACTER(len = 64) :: minute   = "minutes"
    CHARACTER(len = 64) :: fcPeriod = "hours"
  END TYPE BGunits
!
!****

!****d* Datatypes/BGrange
!
! NAME
!    BGrange - A sub-structure of the BGtype structure, setting valid ranges.
!
! SOURCE
!
  TYPE BGrange
    INTEGER,  DIMENSION(2) :: year     = (/1995,    2099/)
    INTEGER,  DIMENSION(2) :: month    = (/  01,      12/)
    INTEGER,  DIMENSION(2) :: day      = (/  01,      31/)
    INTEGER,  DIMENSION(2) :: hour     = (/  00,      23/)
    INTEGER,  DIMENSION(2) :: minute   = (/  00,      59/)
    REAL(wp), DIMENSION(2) :: fcperiod = (/  00.0_wp, 24.0_wp/)
  END TYPE BGrange
!
!****

!****d* Datatypes/BGtype
!
! NAME
!    BGunits - A sub-structure of the ROprof structure, describing background (as used
!              in the retrieval) meta data.
!
! SOURCE
!
  TYPE BGtype
    CHARACTER(len = 20) :: source   = 'NONE'    ! Source of b/g profile
    INTEGER             :: year     = 9999      ! VT year   of b/g
    INTEGER             :: month    =   99      ! VT month  of b/g
    INTEGER             :: day      =   99      ! VT day    of b/g
    INTEGER             :: hour     =   99      ! VT hour   of b/g
    INTEGER             :: minute   =   99      ! VT minute of b/g
    REAL(wp)            :: fcperiod =  999.9_wp ! F/c period (hrs)
    TYPE(BGunits)       :: units
    TYPE(BGrange)       :: range
  END TYPE BGtype
!
!****

!-------------------------------------------------------------------------------
! 8. Level 1a - Orbits, phases and amplitudes
!-------------------------------------------------------------------------------

!****d* Datatypes/L1aunits
!
! NAME
!    L1aunits - A sub-structure of the L1atype structure, defining units.
!
! SOURCE
!
  TYPE L1aunits
    CHARACTER(len = 64) :: dtime      = "seconds"
    CHARACTER(len = 64) :: snr        = "volt / volt"
    CHARACTER(len = 64) :: phase      = "metres"
    CHARACTER(len = 64) :: r_gns      = "metres"
    CHARACTER(len = 64) :: v_gns      = "metres / seconds"
    CHARACTER(len = 64) :: r_leo      = "metres"
    CHARACTER(len = 64) :: v_leo      = "metres / seconds"
    CHARACTER(len = 64) :: phase_qual = "percent"
  END TYPE L1aunits
!
!****

!****d* Datatypes/L1arange
!
! NAME
!    L1arange - A sub-structure of the L1atype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L1arange
    REAL(wp), DIMENSION(2) :: dtime      = (/   -1.0_wp, 539.999_wp/)
    REAL(wp), DIMENSION(2) :: snr        = (/    0.0_wp, 50000.0_wp/)
    REAL(wp), DIMENSION(2) :: phase      = (/ -1.0e6_wp,   1.0e6_wp/)
    REAL(wp), DIMENSION(2) :: r_gns      = (/-43.0e6_wp,  43.0e6_wp/)
    REAL(wp), DIMENSION(2) :: v_gns      = (/ -1.0e4_wp,   1.0e4_wp/)
    REAL(wp), DIMENSION(2) :: r_leo      = (/-10.0e6_wp,  10.0e6_wp/)
    REAL(wp), DIMENSION(2) :: v_leo      = (/ -1.0e4_wp,   1.0e4_wp/)
    REAL(wp), DIMENSION(2) :: phase_qual = (/    0.0_wp,   100.0_wp/)
  END TYPE L1arange
!
!****

!****d* Datatypes/L1aref
!
! NAME
!    L1aref - A sub-structure of the L1atype structure, defining the reference frame
!             for POD data.
!
! SOURCE
!
  TYPE L1aref
    CHARACTER(len = 3) :: r_gns = "ECF"
    CHARACTER(len = 3) :: v_gns = "ECI"
    CHARACTER(len = 3) :: r_leo = "ECF"
    CHARACTER(len = 3) :: v_leo = "ECI"
  END TYPE L1aref
!
!****

!****d* Datatypes/L1atype
!
! NAME
!    L1atype - A sub-structure of the ROprof structure, containing amplitude, phase and
!              POD data.
!
! SOURCE
!
  TYPE L1atype
    INTEGER                           :: Npoints    = 0       ! No. of samples in L1a profile
    LOGICAL                           :: Missing    = .TRUE.  ! No valid data if .T.
    REAL(wp), DIMENSION(:),   POINTER :: dtime      => null() ! Time since start (s)
    REAL(wp), DIMENSION(:),   POINTER :: snr_L1ca   => null() ! Signal-to-noise ratio - L1 (V/V)
    REAL(wp), DIMENSION(:),   POINTER :: snr_L1p    => null() ! Signal-to-noise ratio - L1 (V/V)
    REAL(wp), DIMENSION(:),   POINTER :: snr_L2p    => null() ! Signal-to-noise ratio - L1 (V/V)
    REAL(wp), DIMENSION(:),   POINTER :: phase_L1   => null() ! Excess phase   - L1 (m)
    REAL(wp), DIMENSION(:),   POINTER :: phase_L2   => null() ! Excess phase   - L2 (m)
    REAL(wp), DIMENSION(:,:), POINTER :: r_gns      => null() ! GNSS position (m)
    REAL(wp), DIMENSION(:,:), POINTER :: v_gns      => null() ! GNSS velocity (m/s)
    REAL(wp), DIMENSION(:,:), POINTER :: r_leo      => null() ! LEO  position (m)
    REAL(wp), DIMENSION(:,:), POINTER :: v_leo      => null() ! LEO velocity  (m/s)
    REAL(wp), DIMENSION(:),   POINTER :: phase_qual => null() ! Quality value (%)
    TYPE(L1aunits)                    :: units
    TYPE(L1arange)                    :: range
    TYPE(L1aref)                      :: reference_frame
  END TYPE L1atype
!
!****

!-------------------------------------------------------------------------------
! 9. Level 1b - Bending angles and impact parametres
!-------------------------------------------------------------------------------

!****d* Datatypes/L1bunits
!
! NAME
!    L1bunits - A sub-structure of the L1btype structure, defining units.
!
! SOURCE
!
  TYPE L1bunits
    CHARACTER(len = 64)             :: lat_tp       = "degrees_north"
    CHARACTER(len = 64)             :: lon_tp       = "degrees_east"
    CHARACTER(len = 64)             :: azimuth_tp   = "degrees"
    CHARACTER(len = 64)             :: impact       = "metres"
    CHARACTER(len = 64)             :: bangle       = "radians"
    CHARACTER(len = 64)             :: bangle_sigma = "radians"
    CHARACTER(len = 64)             :: bangle_qual  = "percent"
  END TYPE L1bunits
!
!****

!****d* Datatypes/L1brange
!
! NAME
!    L1brange - A sub-structure of the L1btype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L1brange
    REAL(wp), DIMENSION(2)          :: lat_tp          = (/ -90.0_wp,   90.0_wp/)
    REAL(wp), DIMENSION(2)          :: lon_tp          = (/-180.0_wp,  180.0_wp/)
    REAL(wp), DIMENSION(2)          :: azimuth_tp      = (/   0.0_wp,  360.0_wp/)
    REAL(wp), DIMENSION(2)          :: impact          = (/   6.2e6_wp,  6.6e6_wp/)
    REAL(wp), DIMENSION(2)          :: bangle          = (/  -0.001_wp,   0.1_wp/)
    REAL(wp), DIMENSION(2)          :: bangle_sigma    = (/   0.0_wp,    0.01_wp/)
    REAL(wp), DIMENSION(2)          :: bangle_qual     = (/   0.0_wp,  100.0_wp/)
  END TYPE L1brange
!
!****

!****d* Datatypes/L1bType
!
! NAME
!    L1btype - A sub-structure of the ROprof structure, containing bending angle and
!              impact parameter data.
!
! SOURCE
!
  TYPE L1btype
    INTEGER                         :: Npoints          = 0       ! No. of samples in L1b profile
    LOGICAL                         :: Missing          = .TRUE.  ! No valid data if .T.
    REAL(wp), DIMENSION(:), POINTER :: lat_tp           => null() ! Latitude  (deg)
    REAL(wp), DIMENSION(:), POINTER :: lon_tp           => null() ! Longitude (deg)
    REAL(wp), DIMENSION(:), POINTER :: azimuth_tp       => null() ! GNSS->LEO line of sight angle (degT)
    REAL(wp), DIMENSION(:), POINTER :: impact_L1        => null() ! Impact param   - L1   (m)
    REAL(wp), DIMENSION(:), POINTER :: impact_L2        => null() ! Impact param   - L2   (m)
    REAL(wp), DIMENSION(:), POINTER :: impact           => null() ! Impact param   - corr (m)
    REAL(wp), DIMENSION(:), POINTER :: impact_Opt       => null() ! Impact param   - opt  (m)
    REAL(wp), DIMENSION(:), POINTER :: bangle_L1        => null() ! Bending angle  - L1   (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_L2        => null() ! Bending angle  - L2   (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle           => null() ! Bending angle  - corr (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_Opt       => null() ! Bending angle  - opt  (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_L1_sigma  => null() ! Error in BA    - L1   (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_L2_sigma  => null() ! Error in BA    - L2   (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_sigma     => null() ! Error in BA    - corr (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_Opt_sigma => null() ! Error in BA    - opt   (rad)
    REAL(wp), DIMENSION(:), POINTER :: bangle_L1_qual   => null() ! Quality values - L1   (%)
    REAL(wp), DIMENSION(:), POINTER :: bangle_L2_qual   => null() ! Quality values - L2   (%)
    REAL(wp), DIMENSION(:), POINTER :: bangle_qual      => null() ! Quality values - corr (%)
    REAL(wp), DIMENSION(:), POINTER :: bangle_Opt_qual  => null() ! Quality values - opt  (%)
    TYPE(L1bunits)                  :: units
    TYPE(L1brange)                  :: range
  END TYPE L1btype
!
!****

!-------------------------------------------------------------------------------
! 10. Level 2a - Refractivity and dry temperature
!-------------------------------------------------------------------------------

!****d* Datatypes/L2aunits
!
! NAME
!    L2aunits - A sub-structure of the L2atype structure, defining units.
!
! SOURCE
!
  TYPE L2aunits
    CHARACTER(len = 64)             :: alt_refrac     = "metres"
    CHARACTER(len = 64)             :: geop_refrac    = "geopotential metres"
    CHARACTER(len = 64)             :: refrac         = "N-units"
    CHARACTER(len = 64)             :: refrac_sigma   = "N-units"
    CHARACTER(len = 64)             :: refrac_qual    = "percent"
    CHARACTER(len = 64)             :: dry_temp       = "kelvin"
    CHARACTER(len = 64)             :: dry_temp_sigma = "kelvin"
    CHARACTER(len = 64)             :: dry_temp_qual  = "percent"
  END TYPE L2aunits
!
!****

!****d* Datatypes/L2arange
!
! NAME
!    L2arange - A sub-structure of the L2atype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L2arange
    REAL(wp), DIMENSION(2)          :: alt_refrac     = (/-1.0e3_wp, 1.5e5_wp/)
    REAL(wp), DIMENSION(2)          :: geop_refrac    = (/-1.0e3_wp, 1.5e5_wp/)
    REAL(wp), DIMENSION(2)          :: refrac         = (/ 0.0_wp, 500.0_wp/)
    REAL(wp), DIMENSION(2)          :: refrac_sigma   = (/ 0.0_wp,  50.0_wp/)
    REAL(wp), DIMENSION(2)          :: refrac_qual    = (/ 0.0_wp, 100.0_wp/)
    REAL(wp), DIMENSION(2)          :: dry_temp       = (/150.0_wp, 350.0_wp/)
    REAL(wp), DIMENSION(2)          :: dry_temp_sigma = (/ 0.0_wp,  50.0_wp/)
    REAL(wp), DIMENSION(2)          :: dry_temp_qual  = (/ 0.0_wp, 100.0_wp/)
  END TYPE L2arange
!
!****

!****d* Datatypes/L2atype
!
! NAME
!    L2atype - A sub-structure of the ROprof structure, containing refractivity, 
!              dry temperature, altitude and geopotential height data.
!
! SOURCE
!
  TYPE L2atype
    INTEGER                         :: Npoints        = 0       ! No. of samples in L2a profile
    LOGICAL                         :: Missing        = .TRUE.  ! No valid data if .T.
    REAL(wp), DIMENSION(:), POINTER :: alt_refrac     => null() ! Geometric height (m)
    REAL(wp), DIMENSION(:), POINTER :: geop_refrac    => null() ! Geopotential height (m)
    REAL(wp), DIMENSION(:), POINTER :: refrac         => null() ! Refractivity (N-units)
    REAL(wp), DIMENSION(:), POINTER :: refrac_sigma   => null() ! Est. error in refractivity (N-units)
    REAL(wp), DIMENSION(:), POINTER :: refrac_qual    => null() ! Quality value - refrac (%)
    REAL(wp), DIMENSION(:), POINTER :: dry_temp       => null() ! Dry temperature (K)
    REAL(wp), DIMENSION(:), POINTER :: dry_temp_sigma => null() ! Est. error in dry temperature (K)
    REAL(wp), DIMENSION(:), POINTER :: dry_temp_qual  => null() ! Quality value - dry temp (%)
    TYPE(L2aunits)                  :: units
    TYPE(L2arange)                  :: range
  END TYPE L2atype
!
!****

!****d* Datatypes/L2atype_2d
!
! NAME
!    L2atype_2d - A sub-structure of the ROprof structure, containing refractivity, 
!                 dry temperature, altitude and geopotential height data.
!                 Two-dimensional version.
!
! SOURCE
!
  TYPE L2atype_2d
    INTEGER                           :: Npoints        = 0       ! No. of vertical points in each L2a profile
    INTEGER                           :: Nhoriz         = 0       ! No. of profiles in L2a slice
    LOGICAL                           :: Missing        = .TRUE.  ! .TRUE. means there are no valid data
    REAL(wp), DIMENSION(:,:), POINTER :: alt_refrac     => null() ! Geometric height (m)
    REAL(wp), DIMENSION(:,:), POINTER :: geop_refrac    => null() ! Geopotential height (m)
    REAL(wp), DIMENSION(:,:), POINTER :: refrac         => null() ! Refractivity (N-units)
    REAL(wp), DIMENSION(:,:), POINTER :: refrac_sigma   => null() ! Est. error in refractivity (N-units)
    REAL(wp), DIMENSION(:,:), POINTER :: refrac_qual    => null() ! Quality value - refrac (%)
    REAL(wp), DIMENSION(:,:), POINTER :: dry_temp       => null() ! Dry temperature (K)
    REAL(wp), DIMENSION(:,:), POINTER :: dry_temp_sigma => null() ! Est. error in dry temperature (K)
    REAL(wp), DIMENSION(:,:), POINTER :: dry_temp_qual  => null() ! Quality value - dry temp (%)
    TYPE(L2aunits)                    :: units
    TYPE(L2arange)                    :: range
  END TYPE L2atype_2d
!
!****

!-------------------------------------------------------------------------------
! 11. Level 2b - Meteorological quantities
!-------------------------------------------------------------------------------

!****d* Datatypes/L2bunits
!
! NAME
!    L2bunits - A sub-structure of the L2btype structure, defining units.
!
! SOURCE
!
  TYPE L2bunits
    CHARACTER(len = 64)             :: geop         = "geopotential metres"
    CHARACTER(len = 64)             :: geop_sigma   = "geopotential metres"
    CHARACTER(len = 64)             :: press        = "hPa"
    CHARACTER(len = 64)             :: press_sigma  = "hPa"
    CHARACTER(len = 64)             :: temp         = "kelvin"
    CHARACTER(len = 64)             :: temp_sigma   = "kelvin"
    CHARACTER(len = 64)             :: shum         = "gram / kilogram"
    CHARACTER(len = 64)             :: shum_sigma   = "gram / kilogram"
    CHARACTER(len = 64)             :: meteo_qual   = "percent"
  END TYPE L2bunits
!
!****

!****d* Datatypes/L2brange
!
! NAME
!    L2brange - A sub-structure of the L2btype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L2brange
    REAL(wp), DIMENSION(2)          :: geop         = (/ -1.0e3_wp,  1.0e5_wp /)
    REAL(wp), DIMENSION(2)          :: geop_sigma   = (/    0.0_wp,  500.0_wp /)
    REAL(wp), DIMENSION(2)          :: press        = (/ 0.0001_wp, 1100.0_wp /)
    REAL(wp), DIMENSION(2)          :: press_sigma  = (/    0.0_wp,    5.0_wp /)
    REAL(wp), DIMENSION(2)          :: temp         = (/  150.0_wp,  350.0_wp /)
    REAL(wp), DIMENSION(2)          :: temp_sigma   = (/    0.0_wp,    5.0_wp /)
    REAL(wp), DIMENSION(2)          :: shum         = (/    0.0_wp,   50.0_wp /)
    REAL(wp), DIMENSION(2)          :: shum_sigma   = (/    0.0_wp,   50.0_wp /)
    REAL(wp), DIMENSION(2)          :: meteo_qual   = (/    0.0_wp,  100.0_wp /)
  END TYPE L2brange
!
!****

!****d* Datatypes/L2btype
!
! NAME
!    L2btype - A sub-structure of the ROprof structure, containing meteorological (i.e.
!              temperature, pressure, (specific) humidity and geopotential height data.
!
! SOURCE
!
  TYPE L2btype
    INTEGER                         :: Npoints      = 0       ! No. of samples in L2b profile
    LOGICAL                         :: Missing      = .TRUE.  ! No valid data if .T.
    REAL(wp), DIMENSION(:), POINTER :: geop         => null() ! Geopotential height (m)
    REAL(wp), DIMENSION(:), POINTER :: geop_sigma   => null() ! Est. error in geopotential height (m)
    REAL(wp), DIMENSION(:), POINTER :: press        => null() ! Pressure (hPa)
    REAL(wp), DIMENSION(:), POINTER :: press_sigma  => null() ! Est. error in pressure (hPa)
    REAL(wp), DIMENSION(:), POINTER :: temp         => null() ! Temperature (K)
    REAL(wp), DIMENSION(:), POINTER :: temp_sigma   => null() ! Est. error in temperature (K)
    REAL(wp), DIMENSION(:), POINTER :: shum         => null() ! Specific humidity (g/Kg)
    REAL(wp), DIMENSION(:), POINTER :: shum_sigma   => null() ! Est. error in SH  (g/Kg)
    REAL(wp), DIMENSION(:), POINTER :: meteo_qual   => null() ! Quality value (%)
    TYPE(L2bunits)                  :: units
    TYPE(L2brange)                  :: range
  END TYPE L2btype
!
!****

!****d* Datatypes/L2btype_2d
!
! NAME
!    L2btype_2d - A sub-structure of the ROprof structure, containing meteorological (i.e.
!                 temperature, pressure, (specific) humidity and geopotential height data.
!                 Two-dimensional meteorological data version.
!
! SOURCE
!
  TYPE L2btype_2d
    INTEGER                           :: Npoints     = 0       ! No. of samples in L2b profile
    LOGICAL                           :: Missing     = .TRUE.  ! No valid data if .T.
    INTEGER                           :: Nhoriz      = 0       ! No. of horizontal points in L2b
    REAL(wp), DIMENSION(:,:), POINTER :: geop        => null() ! Geopotential height (m)
    REAL(wp), DIMENSION(:,:), POINTER :: geop_sigma  => null() ! Est. Error in geopot height (m)
    REAL(wp), DIMENSION(:,:), POINTER :: press       => null() ! Pressure (hPa)
    REAL(wp), DIMENSION(:,:), POINTER :: press_sigma => null() ! Est. Error in pressure (hPa)
    REAL(wp), DIMENSION(:,:), POINTER :: temp        => null() ! Temperature (K)
    REAL(wp), DIMENSION(:,:), POINTER :: temp_sigma  => null() ! Est. error in temperature (K)
    REAL(wp), DIMENSION(:,:), POINTER :: shum        => null() ! Specific humidity (g/Kg)
    REAL(wp), DIMENSION(:,:), POINTER :: shum_sigma  => null() ! Est. error in SH  (g/Kg)
    REAL(wp), DIMENSION(:,:), POINTER :: meteo_qual  => null() ! Quality value (%)
    TYPE(L2bunits)                    :: units
    TYPE(L2brange)                    :: range
  END TYPE L2btype_2d
!
!****

!-------------------------------------------------------------------------------
! 12. Level 2c - Meteorological surface quantities
!-------------------------------------------------------------------------------

!****d* Datatypes/L2cunits
!
! NAME
!    L2cunits - A sub-structure of the L2ctype structure, defining units.
!
! SOURCE
!
  TYPE L2cunits

! Standard variables

    CHARACTER(len = 64) :: lat_2d            = "degrees_north"
    CHARACTER(len = 64) :: lon_2d            = "degrees_east"
    CHARACTER(len = 64) :: dtheta            = "radians"
    CHARACTER(len = 64) :: geop_sfc          = "geopotential metres"
    CHARACTER(len = 64) :: press_sfc         = "hPa"
    CHARACTER(len = 64) :: press_sfc_sigma   = "hPa"
    CHARACTER(len = 64) :: press_sfc_qual    = "percent"

! Ionospheric variables

    CHARACTER(len = 64) :: Ne_max            = "m-3"
    CHARACTER(len = 64) :: Ne_max_sigma      = "m-3"
    CHARACTER(len = 64) :: H_peak            = "m"
    CHARACTER(len = 64) :: H_peak_sigma      = "m"
    CHARACTER(len = 64) :: H_width           = "m"
    CHARACTER(len = 64) :: H_width_sigma     = "m"

! Tropopause height variables

    CHARACTER(len = 64) :: tph_bangle        = "metres"
    CHARACTER(len = 64) :: tpa_bangle        = "radians"
    CHARACTER(len = 64) :: tph_bangle_flag   = ""

    CHARACTER(len = 64) :: tph_refrac        = "metres"
    CHARACTER(len = 64) :: tpn_refrac        = "N-units"
    CHARACTER(len = 64) :: tph_refrac_flag   = ""

    CHARACTER(len = 64) :: tph_tdry_lrt      = "metres"
    CHARACTER(len = 64) :: tpt_tdry_lrt      = "kelvin"
    CHARACTER(len = 64) :: tph_tdry_lrt_flag = ""

    CHARACTER(len = 64) :: tph_tdry_cpt      = "metres"
    CHARACTER(len = 64) :: tpt_tdry_cpt      = "kelvin"
    CHARACTER(len = 64) :: tph_tdry_cpt_flag = ""

    CHARACTER(len = 64) :: prh_tdry_cpt      = "metres"
    CHARACTER(len = 64) :: prt_tdry_cpt      = "kelvin"
    CHARACTER(len = 64) :: prh_tdry_cpt_flag = ""

    CHARACTER(len = 64) :: tph_temp_lrt      = "geopotential metres"
    CHARACTER(len = 64) :: tpt_temp_lrt      = "kelvin"
    CHARACTER(len = 64) :: tph_temp_lrt_flag = ""

    CHARACTER(len = 64) :: tph_temp_cpt      = "geopotential metres"
    CHARACTER(len = 64) :: tpt_temp_cpt      = "kelvin"
    CHARACTER(len = 64) :: tph_temp_cpt_flag = ""

    CHARACTER(len = 64) :: prh_temp_cpt      = "metres"
    CHARACTER(len = 64) :: prt_temp_cpt      = "kelvin"
    CHARACTER(len = 64) :: prh_temp_cpt_flag = ""

! Planetary boundary layer height variables

    CHARACTER(len = 64) :: pblh_bangle        = "metres"
    CHARACTER(len = 64) :: pbla_bangle        = "radians"
    CHARACTER(len = 64) :: pblh_bangle_flag   = ""

    CHARACTER(len = 64) :: pblh_refrac        = "metres"
    CHARACTER(len = 64) :: pbln_refrac        = "N-units"
    CHARACTER(len = 64) :: pblh_refrac_flag   = ""

    CHARACTER(len = 64) :: pblh_tdry          = "metres"
    CHARACTER(len = 64) :: pblt_tdry          = "kelvin"
    CHARACTER(len = 64) :: pblh_tdry_flag     = ""

    CHARACTER(len = 64) :: pblh_temp          = "geopotential metres"
    CHARACTER(len = 64) :: pblt_temp          = "kelvin"
    CHARACTER(len = 64) :: pblh_temp_flag     = ""

    CHARACTER(len = 64) :: pblh_shum          = "geopotential metres"
    CHARACTER(len = 64) :: pblq_shum          = "gram / kilogram"
    CHARACTER(len = 64) :: pblh_shum_flag     = ""

    CHARACTER(len = 64) :: pblh_rhum          = "geopotential metres"
    CHARACTER(len = 64) :: pblr_rhum          = "percent"
    CHARACTER(len = 64) :: pblh_rhum_flag     = ""

  END TYPE L2cunits
!
!****

!****d* Datatypes/L2crange
!
! NAME
!    L2crange - A sub-structure of the L2ctype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L2crange
    REAL(wp), DIMENSION(2) :: lat_2d            = (/  -90.0_wp,     90.0_wp /)
    REAL(wp), DIMENSION(2) :: lon_2d            = (/ -180.0_wp,    180.0_wp /)
    REAL(wp), DIMENSION(2) :: dtheta            = (/    0.0_wp,  3.14159_wp /)
    REAL(wp), DIMENSION(2) :: geop_sfc          = (/ -1.0e3_wp,    1.0e4_wp /)
    REAL(wp), DIMENSION(2) :: press_sfc         = (/  250.0_wp,   1100.0_wp /)
    REAL(wp), DIMENSION(2) :: press_sfc_sigma   = (/    0.0_wp,     10.0_wp /)
    REAL(wp), DIMENSION(2) :: press_sfc_qual    = (/    0.0_wp,    100.0_wp /)

    REAL(wp), DIMENSION(2) :: Ne_max            = (/-1.0e15_wp,   1.0e15_wp /)
    REAL(wp), DIMENSION(2) :: Ne_max_sigma      = (/    0.0_wp,   1.0e15_wp /)
    REAL(wp), DIMENSION(2) :: H_peak            = (/    0.0_wp,    1.0e6_wp /)
    REAL(wp), DIMENSION(2) :: H_peak_sigma      = (/    0.0_wp,    1.0e6_wp /)
    REAL(wp), DIMENSION(2) :: H_width           = (/    0.0_wp,    1.0e6_wp /)
    REAL(wp), DIMENSION(2) :: H_width_sigma     = (/    0.0_wp,    1.0e6_wp /)

    REAL(wp), DIMENSION(2) :: tph_bangle        = (/  6.2e6_wp,    6.6e6_wp /)
    REAL(wp), DIMENSION(2) :: tpa_bangle        = (/ -0.001_wp,      0.1_wp /)
    INTEGER,  DIMENSION(2) :: tph_bangle_flag   = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: tph_refrac        = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: tpn_refrac        = (/    0.0_wp,    500.0_wp /)
    INTEGER,  DIMENSION(2) :: tph_refrac_flag   = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: tph_tdry_lrt      = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: tpt_tdry_lrt      = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: tph_tdry_lrt_flag = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: tph_tdry_cpt      = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: tpt_tdry_cpt      = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: tph_tdry_cpt_flag = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: prh_tdry_cpt      = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: prt_tdry_cpt      = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: prh_tdry_cpt_flag = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: tph_temp_lrt      = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: tpt_temp_lrt      = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: tph_temp_lrt_flag = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: tph_temp_cpt      = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: tpt_temp_cpt      = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: tph_temp_cpt_flag = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: prh_temp_cpt      = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: prt_temp_cpt      = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: prh_temp_cpt_flag = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: pblh_bangle        = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: pbla_bangle        = (/ -0.001_wp,      0.1_wp /)
    INTEGER,  DIMENSION(2) :: pblh_bangle_flag   = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: pblh_refrac        = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: pbln_refrac        = (/    0.0_wp,    500.0_wp /)
    INTEGER,  DIMENSION(2) :: pblh_refrac_flag   = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: pblh_tdry          = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: pblt_tdry          = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: pblh_tdry_flag     = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: pblh_temp          = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: pblt_temp          = (/  150.0_wp,    350.0_wp /)
    INTEGER,  DIMENSION(2) :: pblh_temp_flag     = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: pblh_shum          = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: pblq_shum          = (/    0.0_wp,     50.0_wp /)
    INTEGER,  DIMENSION(2) :: pblh_shum_flag     = (/         0,       65535 /)

    REAL(wp), DIMENSION(2) :: pblh_rhum          = (/ -1.0e3_wp,    1.0e5_wp /)
    REAL(wp), DIMENSION(2) :: pblr_rhum          = (/    0.0_wp,    200.0_wp /)
    INTEGER,  DIMENSION(2) :: pblh_rhum_flag     = (/         0,       65535 /)

  END TYPE L2crange
!
!****

!****d* Datatypes/L2ctype
!
! NAME
!    L2ctype - A sub-structure of the ROprof structure, containing meteorological
!              surface pressure.
!
! SOURCE
!
  TYPE L2ctype
    INTEGER        :: Npoints           = 0         ! No. of samples in profile (0 or 1)
    LOGICAL        :: Missing           = .TRUE.    ! No valid data if .T.
    REAL(wp)       :: geop_sfc          = ropp_MDFV ! Geopotential height of surface (m)
    REAL(wp)       :: press_sfc         = ropp_MDFV ! Surface pressure (hPa)
    REAL(wp)       :: press_sfc_sigma   = ropp_MDFV ! Est. error in surface pressure (hPa)
    REAL(wp)       :: press_sfc_qual    = ropp_MDFV ! Quality value for L2b+c (%)

    REAL(wp)       :: Ne_max            = ropp_MDFV ! Peak elec.density (m-3)
    REAL(wp)       :: Ne_max_sigma      = ropp_MDFV ! Est error in peak elec.density (m-3)
    REAL(wp)       :: H_peak            = ropp_MDFV ! Height of peak elec. dens. (m)
    REAL(wp)       :: H_peak_sigma      = ropp_MDFV ! Est error in height of peak elec. dens. (m)
    REAL(wp)       :: H_width           = ropp_MDFV ! Width of Chapman layer (m)
    REAL(wp)       :: H_width_sigma     = ropp_MDFV ! Est error in width of Chapman layer (m)
    LOGICAL        :: direct_ion        = .FALSE.   ! Model L1 and L2 directly if .T.

    REAL(wp)       :: tph_bangle        = ropp_MDFV ! Bangle-derived TPH (m)
    REAL(wp)       :: tpa_bangle        = ropp_MDFV ! Bangle-derived TPA (rad)
    INTEGER        :: tph_bangle_flag   = ropp_MIFV ! Bangle-derived TPH QC flag

    REAL(wp)       :: tph_refrac        = ropp_MDFV ! Refrac-derived TPH (m)
    REAL(wp)       :: tpn_refrac        = ropp_MDFV ! Refrac-derived TPN (N_unit)
    INTEGER        :: tph_refrac_flag   = ropp_MIFV ! Refrac-derived TPH QC flag

    REAL(wp)       :: tph_tdry_lrt      = ropp_MDFV ! Tdry-derived TPH (lapse rate) (m)
    REAL(wp)       :: tpt_tdry_lrt      = ropp_MDFV ! Tdry-derived TPT (lapse rate) (K)
    INTEGER        :: tph_tdry_lrt_flag = ropp_MIFV ! Tdry-derived TPH (lapse rate) QC flag

    REAL(wp)       :: tph_tdry_cpt      = ropp_MDFV ! Tdry-derived TPH (cold point) (m)
    REAL(wp)       :: tpt_tdry_cpt      = ropp_MDFV ! Tdry-derived TPT (cold point) (K)
    INTEGER        :: tph_tdry_cpt_flag = ropp_MIFV ! Tdry-derived TPH (cold point) QC flag

    REAL(wp)       :: prh_tdry_cpt      = ropp_MDFV ! Tdry-derived PRH (cold point) (m)
    REAL(wp)       :: prt_tdry_cpt      = ropp_MDFV ! Tdry-derived PRT (cold point) (K)
    INTEGER        :: prh_tdry_cpt_flag = ropp_MIFV ! Tdry-derived TPH (cold point) QC flag

    REAL(wp)       :: tph_temp_lrt      = ropp_MDFV ! Temp-derived TPH (lapse rate) (m)
    REAL(wp)       :: tpt_temp_lrt      = ropp_MDFV ! Temp-derived TPT (lapse rate) (K)
    INTEGER        :: tph_temp_lrt_flag = ropp_MIFV ! Temp-derived TPH (lapse rate) QC flag

    REAL(wp)       :: tph_temp_cpt      = ropp_MDFV ! Temp-derived TPH (cold point) (m)
    REAL(wp)       :: tpt_temp_cpt      = ropp_MDFV ! Temp-derived TPT (cold point) (K)
    INTEGER        :: tph_temp_cpt_flag = ropp_MIFV ! Temp-derived TPH (cold point) QC flag

    REAL(wp)       :: prh_temp_cpt      = ropp_MDFV ! Temp-derived PRH (cold point) (m)
    REAL(wp)       :: prt_temp_cpt      = ropp_MDFV ! Temp-derived PRT (cold point) (K)
    INTEGER        :: prh_temp_cpt_flag = ropp_MIFV ! Temp-derived PRH (cold point) error flag

    REAL(wp)       :: pblh_bangle       = ropp_MDFV ! Bangle-derived PBLH (m)
    REAL(wp)       :: pblh_bangle2      = ropp_MDFV ! Bangle-derived PBLH (m)
    REAL(wp)       :: pbla_bangle       = ropp_MDFV ! Bangle-derived PBLA (rad)
    REAL(wp)       :: pbla_bangle2      = ropp_MDFV ! Bangle-derived PBLA (rad)
    INTEGER        :: pblh_bangle_flag  = ropp_MIFV ! Bangle-derived PBLH QC flag

    REAL(wp)       :: pblh_refrac       = ropp_MDFV ! Refrac-derived PBLH (m)
    REAL(wp)       :: pblh_refrac2      = ropp_MDFV ! Refrac-derived PBLH (m)
    REAL(wp)       :: pbln_refrac       = ropp_MDFV ! Refrac-derived PBLN (N_unit)
    REAL(wp)       :: pbln_refrac2      = ropp_MDFV ! Refrac-derived PBLN (N_unit)
    INTEGER        :: pblh_refrac_flag  = ropp_MIFV ! Refrac-derived PBLH QC flag

    REAL(wp)       :: pblh_tdry         = ropp_MDFV ! Tdry-derived PBLH (m)
    REAL(wp)       :: pblh_tdry2        = ropp_MDFV ! Tdry-derived PBLH (m)
    REAL(wp)       :: pblt_tdry         = ropp_MDFV ! Tdry-derived PBLT (K)
    REAL(wp)       :: pblt_tdry2        = ropp_MDFV ! Tdry-derived PBLT (K)
    INTEGER        :: pblh_tdry_flag    = ropp_MIFV ! Tdry-derived PBLH QC flag

    REAL(wp)       :: pblh_temp         = ropp_MDFV ! Temp-derived PBLH (m)
    REAL(wp)       :: pblh_temp2        = ropp_MDFV ! Temp-derived PBLH (m)
    REAL(wp)       :: pblt_temp         = ropp_MDFV ! Temp-derived PBLT (K)
    REAL(wp)       :: pblt_temp2        = ropp_MDFV ! Temp-derived PBLT (K)
    INTEGER        :: pblh_temp_flag    = ropp_MIFV ! Temp-derived PBLH QC flag

    REAL(wp)       :: pblh_shum         = ropp_MDFV ! shum-derived PBLH (m)
    REAL(wp)       :: pblh_shum2        = ropp_MDFV ! shum-derived PBLH (m)
    REAL(wp)       :: pblq_shum         = ropp_MDFV ! shum-derived PBLQ (g/kg)
    REAL(wp)       :: pblq_shum2        = ropp_MDFV ! shum-derived PBLQ (g/kg)
    INTEGER        :: pblh_shum_flag    = ropp_MIFV ! shum-derived PBLH QC flag

    REAL(wp)       :: pblh_rhum         = ropp_MDFV ! rhum-derived PBLH (m)
    REAL(wp)       :: pblh_rhum2        = ropp_MDFV ! rhum-derived PBLH (m)
    REAL(wp)       :: pblr_rhum         = ropp_MDFV ! rhum-derived PBLR (%)
    REAL(wp)       :: pblr_rhum2        = ropp_MDFV ! rhum-derived PBLR (%)
    INTEGER        :: pblh_rhum_flag    = ropp_MIFV ! rhum-derived PBLH QC flag

    TYPE(L2cunits) :: units
    TYPE(L2crange) :: range
  END TYPE L2ctype
!
!****

!****d* Datatypes/L2ctype_2d
!
! NAME
!    L2ctype_2d - A sub-structure of the ROprof structure, containing meteorological
!                 surface pressure.
!                 Two-dimensional meteorological data version.
!
! SOURCE
!
  TYPE L2ctype_2d
    INTEGER                         :: Npoints = 0        ! No. of samples in profile (0 or 1)
    LOGICAL                         :: Missing = .TRUE.   ! No valid data if .T.
    INTEGER                         :: Nhoriz  = 0        ! No. of horizontal locations
    REAL(wp)                        :: dtheta             ! Angle between profiles
    REAL(wp), DIMENSION(:), POINTER :: lat_2d => null()   ! Latitude position
    REAL(wp), DIMENSION(:), POINTER :: lon_2d => null()   ! Longitude position
    REAL(wp), DIMENSION(:), POINTER :: geop_sfc => null() ! Geopotential height of surface (m)
    REAL(wp), DIMENSION(:), POINTER :: press_sfc => null() ! Surface pressure (hPa)
    REAL(wp), DIMENSION(:), POINTER :: press_sfc_sigma => null() ! Est. error in sfc pressure (hPa)
    REAL(wp), DIMENSION(:), POINTER :: press_sfc_qual => null() ! Quality value for L2b+c (%)
    TYPE(L2cunits) :: units
    TYPE(L2crange) :: range
 END TYPE L2ctype_2d
!
!****

!-------------------------------------------------------------------------------
! 13. Level 2d - Meteorological model level coefficients
!-------------------------------------------------------------------------------

!****d* Datatypes/L2dunits
!
! NAME
!    L2dunits - A sub-structure of the L2dtype structure, defining units.
!
! SOURCE
!
  TYPE L2dunits
    CHARACTER(len = 40) :: level_coeff_a = 'hPa'
    CHARACTER(len = 40) :: level_coeff_b = ''
  END TYPE L2dunits
!
!****

!****d* Datatypes/L2drange
!
! NAME
!    L2drange - A sub-structure of the L2dtype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L2drange
    REAL(wp), DIMENSION(2) :: level_coeff_a = (/ 0.0_wp, 2000.0_wp/)
    REAL(wp), DIMENSION(2) :: level_coeff_b = (/ 0.0_wp,    2.0_wp/)
  END TYPE L2drange
!
!****

!****d* Datatypes/L2dtype
!
! NAME
!    L2dtype - A sub-structure of the ROprof structure, containing the defining
!              coefficients for vertical hybrid or eta-type level structures.
!
! SOURCE
!
  TYPE L2dtype
    INTEGER                         :: Npoints       = 0
    LOGICAL                         :: Missing       = .TRUE.  ! No valid data if .T.
    CHARACTER(len = 64)             :: level_type    = "UNKNOWN"
    REAL(wp), DIMENSION(:), POINTER :: level_coeff_a => null() ! Model level coefficients
    REAL(wp), DIMENSION(:), POINTER :: level_coeff_b => null()
    TYPE(L2dunits)                  :: units
    TYPE(L2drange)                  :: range
  END TYPE L2dtype
!
!****

!-------------------------------------------------------------------------------
! 14. Level 2e - Ionospheric quantities
!-------------------------------------------------------------------------------

!****d* Datatypes/L2eunits
!
! NAME
!    L2eunits - A sub-structure of the L2etype structure, defining units.
!
! SOURCE
!
  TYPE L2eunits
    CHARACTER(len = 64)             :: n_e           = "m-3"
    CHARACTER(len = 64)             :: n_e_sigma     = "m-3"
    CHARACTER(len = 64)             :: r_iono        = "m"
    CHARACTER(len = 64)             :: ne_peak       = "m-3"
    CHARACTER(len = 64)             :: ne_peak_sigma = "m-3"
    CHARACTER(len = 64)             :: r_peak        = "m"
    CHARACTER(len = 64)             :: r_peak_sigma  = "m"
    CHARACTER(len = 64)             :: h_zero        = "m"
    CHARACTER(len = 64)             :: h_zero_sigma  = "m"
    CHARACTER(len = 64)             :: h_grad        = ""
    CHARACTER(len = 64)             :: h_grad_sigma  = ""
  END TYPE L2eunits
!
!****

!****d* Datatypes/L2erange
!
! NAME
!    L2erange - A sub-structure of the L2etype structure, setting valid ranges.
!
! SOURCE
!
  TYPE L2erange
    REAL(wp), DIMENSION(2)          :: n_e           = (/    0.0_wp, 1.0e15_wp /)
    REAL(wp), DIMENSION(2)          :: n_e_sigma     = (/    0.0_wp, 1.0e15_wp /)
    REAL(wp), DIMENSION(2)          :: r_iono        = (/  6.2e6_wp,  7.4e6_wp /)
    REAL(wp), DIMENSION(2)          :: ne_peak       = (/    0.0_wp, 1.0e15_wp /)
    REAL(wp), DIMENSION(2)          :: ne_peak_sigma = (/    0.0_wp, 1.0e15_wp /)
    REAL(wp), DIMENSION(2)          :: r_peak        = (/    0.0_wp,  1.0e6_wp /)
    REAL(wp), DIMENSION(2)          :: r_peak_sigma  = (/    0.0_wp,  1.0e6_wp /)
    REAL(wp), DIMENSION(2)          :: h_zero        = (/    0.0_wp,  1.0e6_wp /)
    REAL(wp), DIMENSION(2)          :: h_zero_sigma  = (/    0.0_wp,  1.0e6_wp /)
    REAL(wp), DIMENSION(2)          :: h_grad        = (/    0.0_wp,    1.0_wp /)
    REAL(wp), DIMENSION(2)          :: h_grad_sigma  = (/    0.0_wp,    1.0_wp /)
  END TYPE L2erange
!
!****

!****d* Datatypes/L2etype
!
! NAME
!    L2etype - A sub-structure of the ROprof structure, containing
!              ionospheric quantities, such as VaryChap parameters and
!              electron density profiles.
!
! SOURCE
!
  TYPE L2etype
    LOGICAL                         :: Missing       = .TRUE.  ! No valid data if .T.
    INTEGER                         :: Npoints       = 0       ! No. of ionospheric levels
    REAL(wp), DIMENSION(:), POINTER :: n_e           => null() ! Electron density (/m3)
    REAL(wp), DIMENSION(:), POINTER :: n_e_sigma     => null() ! Est. error in electron density (/m3)
    REAL(wp), DIMENSION(:), POINTER :: r_iono        => null() ! Electron density impact parameter (m)
    INTEGER                         :: Nlayers       = 0       ! No. of VaryChap layers
    REAL(wp), DIMENSION(:), POINTER :: ne_peak       => null() ! Peak electron density (/m3)
    REAL(wp), DIMENSION(:), POINTER :: ne_peak_sigma => null() ! Est. error in peak electron density (/m3)
    REAL(wp), DIMENSION(:), POINTER :: r_peak        => null() ! Impact height of peak ne (m)
    REAL(wp), DIMENSION(:), POINTER :: r_peak_sigma  => null() ! Est. error in impact height of peak ne (m)
    REAL(wp), DIMENSION(:), POINTER :: h_zero        => null() ! Scale height of ne (m)
    REAL(wp), DIMENSION(:), POINTER :: h_zero_sigma  => null() ! Est. error in scale height of ne (m)
    REAL(wp), DIMENSION(:), POINTER :: h_grad        => null() ! Gradient of scale height of ne ()
    REAL(wp), DIMENSION(:), POINTER :: h_grad_sigma  => null() ! Est. error in gradient of scale height of ne ()
    TYPE(L2eunits)                  :: units
    TYPE(L2erange)                  :: range
  END TYPE L2etype
!
!****

!-------------------------------------------------------------------------------
! 15. Variable lists
!-------------------------------------------------------------------------------

!****id* Datatypes/VlisttypeD0d
!
! NAME
!    VlisttypeD0d - Variable list for scalar double precision variables.
!
! NOTES
!    This parameter is an internal parameter to the ropp_io library and not
!    intended to be used by users.
!
! SOURCE
!
  TYPE VlisttypeD0d
    CHARACTER(len = 1024)             :: name      = ""
    CHARACTER(len = 1024)             :: long_name = ""
    CHARACTER(len = 1024)             :: units     = ""
    REAL(wp), DIMENSION(2)            :: range     = (/ ropp_MDFV, &
                                                        ropp_MDFV /)
    REAL(wp)                          :: DATA
    TYPE(VlisttypeD0d), POINTER       :: next => null()
  END TYPE VlisttypeD0d
!
!****

!****id* Datatypes/VlisttypeD1d
!
! NAME
!    VlisttypeD1d - Variable list for one-dimensional double precision
!                     variables.
!
! NOTES
!    This parameter is an internal parameter to the ropp_io library and not
!    intended to be used by users.
!
! SOURCE
!
  TYPE VlisttypeD1d
    CHARACTER(len = 1024)             :: name      = ""
    CHARACTER(len = 1024)             :: long_name = ""
    CHARACTER(len = 1024)             :: units     = ""
    REAL(wp), DIMENSION(2)            :: range     = (/ ropp_MDFV, &
                                                        ropp_MDFV /)
    REAL(wp), DIMENSION(:), POINTER   :: DATA
    TYPE(VlisttypeD1d), POINTER       :: next => null()
  END TYPE VlisttypeD1d
!
!****

!****id* Datatypes/VlisttypeD2d
!
! NAME
!    VlisttypeD2d - Variable list for two-dimensional double precision
!                     variables.
!
! NOTES
!    This parameter is an internal parameter to the ropp_io library and not
!    intended to be used by users.
!
! SOURCE
!
  TYPE VlisttypeD2d
    CHARACTER(len = 1024)             :: name      = ""
    CHARACTER(len = 1024)             :: long_name = ""
    CHARACTER(len = 1024)             :: units     = ""
    REAL(wp), DIMENSION(2)            :: range     = (/ ropp_MDFV, &
                                                        ropp_MDFV /)
    REAL(wp), DIMENSION(:,:), POINTER :: DATA
    TYPE(VlisttypeD2d), POINTER       :: next => null()
  END TYPE VlisttypeD2d
!
!****

!****id* Datatypes/Vlisttype
!
! NAME
!    Vlisttype - Variable list for additional variables variables.
!
! NOTES
!    This parameter is an internal parameter to the ropp_io library and not
!    intended to be used by users.
!
! SOURCE
!
  TYPE Vlisttype
    TYPE(VlisttypeD0d), POINTER :: VlistD0d => null()
    TYPE(VlisttypeD1d), POINTER :: VlistD1d => null()
    TYPE(VlisttypeD2d), POINTER :: VlistD2d => null()
  END TYPE Vlisttype
!
!****

!-------------------------------------------------------------------------------
! 16. RO profile
!-------------------------------------------------------------------------------

!****d* Datatypes/ROunits
!
! NAME
!    ROunits - A sub-structure of the ROprof structure, defining (top-level) units.
!
! SOURCE
!
  TYPE ROunits
    CHARACTER(len = 64) :: pcd          = "bits"
    CHARACTER(len = 64) :: overall_qual = "percent"
  END TYPE ROunits
!
!****

!****d* Datatypes/ROrange
!
! NAME
!    ROrange - A sub-structure of the ROprof structure, defining (top level) ranges.
!
! SOURCE
!
  TYPE ROrange
    INTEGER,  DIMENSION(2) :: pcd          = (/ 0,      32767      /)
    REAL(wp), DIMENSION(2) :: overall_qual = (/ 0.0_wp,   100.0_wp /)
  END TYPE ROrange
!
!****

!****d* Datatypes/ROprof
!
! NAME
!    ROprof - Radio Occultation data (profile) data type.
!
! SYNOPSIS
!    use ropp_io_types
!      ...
!    type(ROprof) :: ro_data
!
! NOTES
!    The ROprof structure is composed out of several other structures; see the user guide
!    for a breakdown of the actual element names.
!
! SEE ALSO
!    DT7type
!    GEOtype
!    SIGtype
!    BGtype
!    L1atype
!    L1btype
!    L2atype
!    L2btype
!    L2ctype
!    L2dtype
!    ROunits
!
! SOURCE
!
  TYPE ROprof
    CHARACTER(len = 21)  :: FmtVersion          = "UNKNOWN" ! File format version ID
    CHARACTER(len = 40)  :: occ_id              = "UNKNOWN" ! Occultation ID
    CHARACTER(len = 4)   :: leo_id              = "UNKN"    ! LEO identifier
    CHARACTER(len = 4)   :: gns_id              = "U999"    ! GNSS identifier
    CHARACTER(len = 4)   :: stn_id              = "UNKN"    ! GSN station identifier
    CHARACTER(len = 40)  :: processing_centre   = "UNKNOWN" ! Processing centre
    CHARACTER(len = 40)  :: processing_software = "UNKNOWN" ! Processing centre software
    CHARACTER(len = 40)  :: pod_method          = "UNKNOWN" ! POD processing method
    CHARACTER(len = 40)  :: phase_method        = "UNKNOWN" ! Excess phase processing method
    CHARACTER(len = 40)  :: bangle_method       = "UNKNOWN" ! Bending angle processing method
    CHARACTER(len = 40)  :: refrac_method       = "UNKNOWN" ! Refractivity processing method
    CHARACTER(len = 40)  :: meteo_method        = "UNKNOWN" ! Meteorological processing method
    CHARACTER(len = 160) :: thin_method         = "UNKNOWN" ! Profile thinning method
    CHARACTER(len = 40)  :: software_version    = "UNKNOWN" ! ROPP version ID
    TYPE(DT7type)        :: DTocc                           ! Date/time of occultation
    TYPE(DT7type)        :: DTpro                           ! Date/time of processing
    INTEGER              :: PCD                 = 65535     ! Product quality flags
    REAL(wp)             :: overall_qual        = ropp_MDFV ! Overall quality value
    TYPE(GEOtype)        :: georef                          ! Georeferencing of the profile
    TYPE(SIGtype)        :: signal1                         ! Characteristics of 1st signal
    TYPE(SIGtype)        :: signal2                         ! Characteristics of 2nd signal
    TYPE(BGtype)         :: bg                              ! Background meta-data
    TYPE(L1atype)        :: Lev1a                           ! Level 1a data
    TYPE(L1btype)        :: Lev1b                           ! Level 1b data
    TYPE(L2atype)        :: Lev2a                           ! Level 2a data
    TYPE(L2btype)        :: Lev2b                           ! Level 2b data
    TYPE(L2ctype)        :: Lev2c                           ! Level 2c data
    TYPE(L2dtype)        :: Lev2d                           ! Level 2d data
    TYPE(L2etype)        :: Lev2e                           ! Level 2e data
    TYPE(ROunits)        :: units                           ! Parameter unit names
    TYPE(ROrange)        :: range                           ! Parameter ranges
    TYPE(Vlisttype)      :: vlist                           ! Additional variables
  END TYPE ROprof

!****

!****d* Datatypes/ROprof2d
!
! NAME
!    ROprof2d - Radio Occultation data (profile) data type (two-dimensional meteorological data).
!
! SYNOPSIS
!    use ropp_io_types
!      ...
!    type(ROprof2d) :: ro_data
!
! NOTES
!    The ROprof structure is composed out of several other structures; see the user guide
!    for a breakdown of the actual element names.
!
! SEE ALSO
!    DT7type
!    GEOtype
!    SIGtype
!    BGtype
!    L1atype
!    L1btype
!    L2atype
!    L2btype_2d
!    L2ctype_2d
!    L2dtype
!    ROunits
!
! SOURCE
!
  TYPE ROprof2d
    CHARACTER(len = 21)  :: FmtVersion          = "UNKNOWN" ! File format version ID
    CHARACTER(len = 40)  :: occ_id              = "UNKNOWN" ! Occultation ID
    CHARACTER(len = 4)   :: leo_id              = "UNKN"    ! LEO identifier
    CHARACTER(len = 4)   :: gns_id              = "U999"    ! GNSS identifier
    CHARACTER(len = 4)   :: stn_id              = "UNKN"    ! GSN station identifier
    CHARACTER(len = 40)  :: processing_centre   = "UNKNOWN" ! Processing centre
    CHARACTER(len = 40)  :: processing_software = "UNKNOWN" ! Processing centre software
    CHARACTER(len = 40)  :: pod_method          = "UNKNOWN" ! POD processing method
    CHARACTER(len = 40)  :: phase_method        = "UNKNOWN" ! Excess phase processing method
    CHARACTER(len = 40)  :: bangle_method       = "UNKNOWN" ! Bending angle processing method
    CHARACTER(len = 40)  :: refrac_method       = "UNKNOWN" ! Refractivity processing method
    CHARACTER(len = 40)  :: meteo_method        = "UNKNOWN" ! Meteorological processing method
    CHARACTER(len = 160) :: thin_method         = "UNKNOWN" ! Profile thinning method
    CHARACTER(len = 40)  :: software_version    = "UNKNOWN" ! ROPP version ID
    TYPE(DT7type)        :: DTocc                           ! Date/time of occultation
    TYPE(DT7type)        :: DTpro                           ! Date/time of processing
    INTEGER              :: PCD                 = 65535     ! Product quality flags
    REAL(wp)             :: overall_qual        = ropp_MDFV ! Overall quality value
    TYPE(GEOtype)        :: georef                          ! Georeferencing of the profile
    TYPE(SIGtype)        :: signal1                         ! Characteristics of 1st signal
    TYPE(SIGtype)        :: signal2                         ! Characteristics of 2nd signal
    TYPE(BGtype)         :: bg                              ! Background meta-data
    TYPE(L1atype)        :: Lev1a                           ! Level 1a data
    TYPE(L1btype)        :: Lev1b                           ! Level 1b data
    TYPE(L2atype_2d)     :: Lev2a                           ! Level 2a data (two-dimensional)
    TYPE(L2btype_2d)     :: Lev2b                           ! Level 2b data (two-dimensional)
    TYPE(L2ctype_2d)     :: Lev2c                           ! Level 2c data (two-dimensional)
    TYPE(L2dtype)        :: Lev2d                           ! Level 2d data
    TYPE(ROunits)        :: units                           ! Parameter unit names
    TYPE(ROrange)        :: range                           ! Parameter ranges
    TYPE(Vlisttype)      :: vlist                           ! Additional variables
  END TYPE ROprof2d

!****

!-------------------------------------------------------------------------------
! 17. Error correlation / covariance matrix
!-------------------------------------------------------------------------------

!****d* Datatypes/ROcorcov
!
! NAME
!    ROcorcov - Error correlation or covariance data type.
!
! SYNOPSIS
!    use ropp_io_types
!      ...
!    type(ROcorcov) :: covar
!
! NOTES
!    The ROcorcov structure contains an error correlation or covariance matrix
!    (the latter will be splitted into an error correlation matrix and an array
!    of diagonal standard deviations). The error correlation matrix is stored in
!    Lapack's packed format for positive definite matrices, i.e. as a 1d array.
!
!    As an additional element, the structure may also contain a latitude range
!    pair which is intended to specify within which latitude band the error
!    covariance is applicable.
!
! SEE ALSO
!
! SOURCE
!
  TYPE ROcorcov
    CHARACTER(len = 13) :: FmtVersion        = "UNKNOWN" ! File format version ID
    CHARACTER(len = 40) :: processing_centre = "UNKNOWN" ! Processing centre

    REAL(wp)                        :: lat_min = -90.0_wp
    REAL(wp)                        :: lat_max =  90.0_wp
    REAL(wp), DIMENSION(:), POINTER :: sigma          => null()
    REAL(wp), DIMENSION(:), POINTER :: corr           => null()
    REAL(wp), DIMENSION(:), POINTER :: temp_sigma     => null()
    REAL(wp), DIMENSION(:), POINTER :: shum_rel_sigma => null()
    REAL(wp)                        :: press_sfc_rel_sigma

  END TYPE ROcorcov
!
!****

END MODULE ropp_io_types
