! $Id: earth.f90,v 1.6 2006/01/17 14:28:25 frdo Exp $

MODULE EarthMod

!****m* Earth/EarthMod *
!-----------------------------------------------------------------------
!
! NAME
!   EarthMod     (earth.f90)
!
! SYNOPSIS
!   Module defining Earth-related constants
!   and datum conversion routines.
!
!   USE EarthMod
!   CHARACTER (LEN=6)  :: Datum1,  Datum2
!   CHARACTER (LEn=12) :: NGref
!   DOUBLE PRECISION   :: GEO1(3), GEO2(3), ECF(3), delXYZ(3)
!   DOUBLE PRECISION   :: rm, rp, re, a, f
!   DOUBLE PRECISION   :: rlat, rleg(nmax+1)
!   DOUBLE PRECISION   :: Latitude1, Longitude1
!   DOUBLE PRECISION   :: Latitude2, Longitude2
!   real(wp)   :: DistNorth, DistEast, Easting, Northing
!   REAL               :: Lat1, Lon1, Lat2, Lon2, Distance, Bearing, Ht
!   INTEGER            :: m, nmax, iflg
!
!  - Ellipsoid/geoid datum routines:
!
!   CALL Datum_Trans  ( Datum1, GEO1, Datum2, GEO2 )
!   CALL Datum_Erad   ( Datum1, Latitude1, rm, rp, re )
!   CALL Datum_Param  ( Datum1, delXYZ, A, F )
!   CALL Datum_HMSL   ( Datum1, GEO1, HMSL )
!   CALL Geopot_LegFn ( m, rlat, nmax, rleg )
!   CALL ECFtoGEO     ( ECF, Datum1, GEO, iflg )
!
!  - Earth physics:
!
!   RAD = EradL      ( Lat1 )         [See also Datum_Erad]
!   g   = EGravity    ( Lat1, Ht )
!
! CONTAINS
!   Datum_Trans
!   Datum_Erad
!   Datum_Param
!   Datum_HMSL
!   Geopot_LegFn
!   ECFtoGEO
!   EradL
!   EGravity
!
! PARAMETERS
!   BIGNEG   = -9999999.0D0             ! Missing data value
!   SMALL    = 1.0D-8                   ! Something close to zero
!   Pi       = 3.14159265358979323846D0 ! Pi
!   HalfPi   = Pi / 2.0D0               ! Pi / 2
!   TwoPi    = Pi * 2.0D0               ! Pi * 2
!   RadToDeg = 180.0D0 / Pi             ! Radians to degrees
!   DegToRad = Pi / 180.0D0             ! Degrees to radians
!
! DESCRIPTION
!   Defines (a) physical constants related to angle conversion,
!           (b) various routines to transform a datum (location
!               on the Earth) between different coordinate systems
!           (c) various routines to calculate point to point parameters
!               (distance, bearing...)
!           (d) Earth's physical parameters (radius, gravity)
!
! AUTHOR
!  Dave Offiler
!
! COPYRIGHT
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal

! Fixed values

  real(wp), PARAMETER :: BIGNEG   = -9999999.0D0       ! Missing data value
  real(wp), PARAMETER :: SMALL    = 1.0D-8             ! Something close to zero

  real(wp), PARAMETER :: Pi       = 3.14159265358979323846D0 ! Pi
  real(wp), PARAMETER :: HalfPi   = Pi / 2.0D0         ! Pi / 2
  real(wp), PARAMETER :: TwoPi    = Pi * 2.0D0         ! Pi * 2
  real(wp), PARAMETER :: RadToDeg = 180.0D0 / Pi       ! Radians to degrees
  real(wp), PARAMETER :: DegToRad = Pi / 180.0D0       ! Degrees to radians

!--------------------------------------------------------------------------

CONTAINS

!--------------------------------------------------------------------------

SUBROUTINE Datum_Trans ( Datum1, & ! (in)
                         GEO1,   & ! (in)
                         Datum2, & ! (in)
                         GEO2 )    ! (out)

!****s* Earth/Datum_Trans *
!-----------------------------------------------------------------------
!
! NAME
!   Datum_Trans     (earth.f90)
!
! SYNOPSIS
!   Transform geographic coordinates from one ellipsoid datum to another
!
!   USE EarthMod
!   real(wp) :: GEO1(3), GEO2(3)
!   GEO1(:) = (/ <lat>, <lon>, <ht> /) ! on WGS-84 ellipsoid
!   CALL Datum_Trans ( "WGS84", GEO1, "OSGB36", GEO2 )
!   [GEO2 contains lat/lon/ht on OS-GB (Airy 1936) spheroid]
!
! ARGUMENTS
!   Datum1  (in)   chr   Name of datum to transform 'from'
!   GEO1    (in)   dflt  Array(3) cordinates in Datum1
!                        (1) latitude  (deg) [X (m) if ECF]
!                        (2) longitude (deg) [Y (m) if ECF]
!                        (3) height    (m)   [Z (m) if ECF]
!   Datum2  (in)   chr   Name of datum to transform 'to'
!   GEO2    (out)  dflt  Array(3) coordinates in Datum2
!                        (1) latitude  (deg) [X (m) if ECF]
!                        (2) longitude (deg) [Y (m) if ECF]
!                        (3) height    (m)   [Z (m) if ECF]
!
! CALLS
!   Datum_Erad
!   Datum_Param
!   ECFtoGEO
!
! CALLED BY
!   Datum_HMSL
!
! ALGORITHM
!   Simplified Molodensky formulae [4]
!
! DESCRIPTION
!   Transforms latitude, longitude and height relative to one (ellipsoid)
!   datum to another datum. Input and output datums are specified as
!   character strings for any of the supported types:
!    "ED50"   - European datum, 1950
!    "GEM6"   - used for ERS orbits
!    "GRS80"  - Geodetic Reference System, 1980 [5]
!    "KL86"   - Kaye & Laby, 1986 [3]
!    "NAD27"  - North American Datum, 1927 (Clark 1866)
!    "OSGB36" - Airy 1936, UK National Grid (Ordnance Survey)
!    "OSU91"  - OSU, 1991-a
!    "WGS84"  - World Geodetic System 1984 [2]
!   plus the non-ellipsoid cases:
!    "SPHERE" - spherical earth (latitude independent)
!    "ECF"    - earth-centred fixed (x,y,z in m)
!   For datum "ECF", the associated 'GEO1' or 'GEO2' array elements are
!   interpreted as X,Y,Z distances from the Earth's centre.
!   Datum names are case-insensitive. If Datum1 name is not one of the
!   above, GEO2 array is returned with all elemnts -999999.0; if Datum2
!   name is unknown, the returned values are +999999.0.
!
! REFERENCES
!   [1] ACIC (1962). Geodesy for the layman. Geosciences Branch, Chart
!       Research Division, Aeronautical Chart and Information Centre,
!       St.Louis, Miss. January 1992.
!   [2] DMA (1987). Department of Defense World Geodetic System 1984 -
!       its definition and relationships with local geodetic systems.
!       DMA Technical Report 8350.2, DMA, September 1987.
!   [3] Kaye, CGW and TH Laby (1986). Tables of Physical and Chemical
!       Constants, 15th Edn. Longman, UK.
!   [4] Ruffhead, AC (1989). A 5-Parameter Transformation for OSGB36
!       to WGS84. MCE Working Paper No. 7/89, Mapping and Charting
!       Establishment, Computer and Geodesy Support Group, September 1989.
!   [5] Geodetic Reference System 1980, Bulletin Géodésique, Vol 54:3, 1980.
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
!
! COPYRIGHT
!   Language:		Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal
!
  IMPLICIT NONE

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)  :: Datum1   ! 'from' datum name
  CHARACTER (LEN=*), INTENT(IN)  :: Datum2   ! 'to'   datum name
  real(wp),  INTENT(IN)  :: GEO1(:)  ! coords in datum1
  real(wp),  INTENT(OUT) :: GEO2(:)  ! coords in datum2

! Local variables

  real(wp) :: DEL1(3), DEL2(3)       ! X,Y,Z offsets
  real(wp) :: GEO3(3)                ! local coord array
  real(wp) :: A1, A2, F1, F2         ! A and F parameters
  real(wp) :: RM, RP, RE             ! Earth radii of curvature
  real(wp) :: DA, DF                 ! delats is A & F
  real(wp) :: DX, DY, DZ             ! deltas in X,Y,Z
  real(wp) :: DLT, DLN, DHT          ! deltas in lat/lon/ht
  real(wp) :: CLT, CLN, SLT, SLN     ! cosines & sines

! Get geoid parameters for both datums

  CALL Datum_Param ( Datum1, DEL1, A1, F1 )
  CALL Datum_Param ( Datum2, DEL2, A2, F2 )

! Datum1 not found

  IF ( F1 <= BIGNEG ) THEN
    GEO2(:) = BIGNEG

! Datum2 not found

  ELSE IF ( F2 <= BIGNEG ) THEN
    GEO2(:) = -BIGNEG

! Datum1 and Datum2 identical - just copy.

  ELSE IF ( ABS(A1-A2) < SMALL .AND. &
            ABS(F1-F2) < SMALL ) THEN
    GEO2(:) = GEO1(:)

! Datum1 is in ECF coords: ECF --> GEO

  ELSE IF ( A1 < -SMALL .AND. &
            F1 <  SMALL ) THEN
    GEO3(:) = GEO1(:)
    CALL ECFtoGEO ( GEO3, Datum2, GEO2,  1 )

! Datum2 is in ECF coords: GEO --> ECF

  ELSE IF ( A2 < -SMALL .AND. &
            F1 <  SMALL ) THEN
    GEO3(:) = GEO1(:)
    CALL ECFtoGEO ( GEO2, Datum2, GEO3, -1 )

! Both datums ellipsoidal or spherical

  ELSE

! Delta parameters between datums

    DX = DEL1(1) - DEL2(1)
    DY = DEL1(2) - DEL2(2)
    DZ = DEL1(3) - DEL2(3)

    DA = A2 - A1
    DF = F2 - F1

! sines & cosines

    SLT = SIN ( GEO1(1)*DegToRad )
    CLT = COS ( GEO1(1)*DegToRad )
    SLN = SIN ( GEO1(2)*DegToRad )
    CLN = COS ( GEO1(2)*DegToRad )

! Earth radii in meridian & parallel

    CALL Datum_Erad ( Datum1, GEO1(1), RM, RP, RE )

! Simplified Molodensky Formulae for transform deltas

    DLT = -( DX * SLT * CLN + DY * SLT * SLN - DZ * CLT &
          - (A1 * DF + F1 * DA) * SIN(2.0D0*GEO1(1)*DegToRad) ) * RadToDeg / RM

    DLN = -( DX * SLN - DY * CLN ) * RadToDeg / ( RP * CLT )

    DHT = DX * CLT * CLN + DY * CLT * SLN + DZ * SLT &
        + (A1 * DF + F1 * DA) * SLT * SLT - DA

! Apply deltas

    GEO2(1) = GEO1(1) + DLT
    GEO2(2) = GEO1(2) + DLN
    GEO2(3) = GEO1(3) + DHT

  END IF

END SUBROUTINE Datum_Trans
!-----------------------------------------------------------------------

SUBROUTINE Datum_Erad ( Datum, & ! (in)
                        Lat,   & ! (in)
                        Rm,    & ! (out)
                        Rp,    & ! (out)
                        Re )     ! (out)

!****s* Earth/Datum_Erad *
!-----------------------------------------------------------------------
!
! NAME
!   Datum_Erad     (earth.f90)
!
! SYNOPSIS
!   Calculate the Earth's radii of curvature on ellipsoid datum
!
!   USE EarthMod
!   real(wp) :: Rm, Rp, Re
!   CALL Datum_Erad ( "WGS84", 50.0D0, Rm, Rp, Re )
!
! ARGUMENTS
!   Datum  (in)   chr   Name of datum
!   Lat    (in)   dflt  Latitude (degrees)
!   Rm     (out)  dflt  Radius of curvature along meridian
!                       i.e. N-S at given latitude (metres)
!   Rp     (out)  dflt  Radius of curvature along parallel
!                       i.e. E-W at given latitude (metres)
!   Re     (out)  dflt  Distance from earth centre to
!                       ellipsoid surface (metres)
!
! CALLS
!   Datum_Param
!
! CALLED BY
!   Datum_Trans
!   LLtoNE
!
! ALGORITHM
!    Rm   = a (1-e^2) / (1-e^2.sin^2{lat})^(3/2)
!    Rp   = a / (1-e^2.sin^2{lat})^(1/2)
!    Re^2 = a^2.(1-e^2) / (1 - e^2 + e^2.sin^2{lat})
!   where a is the equatorial radius,
!   and e is the eccentricity:
!    e^2  = f(2-f)
!   where f is the flattening parameter, and a,f define the ellipsoid.
!
! DESCRIPTION
!   Given the latitude of a point (in degrees), calculates the Earth's
!   radii of curvature in both the meridional (N/S) and parallel (E/W)
!   directions, and the Earth's radius (i.e. the distance from the
!   Earth's centre to surface at the given latitude) using an ellipsoid
!   model. Datum is specified as character string for any of the
!   supported types:
!    "ED50"   - European datum, 1950
!    "GEM6"   - used for ERS orbits
!    "GRS80"  - Geodetic Reference System, 1980 [5]
!    "KL86"   - Kaye & Laby, 1986 [3]
!    "NAD27"  - North American Datum, 1927 (Clark 1866)
!    "OSGB36" - Airy 1936, UK National Grid (Ordnance Survey)
!    "OSU91"  - OSU, 1991-a
!    "WGS84"  - World Geodetic System 1984 [2]
!   plus the non-elliposid cases:
!    "SPHERE" - spherical earth (latitude independent)
!    "ECF"    - earth-centred fixed (x,y,z in m)
!   Datum names are case-insensitive. If datum name is not one of the
!   above, Rm, Rp and Re are all returned as -9999999.0. For "SPHERE" and
!   "ECF", all 3 radii will be the same and equal to a mean earth radius.
!
! REFERENCES
!   [1] ACIC (1962). Geodesy for the layman. Geosciences Branch, Chart
!       Research Division, Aeronautical Chart and Information Centre,
!       St.Louis, Miss. January 1992.
!   [2] DMA (1987). Department of Defense World Geodetic System 1984 -
!       its definition and relationships with local geodetic systems.
!       DMA Technical Report 8350.2, DMA, September 1987.
!   [3] Kaye, CGW and TH Laby (1986). Tables of Physical and Chemical
!       Constants, 15th Edn. Longman, UK.
!   [4] Ruffhead, AC (1989). A 5-Parameter Transformation for OSGB36
!       to WGS84. MCE Working Paper No. 7/89, Mapping and Charting
!       Establishment, Computer and Geodesy Support Group, September 1989.
!   [5] Geodetic Reference System 1980, Bulletin Géodésique, Vol 54:3, 1980.
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
!
!
! COPYRIGHT
!   Language:		Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal

  IMPLICIT NONE

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)  :: Datum           ! Datum name
  real(wp),  INTENT(IN)  :: Lat             ! Latitude
  real(wp),  INTENT(OUT) :: Rm, Rp, Re      ! Eath radii of curvature

! Local variables

  real(wp) :: delXYZ(3)           ! ECF X,Y,Z offsets
  real(wp) :: a                   ! Equitorial radius
  real(wp) :: f                   ! Earth flattening parameter
  real(wp) :: e2                  ! Eccentricity^2
  real(wp) :: OME2                ! 1-e^2
  real(wp) :: A2OME2              ! a^2.(1-e^2)
  real(wp) :: SLT, X              ! temp values

! Get geoid parameters

  CALL Datum_Param ( Datum, delXYZ, a, f )

! Datum is ellipsoidal

  IF ( f > SMALL ) THEN
    e2     = f * ( 2.0D0 - f )
    OME2   = 1.0D0 - e2
    A2OME2 = a * a * OME2

    SLT = SIN ( Lat * DegToRad )
    X   = SQRT ( 1.0D0 - e2 * SLT * SLT )

    Rm = a * ( 1.0D0 - e2 ) / ( X * X * X )
    Rp = a / X
    Re = SQRT ( A2OME2 / ( OME2 + e2 * SLT * SLT ) )

! Datum is spherical

  ELSE IF ( f > BIGNEG ) THEN
    Rm = ABS ( a )
    Rp = Rm
    Re = Rm

! Datum not defined

  ELSE
    Rm = BIGNEG
    Rp = BIGNEG
    Re = BIGNEG

  END IF

END SUBROUTINE Datum_Erad
!-------------------------------------------------------------------------

SUBROUTINE Datum_Param ( Datum,  & ! (in)
                         delXYZ, & ! (out)
                         a,      & ! (out)
                         f )       ! (out)

!****s* Earth/Datum_Param *
!-----------------------------------------------------------------------
!
! NAME
!   Datum_Param     (earth.f90)
!
! SYNOPSIS
!   Return geoid parameters
!
!   USE EarthMod
!   real(wp) :: delXYZ(3), a, f
!   CALL Datum_Param ( "OSGB36", delXYZ, a, f )
!
! ARGUMENTS
!   Datum   (in)   chr   Name of datum
!   delXYZ  (out)  dflt  Array(3) delta offsets in x,y,z (m)
!   a       (out)  dflt  Semi-major axis (m)
!   f       (out)  dflt  Flattening parameter
!
! CALLED BY
!   Datum_HMSL
!   Datum_Trans
!   Datum_Erad
!   ECFtoGEO
!
! DESCRIPTION
!   Returns ellipsoid datum parameters. Datum is specified as a character
!   string for any of the supported types:
!    "ED50"   - European datum, 1950
!    "GEM6"   - used for ERS orbits
!    "GRS80"  - Geodetic Reference System, 1980 [5]
!    "KL86"   - Kaye & Laby, 1986 [3]
!    "NAD27"  - North American Datum, 1927 (Clark 1866)
!    "OSGB36" - Airy 1936, UK National Grid (Ordnance Survey)
!    "OSU91"  - OSU, 1991-a
!    "WGS84"  - World Geodetic System 1984 [2]
!   plus the non-elliposid cases:
!    "SPHERE" - spherical earth (latitude independent)
!    "ECF"    - earth-centred fixed (x,y,z in m)
!   Datum names are case-insensitive. If datum is blank, WGS-84 is used;
!   if datum is not blank but is not one of the above names, all
!   parameters are all returned as -9999999. For "SPHERE" and "ECF",
!   'a' represents mean earth radius value and 'f' is zero. NB: 'a' is
!   returned as a -ve value for "ECF" type.
!
! REFERENCES
!   [1] ACIC (1962). Geodesy for the layman. Geosciences Branch, Chart
!       Research Division, Aeronautical Chart and Information Centre,
!       St.Louis, Miss. January 1992.
!   [2] DMA (1987). Department of Defense World Geodetic System 1984 -
!       its definition and relationships with local geodetic systems.
!       DMA Technical Report 8350.2, DMA, September 1987.
!   [3] Kaye, CGW and TH Laby (1986). Tables of Physical and Chemical
!       Constants, 15th Edn. Longman, UK.
!   [4] Ruffhead, AC (1989). A 5-Parameter Transformation for OSGB36
!       to WGS84. MCE Working Paper No. 7/89, Mapping and Charting
!       Establishment, Computer and Geodesy Support Group, September 1989.
!   [5] Geodetic Reference System 1980, Bulletin Géodésique, Vol 54:3, 1980.
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
! 1.1-0   17-Jan-2006   Add GRS80 ellipsoid.          D. Offiler
!
!
! COPYRIGHT
!   Language:		Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal

  IMPLICIT NONE

! Constants

  INTEGER, PARAMETER :: ND = 11        ! No. of datums defined

! Parameters for each named datum:
!  x  = ecf x-offset from centre of gravity of the Earth (m)
!  y  = ecf y-offset from centre of gravity of the Earth (m)
!  z  = ecf z-offset from centre of gravity of the Earth (m)
!  ae = semi-major axis (m)
!  f1 = 1/flattening parameter (=1/f)

  CHARACTER (LEN=6), PARAMETER :: DATUMS(ND) = &
                 (/      "WGS84 ",    "OSU91 ",    "OSGB36",    "ED50  ", &
                         "NAD27 ",    "GEM6  ",    "GRS80 ",    "KL86  ", &
                         "WGS72 ",    "ECF   ",    "SPHERE" /)

  real(wp), PARAMETER  :: X(ND) = &
                 (/        0.0D0,       0.0D0,     375.0D0,     -87.0D0, &
                          -8.0D0,       0.0D0,       0.0D0,       0.0D0, &
                           0.0D0,       0.0D0,       0.0D0 /)

  real(wp), PARAMETER  :: Y(ND) = &
                 (/        0.0D0,       0.0D0,    -111.0D0,     -98.0D0, &
                         160.0D0,       0.0D0,       0.0D0,       0.0D0, &
                           0.0D0,       0.0D0,       0.0D0 /)

  real(wp), PARAMETER  :: Z(ND) = &
                 (/        0.0D0,       0.0D0,     431.0D0,    -121.0D0, &
                         176.0D0,       0.0D0,       0.0D0,       0.0D0, &
                           0.0D0,       0.0D0,       0.0D0 /)

  real(wp), PARAMETER  :: ae(ND) = &
                 (/  6378137.0D0,  6378135.3D0, 6377563.4D0, 6378388.0D0, &
                     6378206.4D0,  6378144.0D0, 6378137.0D0, 6367160.0D0, &
                     6378135.0D0, -6356750.0D0, 6367000.0D0 /)

  real(wp), PARAMETER  :: f1(ND) = &
                 (/  298.25722D0, 298.25722D0, 299.32496D0, 297.00000D0, &
                     294.97870D0, 298.25722D0, 298.25285D0, 298.25000D0, &
                     298.26000D0,       0.0D0,       0.0D0 /)

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN) :: Datum         ! Datum name
  real(wp), INTENT(OUT) :: delXYZ(:)     ! ecf x,y,z offsets
  real(wp), INTENT(OUT) :: a, f          ! semi-major axis & flattening param

! Local variables

  CHARACTER (LEN=6) :: D
  INTEGER   i

! Which datum?

  D = Datum
  IF ( D == " " ) D = "WGS84"
  i = 1
  DO WHILE ( i <= ND .AND. D /= DATUMS(i) )
    i = i + 1
  END DO

! Datum found

  IF ( i <= ND ) THEN
    delXYZ(1) = X(i)
    delXYZ(2) = Y(i)
    delXYZ(3) = Z(i)
    a         = ae(i)
    IF ( f1(i) > 0.0D0 ) THEN
      f      = 1.0D0 / f1(i)
    ELSE
      f      = 0.0D0
    END IF

! Datum not found

  ELSE
    delXYZ(:) = BIGNEG
    a         = BIGNEG
    f         = BIGNEG
  END IF

END SUBROUTINE Datum_Param
!---------------------------------------------------------------------------

SUBROUTINE ECFtoGEO ( ECF,   & ! (in)
                      Datum, & ! (in)
                      GEO,   & ! (out)
                      iflg )   ! (in)

!****s* Earth/ECFtoGEO *
!-----------------------------------------------------------------------
!
! NAME
!   ECFtoGEO     (earth.f90)
!
! SYNOPSIS
!   Convert between ECF coordinates & geodetic position
!
!   USE EarthMod
!   real(wp) :: EFC(3), GEO(3)
!   ECF(:) = (/ <X>, <Y>, <Z> /)   !   location wrt Earth's centre
!   CALL ECFtoGEO ( ECF, "WGS84", GEO, 1 )
!   [GEO contains lat/lon/ht on WGS-84 ellipsoid]
!
! ARGUMENTS
!   ECF    (inout)  dflt  Array(3) ECF x/y/z coordinates (m)
!   Datum  (in)     chr   Name of datum for geo coords
!   GEO    (inout)  dflt  Array(3) geodetic position
!                         Lat/lon (deg) & altitude (m)
!   iflg   (in)     int   Conversion flag:
!                          >  0 : ecf --> geo
!                          <= 0 : geo --> ecf
!
! CALLS
!   Datum_Param
!
! CALLED BY
!   Datum_Trans
!
! DESCRIPTION
!   Converts between earth-centred fixed (ECF), X,Y,Z coordinates
!   (in m from Earth's centre) and geodetic latitude, longitude
!   (deg) and altitude above the Earth's surface (m) relative to
!   the specified (ellipsoid) datum. The direction of conversion
!   depends on iflg.
!   Datum is specified as a character string for any of the supported
!   types:
!    "ED50"   - European datum, 1950
!    "GEM6"   - used for ERS orbits
!    "GRS80"  - Geodetic Reference System, 1980 [5]
!    "KL86"   - Kaye & Laby, 1986 [3]
!    "NAD27"  - North American Datum, 1927 (Clark 1866)
!    "OSGB36" - Airy 1936, UK National Grid (Ordnance Survey)
!    "OSU91"  - OSU, 1991-a
!    "WGS84"  - World Geodetic System 1984 [2]
!   plus the non-elliposid cases:
!    "SPHERE" - spherical earth (latitude independent)
!    "ECF"    - earth-centred fixed (x,y,z in m)
!   Datum names are case-insensitive. If datum is blank, WGS-84 is used
!
! REFERENCES
!   The 1992 Astronomical Almanac, page K12.
!   See also numbered references under routine Datum_Param
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
!
!
! COPYRIGHT
!   Language:		Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal

  IMPLICIT NONE

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)    :: Datum
  INTEGER,           INTENT(IN)    :: iflg
  real(wp),  INTENT(INOUT) :: ECF(:)
  real(wp),  INTENT(INOUT) :: GEO(:)

! Local variables

  INTEGER          :: COUNT
  real(wp) :: LAT, LON, HT
  real(wp) :: B, B2OA2, C, e2, PHI, R, SPHI, THETA, T1, T2
  real(wp) :: delXYZ(3), a, f, N

! Get parameters for specified 'from' datum

  CALL Datum_Param ( Datum, delXYZ, a, f )
  e2 = f * ( 2.0D0 - f )

! ============ ECF to GEO ===========

  IF ( iflg > 0 ) THEN

! Output 'to' datum was ECF too - no conversion, just copy

    IF ( f > -SMALL .AND. &
         a <  SMALL ) THEN
      GEO(:) = ECF(:)

! Valid datum

    ELSE IF ( f > SMALL ) THEN
      THETA = ATAN2 ( ECF(2), ECF(1) )
      LON   = MOD   ( THETA,  TwoPi  )

      R     = SQRT  ( ECF(1) * ECF(1) + ECF(2) * ECF(2) )
      LAT   = ATAN2 ( ECF(3), R )

      PHI   = LAT + 1.0D0
      COUNT = 0
      DO WHILE ( ABS ( LAT - PHI ) > SMALL .AND. &
                 COUNT < 10 )
        PHI   = LAT
        SPHI  = SIN ( PHI )
        C     = 1.0D0 / SQRT ( 1.0D0 - e2 * SPHI * SPHI )
        LAT   = ATAN2 ( ECF(3) + a * C * e2 * SPHI, R )
        COUNT = COUNT + 1
      END DO

      HT = R / COS ( LAT ) - a * C

! Convert geocentric latitude to geodetic...

      B     = ( 1D0 - f ) * a
      B2OA2 = ( B * B ) / ( a * a )
      LAT   = ATAN ( TAN ( LAT ) / B2OA2 )

! Convert Lat/Lon to degrees (Alt in m already)

      GEO(1) = LAT * RadToDeg
      IF ( GEO(1) > 180D0 ) GEO(1) = GEO(1) - 360D0

      GEO(2) = LON * RadToDeg
      IF ( GEO(2) > 180D0 ) THEN
           GEO(2) = GEO(2) - 360D0
      ELSE IF ( GEO(2) < -180D0 ) THEN
           GEO(2) = GEO(2) + 360D0
      END IF

      GEO(3) = HT

! Datum not found

    ELSE
      GEO(:) = BIGNEG

    END IF

! ========== GEO to ECF ============

  ELSE

! Input datum was ECF too - no conversion, just copy

    IF ( f > -SMALL .AND. &
         a <  SMALL ) THEN
      ECF(:) = GEO(:)

! Valid datum to convert

    ELSE IF ( f > SMALL ) THEN

! Convert geodetic latitude to geocentric...

      B     = ( 1.0D0 - f ) * a
      B2OA2 = ( B * B ) / ( a * a )
      LAT   = ATAN ( B2OA2 * TAN ( GEO(1)*DegToRad ) )

! ...and geocentric to x,y,z.

      T1 = SIN ( LAT ) ** 2
      N  = a / SQRT ( 1.0D0 - e2 * T1 )
      T2 = ( N + GEO(3) ) * COS ( LAT )

      ECF(1) = T2 * COS ( GEO(2)*DegToRad )
      ECF(2) = T2 * SIN ( GEO(2)*DegToRad )
      ECF(3) = ( N * ( 1.0D0 - e2 ) + GEO(3) ) * SIN ( LAT )

! Datum not found

    ELSE
      ECF(:) = BIGNEG

    END IF

  END IF

END SUBROUTINE ECFtoGEO
!----------------------------------------------------------------------

SUBROUTINE Datum_HMSL ( Datum,  & ! (in)
                        GEO,    & ! (in)
                        HMSL,   & ! (out)
                        FCOEFF, & ! (in, optional)
                        FCORR )   ! (in, optional)

!****s* Earth/Datum_HMSL *
!-----------------------------------------------------------------------
!
! NAME
!   Datum_HMSL     (earth.f90)
!
! SYNOPSIS
!   Convert height on datum to geoid (msl)
!
!   USE EarthMod
!   real(wp) :: GEO(3), HMSL
!   GEO(:) = (/ <lat>, <lon>, <ht> /) ! on WGS-84 ellipsoid
!   CALL Datum_HMSL ( "WGS84", GEO, HMSL )
!
! ARGUMENTS
!   Datum  (in)   chr   Name of datum
!   GEO    (in)   dflt  Point coordinates wrt datum
!                       for ellipsoid: lat/lon (deg)/ht (m)
!                       for ECF: x/y/z (m)
!   HMSL   (out)  dflt  Height above geoid (msl)
!   FCOEFF (in, optional) chr Path to geoid potential coeffs file
!   FCORR  (in, optional) chr Path to geoid potential corrections file
!
! ENVIRONMENT VARIABLES
!   GEOPOT_COEF - path to geoid potential coeffs file (if not specified arg)
!   GEOPOT_CORR - path to geoid corrections file (if not specified arg)
!
! CALLS
!   Datum_Param
!   Datum_Trans
!   GEOPOT_LEGDFN
!
! USES
!
! ALGORITHM
!   Uses NIMA EGM96 geoid coefficients wrt WGS-84 datum. These are
!   in the form of geoid potential and correction coefficients to
!   order and degree 360. The coefficients are expanded as Legendre
!   polynomials and applied to the given location to calculate the
!   local EGM96 undulation wrt WGS-84; the location Datum is also
!   converted to the same ellipsoid if not already in that system.
!   The HMSL value (strictly orthometric height) is simply equal to
!   WGS-84 minus EGM96 heights.
!   This routine can be validated by comparing its output with the
!   online calculator linked from the NIMA/NASA website URL noted
!   in Refs.
!
! DESCRIPTION
!   Given the location of a point wrt an ellipsoid datum (e.g. WGS-84),
!   or ECF X,Y,Z coordinates, return the location's height above mean
!   sea level. MSL here is assumed to correspond with the local
!   geoid undulation, given by a geoid model. The potential coeffs.
!   for this model are read from a file pointed to by the environment
!   variable 'GEOPOT_COEF' and the corrections file from 'GEOPOT_CORR'.
!   Alternatively, it is possible to specify the files by passing optional
!   arguments FCOEF and FCORR.
!
! REFERENCES
!   Original package from Ohio State University for OSU91.
!   EGM96 coefficients files can be obtained from the National Image
!   and Mapping Agency (NIMA/NASA) website at:
!     http://earth-info.nga.mil/GandG/wgs84/gravitymod/egm96/egm96.html
!   For more information on EGM96, see also:
!     http://cddis.gsfc.nasa.gov/926/egm96/egm96.html
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.                  D. Offiler
! 1.1-0   27-Jun-2002   NMAX corrected from 36 to 360 and
!                       detection logic for last coeff.
!                       in each file modified.              D. Offiler
! 2.0-0   22-Apr-2009   Add options to specify and search
!                       correction and coefficient files
!                       in specified and default search paths H. Lewis
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal
  use messages

! Constants

! - possible file paths for coefficients files
  CHARACTER(len=27), PARAMETER ::  &
       efilepath(9) =  (/"data/egm96.dat             ", &
                         "../data/egm96.dat          ", &
                         "../../data/egm96.dat       ", &
                         "../../../data/egm96.dat    ", &
                         "../../../../data/egm96.dat ", &
                         "*/data/egm96.dat           ", &
                         "*/*/data/egm96.dat         ", &
                         "*/*/*/data/egm96.dat       ", &
                         "*/*/*/*/data/egm96.dat     " /)

! - possible file paths for corrections files
  CHARACTER(len=30), PARAMETER ::  &
       cfilepath(9) =  (/"data/corrcoef.dat             ", &
                         "../data/corrcoef.dat          ", &
                         "../../data/corrcoef.dat       ", &
                         "../../../data/corrcoef.dat    ", &
                         "../../../../data/corrcoef.dat ", &
                         "*/data/corrcoef.dat           ", &
                         "*/*/data/corrcoef.dat         ", &
                         "*/*/*/data/corrcoef.dat       ", &
                         "*/*/*/*/data/corrcoef.dat     " /)

! - shell environment variables for potential coefficients & corrections files

  CHARACTER (LEN=*), PARAMETER :: COEF_env = "GEOPOT_COEF"
  CHARACTER (LEN=*), PARAMETER :: CORR_env = "GEOPOT_CORR"

! - array size values

  INTEGER,           PARAMETER :: NDEG  = 360
  INTEGER,           PARAMETER :: NUM   = ((NDEG+1)*(NDEG+2))/2
  INTEGER,           PARAMETER :: NMAX  =  360               ! max. order of Legendre poly. (1.1) was 36
  INTEGER,           PARAMETER :: PUNIT =  42                ! input file stream unit no.

! - constants

! real(wp),  PARAMETER :: RHO  = RadToDeg * 3600.0D0
  real(wp),  PARAMETER :: GM   = 3.986004418D14      ! Earth's gravitational constant (m^3/s^2)
! real(wp),  PARAMETER :: OM   = 7.2921151467D-5     ! Earth's angular velocity (rad/s)
  real(wp),  PARAMETER :: GEQT = 9.7803253359D0      ! Earth's gravity at the Equator (m/s^2)
  real(wp),  PARAMETER :: GK   = 0.00193185265246D0  ! Gravity coefficient with latitude

! The even degree zonal coefficients given below were computed for the
! WGS84(G873) system of constants and are identical to those values
! used in the NIMA gridding procedure. Computed using subroutine
! GRS written by N.K. Pavlis

  real(wp), PARAMETER :: RJ2  =  0.108262982131D-2
  real(wp), PARAMETER :: RJ4  = -0.237091120053D-5
  real(wp), PARAMETER :: RJ6  =  0.608346498882D-8
  real(wp), PARAMETER :: RJ8  = -0.142681087920D-10
  real(wp), PARAMETER :: RJ10 =  0.121439275882D-13

! Argument list parameters

  CHARACTER (LEN=*), INTENT(IN)  :: Datum       ! Datus name
  real(wp),  INTENT(IN)  :: GEO(:)      ! Coordinates in datum
  real(wp),  INTENT(OUT) :: HMSL        ! Orthometric height (~ht above msl)
  CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: FCOEFF   ! Coeff file name
  CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: FCORR    ! Corrections file name

! Local variables

  CHARACTER (LEN=255) :: FileName

  INTEGER     :: I, J, K, LOC, N, M, ierr, ierr1, ierr2
  real(wp)    :: F, E2, delXYZ(3), GEO2(3)
  real(wp)    :: GR, RLAT, COLAT
  real(wp)    :: T1, T2, C, S, RE, X, Y, Z, U
  real(wp)    :: CLON, SLON

  real(wp)    :: P(NUM), HC(NUM), HS(NUM), CC(NUM), CS(NUM)
  real(wp)    :: RLEG(NDEG+1)
  real(wp)    :: SINML(NDEG+1), COSML(NDEG+1)
  real(wp)    :: A, AC, AE, AR, ARN, HACO
  real(wp)    :: SUM, SUMC, TEMP, TEMPC

  LOGICAL :: FIRST_CALL = .TRUE.
  LOGICAL :: NO_COEFFS  = .TRUE.
  LOGICAL :: FOUND      = .TRUE.

  SAVE FIRST_CALL, NO_COEFFS
  SAVE AE, E2, F
  SAVE RLEG, SINML, COSML
  SAVE P, HC, HS, CC, CS

! Initialise

  IF ( FIRST_CALL ) THEN
    SINML(:) = 0.0D0
    COSML(:) = 0.0D0
    P(:)     = 0.0D0
    HC(:)    = 0.0D0
    HS(:)    = 0.0D0
    CC(:)    = 0.0D0
    CS(:)    = 0.0D0

!  WGS-84 'best ellipsoid' parameters

    CALL Datum_Param ( "WGS84", delXYZ, AE, F )
    E2 = F * ( 2.D0 - F )

!  Read the potential coefficients

    IF (PRESENT(FCOEFF)) THEN
      FileName = TRIM(FCOEFF)
    ELSE
      CALL GETENV ( COEF_env, FileName )
    ENDIF

    INQUIRE(File=FileName, Exist=Found)

    IF ( (.NOT. Found) .OR. (TRIM(ADJUSTL(FileName)) == '') ) THEN
      DO i=1,9
        INQUIRE(File=efilepath(i), Exist=Found)
        IF (Found) THEN
          FileName = efilepath(i)
          EXIT
        ENDIF
      ENDDO
    ENDIF

    OPEN ( UNIT=PUNIT,    &
         STATUS="OLD",    &
           FILE=FileName, &
         ACTION="READ",   &
         IOSTAT=ierr1 )

    IF (ierr1 /= 0) THEN

      CALL message ( msg_warn, 'Cannot open ' //                        &
        TRIM(ADJUSTL(COEF_env)) // ' = ' // TRIM(ADJUSTL(FileName)) //  &
        ' ... will return missing HMSL' )

      ierr = ierr1

    ELSE

      DO
        READ ( UNIT=PUNIT, &
                FMT=*,     &
             IOSTAT=ierr ) N, M, C, S
        IF ( ierr /= 0 ) EXIT
        N = ( N * ( N + 1 ) ) / 2 + M + 1
        HC(N) = C
        HS(N) = S
      END DO

      CLOSE ( PUNIT )

      HC(4)  = HC(4)  + RJ2  / SQRT(5.D0)
      HC(11) = HC(11) + RJ4  / 3.D0
      HC(22) = HC(22) + RJ6  / SQRT(13.D0)
      HC(37) = HC(37) + RJ8  / SQRT(17.D0)
      HC(56) = HC(56) + RJ10 / SQRT(21.D0)

    ENDIF

    ierr1 = ierr

! Read the correction coefficients

    IF (PRESENT(FCORR)) THEN
      FileName = TRIM(FCORR)
    ELSE
      CALL GETENV ( CORR_env, FileName )
    ENDIF

    INQUIRE(File=FileName, Exist=Found)

    IF ( (.NOT. Found) .OR. (TRIM(ADJUSTL(FileName)) == '') ) THEN
      DO i=1,9
        INQUIRE(File=cfilepath(i), Exist=Found)
        IF (Found) THEN
          FileName = cfilepath(i)
          EXIT
        ENDIF
      ENDDO
    ENDIF

    OPEN ( UNIT=PUNIT,    &
         STATUS="OLD",    &
           FILE=FileName, &
         ACTION="READ",   &
         IOSTAT=ierr2 )

    IF (ierr2 /= 0) THEN

      CALL message ( msg_warn, 'Cannot open ' //                        &
        TRIM(ADJUSTL(CORR_env)) // ' = ' // TRIM(ADJUSTL(FileName)) //  &
        ' ... will return missing HMSL' )

      ierr = ierr2

    ELSE

      DO
        READ ( UNIT=PUNIT, &
                FMT=*,     &
             IOSTAT=ierr ) N, M, C, S
        IF ( ierr /= 0 ) EXIT
        N = ( N * ( N + 1 ) ) / 2 + M + 1
        CC(N) = C
        CS(N) = S
      END DO

      CLOSE ( PUNIT )

    ENDIF

    ierr2 = ierr

! Have we found coeffs and their corrections?
    IF ( ierr1 <= 0 .AND. ierr2 <= 0 ) NO_COEFFS = .FALSE.

    FIRST_CALL = .FALSE.

  END IF

!  Convert coordinates on given datum or in ECF to WGS-84 ellipsoid datum

  CALL Datum_Trans ( Datum, GEO, "WGS84", GEO2 )

  IF ( NO_COEFFS .OR. GEO2(1) <= BIGNEG ) THEN

    HMSL = BIGNEG

!  Compute geocentric radius, geocentric latitude and normal gravity with
!  approximate elevation correction

  ELSE

    T1   = ( SIN ( GEO2(1)*DegToRad ) )**2
    N    = AE / SQRT ( 1.D0 - E2 * T1 )
    T2   = ( N + GEO2(3) ) * COS ( GEO2(1)*DegToRad )
    X    = T2 * COS ( GEO2(2)*DegToRad )
    Y    = T2 * SIN ( GEO2(2)*DegToRad )
    Z    = ( N * ( 1.D0 - E2 ) + GEO2(3) ) * SIN ( GEO2(1)*DegToRad )
    RE   = SQRT ( X*X + Y*Y + Z*Z )
    RLAT = ATAN ( Z / SQRT ( X*X + Y*Y ) )
    GR   = GEQT * ( 1.D0 + GK * T1 ) / SQRT ( 1.D0 - E2 * T1 )
    GR   = GR - GEO2(3) * 0.3086D-05

!  Compute the Legendre polynomials

    COLAT = HalfPi - RLAT            ! Co-latitude
    DO J = 1, NMAX+1
      M = J - 1
      CALL Geopot_LegFn ( M, COLAT, NMAX, RLEG )
      DO I = J, NMAX+1
        N = I - 1
        LOC = ( N * ( N + 1 ) ) / 2 + M + 1
        P(LOC) = RLEG(I)
      END DO
    END DO

!  Compute the sin and cos of longitude

    SLON = SIN ( GEO2(2)*DegToRad )
    CLON = COS ( GEO2(2)*DegToRad )

    SINML(1) = SLON
    COSML(1) = CLON
    SINML(2) = 2.D0 * CLON * SLON
    COSML(2) = 2.D0 * CLON * CLON - 1.D0

    DO M = 3, NMAX
      SINML(M) = 2.D0 * CLON * SINML(M-1) - SINML(M-2)
      COSML(M) = 2.D0 * CLON * COSML(M-1) - COSML(M-2)
    END DO

!  Compute the geoid undulation

    AR   = AE / RE
    ARN  = AR
    SUM  = 0.D0
    SUMC = 0.D0
    A    = 0.D0
    AC   = 0.D0
    K    = 3

    DO N = 2, NMAX
      ARN  = ARN * AR
      K    = K + 1
      SUM  = P(K) * HC(K)
      SUMC = P(K) * CC(K)

      DO M = 1, N
        K    = K + 1
        TEMP  = HC(K) * COSML(M) + HS(K) * SINML(M)
        TEMPC = CC(K) * COSML(M) + CS(K) * SINML(M)
        SUM   = SUM  + P(K) * TEMP
        SUMC  = SUMC + P(K) * TEMPC
      END DO

      A  = A  + SUM * ARN
      AC = AC + SUMC
    END DO

    U = A * GM / ( GR * RE )

! Add HACO to convert height anomaly on ellipsoid to undulation
! Add -0.53m to make undulation refer to WGS-84 ellipsoid

    HACO = ( AC + CC(1) + P(2) * CC(2) &
                + P(3) * ( CC(3) * COSML(1) + CS(3) * SINML(1) ) ) &
              / 100.D0
    U    = U + HACO - 0.53D0

!  Compute an approximation of height above mean sea level
!  (strictly orthometric height above geoid)
!  by subtracting the geoid undulation from the ellipsoid
!  height. Note that this only works because we converted
!  the inputs to the same ellipsoid as used for the geoid

    HMSL = GEO2(3) - U

  END IF

END SUBROUTINE Datum_HMSL
!-----------------------------------------------------------------------

SUBROUTINE Geopot_LegFn ( M,     & ! (in)
                          COLAT, & ! (in)
                          NMAX,  & ! (in)
                          RLEG )  ! (out)

!****s* Earth/Geopot_LegFn *
!-----------------------------------------------------------------------
!
! NAME
!   Geopot_LegFn     (earth.f90)
!
! SYNOPSIS
!   Compute legendre polynomials for geopotential
!
!   USE EarthMod
!   real(wp) :: COLAT, RLEG(NMAX+1)
!   INTEGER          :: M, NMAX
!   CALL Geopot_LegFn ( M, COLAT, NMAX, RLEG )
!
! ARGUMENTS
!   M      (in)   int   Order of legendre functions
!   COLAT  (in)   dflt  Co-latitude (rad)
!   NMAX   (in)   int   Maximum degree of functions
!   RLEG   (out)  dflt  Array(nmax+1) of legendre function values
!
! CALLED BY
!   Datum_HMSL
!
! DESCRIPTION
!   This subroutine computes all normalized Legendre functions in RLEG
!   for order M and co-latitude COLAT (radians). Maximum degree is NMAX.
!   The dimensions of array RLEG must be at least equal to NMAX+1.
!   Original algorithm & code from Ohio State University.
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
!
!
! COPYRIGHT
!   Language:		Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  use typesizes, only: wp => EightByteReal

  IMPLICIT NONE

! Constants

  INTEGER, PARAMETER :: NDEG = 360

! Argument list parameters

  INTEGER,          INTENT(IN)  :: M           ! order of poly
  INTEGER,          INTENT(IN)  :: NMAX        ! max degree
  real(wp), INTENT(IN)  :: COLAT       ! co-latitude
  real(wp), INTENT(OUT) :: RLEG(:)     ! poly values

! Local variables

  INTEGER          :: NMAX1            ! NMAX+1
  INTEGER          :: M1, M2, M3       ! M+1, M+2, M+3
  INTEGER          :: N, N1, N2        ! counters
  real(wp) :: COTHET, SITHET           ! cosine & sine of colat
  real(wp) :: DIRT(2*NDEG+1)           ! constant coefficients
  real(wp) :: DRTS(2*NDEG+1)           ! constant coefficients
  real(wp) :: RLNN(NDEG+1)             ! constant coefficients

  LOGICAL :: FIRST_CALL = .TRUE.

  SAVE DRTS, DIRT, RLNN
  SAVE FIRST_CALL

  IF ( FIRST_CALL ) THEN
    RLEG(:) = 0.0D0
    RLNN(:) = 0.0D0
    DO N1 = 1, 2*NMAX+1
      DRTS(N1) = SQRT ( DBLE(N1) )
      DIRT(N1) = 1.0D0 / DRTS(N1)
    END DO
    FIRST_CALL = .FALSE.
  ENDIF

  NMAX1  = NMAX + 1
  M1     = M + 1
  M2     = M + 2
  M3     = M + 3

  COTHET = COS ( COLAT )
  SITHET = SIN ( COLAT )

!  Compute the Legendre functions

  RLNN(1) = 1.0D0
  RLNN(2) = SITHET * DRTS(3)

  DO N1 = 3, M1
    N        = N1 - 1
    N2       = 2 * N
    RLNN(N1) = DRTS(N2+1) * DIRT(N2) * SITHET * RLNN(N1-1)
  END DO

  IF ( M == 0 ) THEN
    RLEG(1) = 1.0D0
    RLEG(2) = COTHET * DRTS(3)
  ELSE IF ( M == 1 ) THEN
    RLEG(2) = RLNN(2)
    RLEG(3) = COTHET * DRTS(5) * RLEG(2)
  END IF
  RLEG(M1) = RLNN(M1)

  IF ( M2 <= NMAX1 ) THEN
    RLEG(M2) = DRTS(M1*2+1) * COTHET * RLEG(M1)

    IF ( M3 <= NMAX1 ) THEN
      DO N1 = M3, NMAX1
        N = N1 - 1
        IF ( ( M == 0 .AND. N < 2 ) .OR. &
             ( M == 1 .AND. N < 3 ) ) CYCLE

        N2 = 2 * N
        RLEG(N1) = DRTS(N2+1)  * DIRT(N+M) * DIRT(N-M)  &
               * ( DRTS(N2-1)  * COTHET    * RLEG(N1-1) &
                 - DRTS(N+M-1) * DRTS(N-M-1)            &
                 * DIRT(N2-3)  * RLEG(N1-2) )
      END DO
    END IF

  ENDIF

END SUBROUTINE Geopot_LegFn

REAL FUNCTION EradL ( Latitude ) ! (in)


!****s* Earth/EradL *
!-----------------------------------------------------------------------
!
! NAME
!   EradL     (earth.f90)
!
! SYNOPSIS
!   Calculate the Earth's radius as a simple function of latitude.
!
!   USE EarthMod
!   REAL Re
!   Re = EradL ( 45.0 )
!
! ARGUMENTS
!   Latitude  (in)  flt  Latitude (deg)
!
! OUTPUTS
!   EradL   (func)  flt  Earth's radius at given latitude (km)
!
! CALLED BY
!   LLtoDB
!
! ALGORITHM
!   re(lat) = r0 + r1.cos(2.lat) * r2.cos(4.lat)
!
! DESCRIPTION
!   Returns radius of earth given the latitude (north or south).
!   Uses a simple non-ellipsoid model. For ellipsoid radius, see
!   Datum_Erad
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
!
!
! COPYRIGHT
!   Language:           Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  IMPLICIT NONE

! Argument list parameters

  REAL, INTENT(IN) :: Latitude

  EradL = 6367.47 + 10.6924 * COS ( 2.0 * Latitude * DegToRad ) &
                     -0.022 * COS ( 4.0 * Latitude * DegToRad )

END FUNCTION EradL

!--------------------------------------------------------------------------

REAL FUNCTION EGravity ( Latitude, &  ! (in)
                        Height )     ! (in)

!****s* Earth/EGravity *
!-----------------------------------------------------------------------
!
! NAME
!   EGravity     (earth.f90)
!
! SYNOPSIS
!   Gravity (m.s^-2) as a function of latitude (deg) and height
!   (m amsl).
!
!   USE EarthMod
!   REAL :: g
!   g = EGravity ( 50.0, 5000.0 )
!
! ALGORITHM
!   g = g0[1-c1.cos(lat)-c2.ht]  (see ref.)
!
! DESCRIPTION
!   Calculates gravity, g, as a simple function of latitude and
!   height above a reference surface (nominally WGS-84).
!
! AUTHOR
!   Dave Offiler
!
! REFERENCES
!   Baker, HC (1998): GPS water vapour estimation for meteorological
!          applications. PhD Thesis, University of Nottingham.
!
! MODIFICATION HISTORY
!
! Version Date          Comment
! ------- ----          -------
! 1.0-0   08-Jan-2002   First f90 release.            D. Offiler
!
!
! COPYRIGHT
!   Language:		Fortran 90
!   Software Standards: GTDP 8
!
! (c) CROWN COPYRIGHT 2013, Met Office, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Met Office, FitzRoy Road,
!                Exeter, Devon, EX1 3PB, UK
!
! If no contract has been raised with this copy of the code, the use,
! duplication and disclosure of it is strictly prohibited. Permission
! to do so must first be obtained in writing, from the Head of Satellite
! Applications at the above address.
!
!-----------------------------------------------------------------------
!****

  IMPLICIT NONE

! Argument list parameters

  REAL, INTENT(IN) :: Latitude  ! Geocentric latitude (deg)
  REAL, INTENT(IN) :: Height    ! Orthometric height  (m amsl)

! Local constants

  REAL, PARAMETER :: g0  = 9.784      ! reference gravity (at msl & Equator)
  REAL, PARAMETER :: c1  = 0.00266    ! variation w.r.t. latitude
  REAL, PARAMETER :: c2  = 0.00028    ! variation w.r.t. height
  REAL, PARAMETER :: d2r = 0.017453   ! Radians to degrees

  EGravity = g0 * ( 1.0 - c1 * COS ( Latitude * d2r ) - c2 * Height )

END FUNCTION EGravity
!------------------------------------------------------------------

END MODULE EarthMod
