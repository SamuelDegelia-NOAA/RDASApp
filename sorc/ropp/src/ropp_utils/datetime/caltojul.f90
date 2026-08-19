! $Id$

!****s* Datetime/CalToJul *
!
! NAME
!   CalToJul  (caltojul.f90)
!
! SYNOPSIS
!   Subroutine to convert between calendar date/time &
!   Julian date & fraction
!
!   REAL(dp) :: JDF
!   INTEGER  :: CDT(8), inv
!   CALL CalToJul ( CDT, JDF, inv )
!
! INPUTS
!   CDT  int   Array(8): Calendar date & clock time [if inv>0]
!   JDF  dflt  Julian date & fraction (UTC)         [if inv<=0]
!   inv  int   Indicator for direction of conversion
!               > 0 : CDT --> JDF
!              <= 0 : JDF --> CDT
!
! OUTPUTS
!   CDT  int   Array(8): Calendar date & clock time [if inv<=0]
!   JDF  dflt  Julian date & fraction (UTC)         [if inv>0]
!
! DESCRIPTION
!   Converts between:
!     - CDT: Gregorian calendar date & clock time as an 8-element array
!       (year,month,day,zone,hour,minute,second,millisecond)
!       as returned by the F90 DATE_AND_TIME(VALUE=array) intrinsic
!   and:
!     - JDF: Julian Date (UTC, in days and fractions) [See NOTES]
!   or vice-versa.
!   inv indicates the direction of conversion: >0 for CDT-->JDF else
!   JDF-->CDT.
!   The zone parameter (CDT(4) - offset from UTC in minutes) is
!   applied to CDT-->JDF conversions so that JDF is always in UTC.
!   JDF-->CDT conversions assume JDF is in UTC and CDT(4) is
!   applied to obtain the appropriate time zone (all elements of CDT
!   except CDT(4) are overwritten). Set CDT(4)=0 to keep the
!   JDF-->CDT conversion in UTC.
!   NB JDF argument's KIND must be declared equivalent to
!   DOUBLE PRECISION (REAL*8).
!   The Date/Julian day number conversions apply the algorithms from
!   Ref.1. All dates are assumed to be in the Gregorian Calendar.
!
! NOTES
!   From Ref.2, Julian period or dates:
!   - Number of consecutive days since 1-Jan-4713 BC.
!   - Devised by Joseph Justus Scaliger in 1582. Thought to be named after
!     his father, Julius Caesar Scaliger, but some now think it named
!     after the Julian calendar (ie after Julius Caesar) - see Ref.3.
!   - Based on 'cycles of history' of period 7980 years, being the
!     product of:
!      28 = so-called solar cycle in Julian calendar
!      19 = lunar or metonic cycle
!      15 = 'cycle of indiction', an ancient Roman taxation period.
!     4713 BC was the nearest past year on which all three cycles started
!     together. See Refs.2,3.
!   - By convention, Julian dates begin at midday, the astronomical start
!     of the day (Ref.3)
!   - Epochs other than 12:00 1-Jan-4713 BC may be used, but these strictly
!     should not be called 'Julian' dates. See the TimeSince() subroutine in
!     this package.
!
! REFERENCES
!   1) Meeus, Jan (1998). Astronomical Algorithms.
!      2nd ed., Willmann-Bell Inc, ISBN 0-943396-61-1
!   2) Encyclpaedia Brittanica, Microcropaedia Vol.V,
!      15th Ed. 1975, p.632.
!   3) Wikipaedia: http://en.wikipedia.org/wiki/Julian_Date
!
! SEE ALSO
!   TimeSince()
!
! AUTHOR
!    D. Offiler, Met Office
!
! COPYRIGHT
!
!    Crown copyright 2013, Met Office. All rights reserved.
!
!    Use, duplication or disclosure of this code is subject to the restrictions
!    as set forth in the contract. If no contract has been raised with this
!    copy of the code, the use, duplication or disclosure of it is strictly
!    prohibited. Permission to do so must first be obtained in writing from
!    the Head of Satellite Applications at the following address:
!       Met Office, FitzRoy Road, Exeter, Devon, EX1 3PB  United Kingdom
!
!****

SUBROUTINE CalToJul ( CDT, & ! (inout)
                      JDF, & ! (inout)
                      inv )  ! (in)

  USE DateTimeTypes

  IMPLICIT NONE

! Units-to-Day conversion factors

  REAL(dp), PARAMETER :: one = 1.0_dp            ! Unity
  REAL(dp), PARAMETER :: H2D = one / 24.0_dp     ! Hours   to Days
  REAL(dp), PARAMETER :: M2D = H2D / 60.0_dp     ! Minutes to Days
  REAL(dp), PARAMETER :: S2D = M2D / 60.0_dp     ! Seconds to Days
  REAL(dp), PARAMETER :: T2D = S2D / 1000.0_dp   ! Msecs   to Days
  REAL(dp), PARAMETER :: hmd = 0.5_dp * T2D      ! Half a Msec in Days

! Argument list parameters

  REAL(dp), INTENT(INOUT) :: JDF                 ! Julian Date & Fraction
  INTEGER,  INTENT(INOUT) :: CDT(:)              ! Calendar date/time (dim>=8)
  INTEGER,  INTENT(IN)    :: inv                 ! inversion flag

! Local variables

  REAL(dp) :: jd, time, Zoffset
  INTEGER  :: Yr, Mo
  INTEGER  :: a, b, c, d, e, z, alpha

!---------------------------------------------------------------
! 1. Check for given valid zone offset value (in minutes)
!    is between -11h30m and +12h00m. Default to UTC if not.
!---------------------------------------------------------------

  IF ( CDT(IdxZone) >= -690 .AND. &
       CDT(IdxZone) <=  720 ) THEN
    Zoffset = CDT(IdxZone) * M2D
  ELSE
    Zoffset = 0
  ENDIF

!---------------------------------------------------------------
! 2. Convert Calendar Date/Time to Julian Day - sum of:
!    a) Julian Day Number at midday of given date (see Ref.1)
!    b) fraction of given time into the given day, with
!       adjustment to UTC
!---------------------------------------------------------------

  IF ( inv > 0 ) THEN
    Yr = CDT(IdxYear)
    Mo = CDT(IdxMonth)
    IF ( Mo < 3 ) THEN
      Yr = Yr - 1
      Mo = Mo + 12
    END IF
    a = INT ( Yr / 100 )
    b = 2 - a + ( a / 4 )
    JDF = INT ( 365.25_dp  * ( Yr + 4716 ) ) &
        + INT ( 30.6001_dp * ( Mo + 1 ) )    &
        + CDT(IdxDay) + b - 1524.5_dp        &
        - Zoffset                            &
        + CDT(IdxHour)   * H2D               &
        + CDT(IdxMinute) * M2D               &
        + CDT(IdxSecond) * S2D               &
        + CDT(IdxMSec)   * T2D

!---------------------------------------------------------------
! 3. Convert Julian Day to Calendar, separately:
!    a) adjust to local time zone & Jul midday --> Cal midnight
!    b) calendar date from integer portion of Julian date (see Ref.1)
!    c) clock time from fractional portion of Julian date
!---------------------------------------------------------------

  ELSE
    jd = JDF + Zoffset + HalfDay

    z = INT ( jd )
    alpha = INT ( ( z - 1867216.25_dp) / 36524.25_dp )
    a = z + 1 + alpha - alpha / 4
    b = a + 1524
    c = INT ( ( b - 122.1_dp ) / 365.25_dp )
    d = INT ( 365.25_dp * c )
    e = INT ( ( b - d ) / 30.6001_dp )

    CDT(IdxDay) = b - d - INT ( 30.6001_dp * e )

    IF ( e < 14 ) THEN
      CDT(IdxMonth) = e - 1
    ELSE
      CDT(IdxMonth) = e - 13
    END IF

    IF ( CDT(IdxMonth) > 2 ) THEN
      CDT(IdxYear) = c - 4716
    ELSE
      CDT(IdxYear) = c - 4715
    END IF

    time           = 24_dp   * MOD ( jd+hmd, one)
    CDT(IdxHour)   = INT ( time )
    time           = 60_dp   * MOD ( time, one )
    CDT(IdxMinute) = INT ( time )
    time           = 60_dp   * MOD ( time, one )
    CDT(IdxSecond) = INT ( time )
    time           = 1000_dp * MOD ( time, one )
    CDT(IdxMSec)   = INT ( time )

  END IF

END SUBROUTINE CalToJul
