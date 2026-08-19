! $Id: leapseconds.f90 3696 2013-06-17 08:48:37Z idculv $

!****f* DateTime/LeapSeconds *
!-----------------------------------------------------------------------
! NAME
!    LeapSeconds   (leapseconds.f90)
!
! SYNOPSIS
!   Return accumulated leap seconds up to the given UTC datetime, expressed
!   as a length-8 integer array (/ YYYY, MM, DD, Z, hh, mm, ss, msec /).
!
! INPUTS
!   UTC   int(8)  input UTC
!
! OUTPUTS
!   lsecs int  Accumulated leap seconds up to UTC
!
! DESCRIPTION
!   Leap seconds are added (or subtracted) to keep civil time (UTC)
!   synchronized with astronomical time (TAI), see Ref.1. Leap seconds are
!   applied only at 23:59:60 UTC on 30 June and/or 31 December in any year,
!   though not every year has any leap seconds. The International Earth
!   Rotation Service (IERS) determines the timing of leap seconds and
!   generally gives advance notice of about 6 months. Since part of the
!   variability in the Earth's rotation is due to unpredictable internal
!   tides in the molten core, it is not possible to give any longer forecast
!   of when leap seconds may be required.
!   This function sums the leap seconds applied up to the datetime supplied
!   as the argument (calendar date/time as a Julian Day and fraction).
!
! NOTES
!   1. The internal list of dates when leap seconds have been applied will
!      need to be updated as and when IERS announce new leap seconds.
!   2. All leap seconds so far have been positive; negative ones are
!      theoretically possible, though very unlikely to occur.
!   3. There is a residual and constant 10-second offset between UTC and TAI.
!   4. GPS time does not apply leap seconds; as of Jan 2018, there is an 18
!      second difference between GPS and UTC (28 secs TAI-UTC).
!   5. Internal calculations are based on Julian days, which are calculated
!      without accounting for leapseconds.  This doesn't matter because we
!      always compare two similarly calculated Julian days.
!
! EXAMPLES
!   1. UTC = (/ 2005, 12, 31,  0, 23, 59, 59, 999 /)
!      lpsec = LeapSeconds(UTC)
!      ==> lpsec = 22
!
!   2. UTC = (/ 2005, 12, 31,  0, 23, 59, 60, 000 /)
!      lpsec = LeapSeconds(UTC)
!      ==> lpsec = 23
!
!   3. UTC = (/ 2006,  1,  1,  0,  0,  0,  0,   0 /)
!      lpsec = LeapSeconds(UTC)
!      ==> lpsec = 23
!
!   4. UTC = (/ 2006,  1,  1,  0,  0,  0,  0, 001 /)
!      lpsec = LeapSeconds(UTC)
!      ==> lpsec = 23
!
! REFERENCES
!   1. Wikipedia http://en.wikipedia.org/wiki/Leap_second
!
! SEE ALSO
!    CalToJul()
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
!-----------------------------------------------------------------------
!****
FUNCTION LeapSeconds ( UTC ) RESULT ( lsecs )

  USE datetimetypes
  USE datetimeprogs, not_this => LeapSeconds

  IMPLICIT NONE

! Fixed parameters

! These numbers are valid for JDFs up to cdtls(nls), i.e. the final one.
! Check Ref. 1 to see if these tables need augmenting (in the obvious way)
! for the next release of ROPP.

  INTEGER, PARAMETER :: nls = 27  ! Number of leap dates

  CHARACTER(LEN=10), PARAMETER :: cdtls(nls) = &  ! Leap days in UTC (Ref. 1)
            (/ '1972-06-30', '1972-12-31', '1973-12-31', '1974-12-31', &
               '1975-12-31', '1976-12-31', '1977-12-31', '1978-12-31', &
               '1979-12-31', '1981-06-30', '1982-06-30', '1983-06-30', &
               '1985-06-30', '1987-12-31', '1989-12-31', '1990-12-31', &
               '1992-06-30', '1993-06-30', '1994-06-30', '1995-12-31', &
               '1997-06-30', '1998-12-31', '2005-12-31', '2008-12-31', &
               '2012-06-30', '2015-06-30', '2016-12-31' /)

  INTEGER, PARAMETER :: incls(nls) = &  ! Increments (secs) (Ref. 1)
            (/            1,            1,            1,            1, &
                          1,            1,            1,            1, &
                          1,            1,            1,            1, &
                          1,            1,            1,            1, &
                          1,            1,            1,            1, &
                          1,            1,            1,            1, &
                          1,            1,            1 /)

! Argument list parameters

  INTEGER, INTENT(IN)  :: UTC(8)         ! UTC Datetime array
  INTEGER              :: lsecs          ! Leap Seconds (function return)

! Local variables

  CHARACTER(LEN=10)    :: SDT            ! CDT of leap second dates (string)
  INTEGER              :: CDT(8)         ! CDT of leap second dates
  REAL(dp)             :: jdf            ! Julian day
  REAL(dp), SAVE       :: jdf_lsecs(nls) ! Julian day of leap second switchover
  INTEGER              :: ils            ! Loop counter
  LOGICAL, SAVE        :: first_call=.TRUE.

!--------------------------------------------------------------------
! 1. Work out the 'GPS' Julian days of the leap second switches
!--------------------------------------------------------------------

  IF ( first_call ) THEN

    CDT(:) = 0

    DO ils=1,nls

      SDT = cdtls(ils)
      READ (SDT( 1: 4), FMT='(I4.4)') CDT(1)
      READ (SDT( 6: 7), FMT='(I2.2)') CDT(2)
      READ (SDT( 9:10), FMT='(I2.2)') CDT(3)

      CALL CalToJul( CDT, jdf, 1)

      jdf_lsecs(ils) = jdf + 1.D0  ! Leap seconds are applied at the _end_ of the quoted dates

    END DO

    first_call = .FALSE.

  END IF

!--------------------------------------------------------------------
! 2. Work out the accumulated leap seconds at the given date
!--------------------------------------------------------------------

  lsecs = SUM(incls)

  CDT = UTC  ! Turn UTC into a variable, so that it can be used an argument of CalToJul

  CALL CalToJul( CDT, jdf, 1)

  DO ils=nls,1,-1  ! Doing it in reverse is likely to be quicker.

    IF ( jdf < jdf_lsecs(ils) ) THEN
      lsecs = lsecs - incls(ils)
    ELSE
      EXIT
    END IF

  END DO

END FUNCTION LeapSeconds
