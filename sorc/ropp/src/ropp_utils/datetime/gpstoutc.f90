! $Id$

!****s* Datetime/GPStoUTC *
!
! NAME
!   GPStoUTC  (gpstoutc.f90)
!
! SYNOPSIS
!   Subroutine to convert between GPS and UTC datetimes, expressed
!   as length-8 integer arrays (/ YYYY, MM, DD, Zone, hh, mm, ss, msec /).
!
!   CALL GPStoUTC (GPS, UTC, inv)
!
! INPUTS
!   GPS  int(8)  GPS datetime   [if inv>0]
!   UTC  int(8)  UTC datetime   [if inv<=0]
!   inv  int     Indicator for direction of conversion
!                 > 0 : GPS --> UTC
!                <= 0 : UTC --> GPS
!
! OUTPUTS
!   UTC  int(8)  UTC datetime   [if inv<=0]
!   GPS  int(8)  GPS datetime   [if inv>0]
!
! DESCRIPTION
!   Converts between:
!     - GPS datetime (in int(8) form)
!   and:
!     - UTC datetime (in int(8) form).
!   inv indicates the direction of conversion:
!     >0 for GPS-->UTC, otherwise UTC-->GPS.
!
! NOTES
!   The conversion is carried out by adding or subtracting leapseconds
!   to the corresponding Julian days.  The governing equation is
!
!   GPS_JDF = UTC_JDF + (lambda(UTC_JDF) - lambda_0)
!
!   where lambda is the (non-linear) leap second function, lambda_0 is
!   the number of leap seconds that had accumulated when the GPS clock
!   started at 0Z 06/01/1980 (which implies that lambda_0 equals 9 secs),
!   and the other variables are the GPS and UTC julian days,
!   calculated _without_ accounting for leap seconds, by means of CalToJul.
!   The non-linearity of lambda requires the inversion to be done iteratively.
!
! SEE ALSO
!   CalToJul()
!   LeapSeconds()
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

SUBROUTINE GPStoUTC ( GPS,   & ! (inout)
                      UTC,   & ! (inout)
                      inv )  ! (in)

  USE datetimetypes, ONLY: dp
  USE datetimeprogs, not_this => GPStoUTC

  IMPLICIT NONE

! Argument list parameters

  INTEGER, INTENT(INOUT)        :: GPS(8)                ! GPS datetime
  INTEGER, INTENT(INOUT)        :: UTC(8)                ! UTC datetime
  INTEGER, INTENT(IN)           :: inv                   ! inversion flag

! Local variables

  REAL(dp), PARAMETER           :: sec2day=1.0_dp/86400.0_dp
  REAL(dp)                      :: jdf, jdf2
  INTEGER, PARAMETER            :: leapsecs_zero=9       ! The number of leapsecs accrued up to
                                                         ! the GPS base time of 0Z 06/01/1980.
  INTEGER, PARAMETER            :: iter_max=10
  INTEGER                       :: iter, delta_old, delta_new

!---------------------------------------------------------------
! 1. Add or subtract the appropriate number of leap seconds
!---------------------------------------------------------------

  iter = 0
  delta_old = 0

  IF ( inv <= 0 ) THEN  ! UTC to GPS

    CALL caltojul(UTC, JDF,  1)

    JDF = JDF + (leapseconds(UTC) - leapsecs_zero) * sec2day

    CALL caltojul(GPS, JDF, -1)

  ELSE  ! GPS to UTC

    CALL caltojul(GPS, JDF,  1)

    DO   ! Need to solve by iterating on delta = UTC_JDF - GPS_JDF
      iter = iter + 1
      JDF2 = JDF + delta_old * sec2day
      CALL caltojul(UTC, JDF2,  -1)
      delta_new = leapsecs_zero - leapseconds(UTC)
      IF ( delta_old == delta_new .OR. iter > iter_max ) EXIT
      delta_old = delta_new
    END DO

    JDF2 = JDF + delta_new * sec2day

    CALL caltojul(UTC, JDF2, -1)

  END IF

END SUBROUTINE GPStoUTC
