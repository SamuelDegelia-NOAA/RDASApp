! $Id$

!****s* Datetime/AugmentUTC *
!
! NAME
!   AugmentUTC  (augmentutc.f90)
!
! SYNOPSIS
!   Subroutine to add specified time in days to given UTC datetime, expressed
!   as length-8 integer arrays (/ YYYY, MM, DD, Zone, hh, mm, ss, msec /).
!   CALL AugmentUTC (UTC, JDF)
!
! INPUTS
!   UTC  int(8)  UTC datetime
!   JDF  (dp)    time to add in days
!
! OUTPUTS
!   UTC  int(8)  UTC datetime (updated)
!
! DESCRIPTION
!   Adds JDF to UTC properly, taking account of leap seconds.
!
! SEE ALSO
!   CalToJul()
!   GPStoUTC()
!
! AUTHOR
!   The ROM SAF team
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

SUBROUTINE AugmentUTC ( UTC,  & ! (inout)
                        JDF )   ! (in)

  USE datetimetypes, ONLY: dp
  USE datetimeprogs, not_this => AugmentUTC

  IMPLICIT NONE

! Argument list parameters
  INTEGER, INTENT(INOUT)        :: UTC(8)  ! UTC datetime
  REAL(dp), INTENT(IN)          :: JDF     ! Number of days to add

! Local variables
  INTEGER                       :: GPS(8)
  REAL(dp)                      :: jdf2

!---------------------------------------------------------------
! 1. Add JDF to UTC, adding or subtracting the right number of leap seconds
!---------------------------------------------------------------

    CALL GPStoUTC(GPS, UTC, -1) ! UTC -> GPS
    CALL CalToJul(GPS, JDF2, 1) ! caldate -> juldate

    jdf2 = jdf2 + jdf ! add the time

    CALL CalToJul(GPS, JDF2, -1) ! juldate -> caldate
    CALL GPStoUTC(GPS, UTC, 1) ! GPS -> UTC

END SUBROUTINE AugmentUTC
