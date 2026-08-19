! $Id$

!****s* DateTime/DateTimeOffset *
!
! NAME
!   DateTimeOffset     (datetimeoffset.f90)
!
! SYNOPSIS
!   Offset a date/time by a given amount
!
!   USE DateTime
!   INTEGER :: CDT1(8), CDT2(8)
!   CALL DateTimeOffset ( CDT1 "+|-", CDT2 )
!
! INPUTS
!   CDT1  int   Array(8): Calendar date & clock time in UTC
!   oper  char  Operation ("+" or "-")
!   CDT2  int   Array(8): Calendar date & clock time in UTC
!
! OUTPUTS
!   CDT1  int   Array(8): Calendar date & clock time in UTC
!
! CALLS
!   CalToJul
!
! DESCRIPTION
!   Appplies an offset to the given date & time array using
!   a second date/time array, which may be a small delta or
!   a full calendar value. The offset operation may be
!   to add (oper="+") or subtract (oper="-"). The result is
!   returned in the first date/time array.
!   This routine handles two types of case:
!   1) CDT1 is an absolute date/time and CDT2 is a relatively
!      small (<10 years) delta offset (oper is "+" or "-"); CDT1
!      will be the absolute date/time with the offset applied.
!   2) CDT1 and CDT2 are both absolute date/times and oper is
!      "-"; CDT1 will be the time difference in days, hours,
!      minutes, seconds & msecs (year, month & zone elements
!      will be zero). If CDT2 is later than CDT1, all of these
!      non-zero time elements will be negative.
!   Note that the case of CDT1 + CDT2 where both are absolute
!   date/times will work - but this is not a sensible operation!
!
! EXAMPLES
!   1) Difference between two date/times:
!      USE DateTime
!      INTEGER :: CDT1(8)
!      INTEGER :: CDT2(8) = (/2010,1,1,0,0,0,0,0/)
!      CALL DATE_AND_TIME_UTC(VALUES=CDT1)
!      --> CDT1 = 2010,4,9,15,17,14,234
!      CALL DateTimeOffset ( CDT1, "-", CDT2 )
!      --> CDT1 = 0,0,98,0,15,17,14,234
!
!   2) Date/time three days, 12 hours ago from now (which just
!      happens to be 06:30 2-Jan-2011):
!      USE DateTime
!      INTEGER :: CDT1(8) = (/2011,1,2,0,06,30,0,0/)
!      INTEGER :: CDT2(8) = (/0,0,3,0,12,0,0,0/)
!      CALL DateTimeOffset ( CDT1, "+", CDT2 )
!      --> CDT1 = 2010,12,29,0,18,30,0,0
!
!   3) Next whole hour after an observaton timestamped at
!      23:14 31-Dec-2010:
!      USE DateTime
!      INTEGER :: CDT1(8) = (/2010,12,31,0,23,14,0,0/)
!      INTEGER :: CDT2(8) = (/0,0,0,0,1,0,0,0/)
!      CALL DateTimeOffset ( CDT1, "+", CDT2 )
!      CDT1(6:8) = 0
!      --> CDT1 = 2011,1,1,0,0,0,0,0
!
!   4) Day-of-year for today
!      USE DateTime
!      INTEGER :: CDT1(8), CDT2(8)
!      CALL Date_and_Time_UTC ( VALUES=CDT1 )
!      --> CDT1 = 2010,4,9,15,17,14,234
!      CDT2 = (/CDT1(1),1,0,0,0,0,0,0/) ! NB day=0 so 1-Jan returns 1
!      CALL DateTimeOffset ( CDT1,'-',CDT2)
!      --> CDT1(IdxDay) = 99
!
! SEE ALSO
!    TimeSince()
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

SUBROUTINE DateTimeOffset ( CDT1,   & ! (inout)
                            oper,   & ! (in)
                            CDT2 )    ! (inout)

  USE DateTimeProgs, ONLY: CalToJul
  USE DateTimeTypes
  
  IMPLICIT NONE

! Fixed parameters

  REAL(dp),          PARAMETER :: JDZ = 1721027.0_dp   ! JDF for 00:00 1-Jan-0000
  REAL(dp),          PARAMETER :: one = 1.0_dp         ! Unity
  REAL(dp),          PARAMETER :: tenyrs = 36525.0_dp  ! 10yrs in days

! Argument list parameters

  CHARACTER, INTENT(IN)    :: oper       ! '+' or '-'
  INTEGER,   INTENT(INOUT) :: CDT1(:)    ! Date/time to apply offset
  INTEGER,   INTENT(INOUT) :: CDT2(:)    ! date/time to offset by

! Local variables

  REAL(dp) :: JDF1, JDF2, time

!--------------------------------------------------------------------
! 1. Convert both input calendar date/times to offsets in days
!    since 'time zero' (midnight 1-Jan-0000). This ensures that
!    CDT2 as a small delta offset remains small in Julian form,
!--------------------------------------------------------------------

  CALL CalToJul ( CDT1, JDF1, 1 )
  JDF1 = JDF1 - JDZ
  CALL CalToJul ( CDT2, JDF2, 1 )
  JDF2 = JDF2 - JDZ

!--------------------------------------------------------------------
! 2. For small offsets (<10yrs) adjust Jul midday-->Cal midnight.
!    Perform the required add or subtract operation
!--------------------------------------------------------------------

  IF ( JDF2 <= tenyrs ) JDF2 = JDF2 - HalfDay
  SELECT CASE (oper)
    CASE ("-")
      JDF1 = JDF1 - JDF2
    CASE ("+")
      JDF1 = JDF1 + JDF2
    CASE DEFAULT
  END SELECT

!--------------------------------------------------------------------
! 3. For large results (>10yrs, assumed to be normal calendar
!    date/times) convert back to calendar form...
!--------------------------------------------------------------------

  IF ( ABS(JDF1) >= tenyrs ) THEN
    JDF1 = JDF1 + JDZ
    CALL CalToJul ( CDT1, JDF1, -1 )

!--------------------------------------------------------------------
! 4. ...else assume a small time in days,hrs,mins,secs & msecs
!--------------------------------------------------------------------

  ELSE
    CDT1(:)         = 0
    CDT1(IdxDay)    = INT ( JDF1 )
    time            = 24_dp   * MOD ( JDF1, one)
    CDT1(IdxHour)   = INT ( time )
    time            = 60_dp   * MOD ( time, one )
    CDT1(IdxMinute) = INT ( time )
    time            = 60_dp   * MOD ( time, one )
    CDT1(IdxSecond) = INT ( time )
    time            = 1000_dp * MOD ( time, one )
    CDT1(IdxMSec)   = NINT ( time )
  END IF

END SUBROUTINE DateTimeOffset
