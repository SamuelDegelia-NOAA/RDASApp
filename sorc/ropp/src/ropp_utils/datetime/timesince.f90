! $Id$

!****s* DateTime/TimeSince *
!
! NAME
!    TimeSince   (timesince.f90)
!
! SYNOPSIS
!   Subroutine to convert between calendar date & time since some
!   arbitrary base date time, or vice-versa
!
!   USE  DateTime
!   CHARACTER (LEN=40) :: Tbase
!   INTEGER            :: CDT(8)
!   REAL(dp)           :: Tsince
!   INTEGER            :: inv
!   CALL TimeSince ( CDT, Tsince, inv[, Base=Tbase] )
!
! INPUTS
!   CDT     int   Array(8): Calendar date & clock time  [if inv>0]
!   Tsince  dflt  Value in 'units since base date/time' [if inv<=0]
!   inv     int   Indicator for direction of conversion
!                  > 0 : Calendar --> time since base
!                 <= 0 : time since base --> Calendar
!   Base    char  (Optional) Base units/date/time
!                 Default: 'JD2000'
!
! OUTPUTS
!   CDT     int   Array(8): Calendar date & clock time  [if inv<=0]
!   Tsince  dflt  Value in 'units since base date/time' [if inv>0]
!
! CALLS
!   CalToJul
!   ConvertDT
!
! DESCRIPTION
!   Converts between:
!     - CDT: Gregorian calendar date & clock time as an 8-element array
!       (year,month,day,zone,hour,minute,second,millisecond)
!       as returned by the F90 DATE_AND_TIME(VALUE=array) intrinsic
!   and:
!     - Tsince: time relative to an arbitary base date/time and in
!       base units (seconds, minutes, hours or days)
!   or vice-versa.
!   inv indicates the direction of conversion: >0 for CDT-->Tsince else
!   Tsince-->CDT.
!   NB Tsince argument's KIND must be declared equivalent to
!   DOUBLE PRECISION (REAL*8).
!
!   The (optional) Base string may specify an arbitrary base
!   units and date/time as one of:
!   1) '<units> SINCE <[d]d-MMM-yyyy [hh[:mm[:ss]]]>'
!      where <units> can be 'seconds', 'minutes', 'hours' or 'days'.
!      If the time portion is truncated, the missing elements are
!      assumed to be zero, so '1-Jan-2010' is the same as
!      '01-Jan-2010 00:00:00'
!   2) '<units> SINCE <yyyy-mm-dd [hh[:mm[:ss]]]>'
!      otherwise the same as (1)
!   3) '<units> SINCE <yyyymmdd[hh[mm[ss]]]'
!      otherwise the same as (1)
!   4) 'J<u>[<yyyy>]'
!      where <u> is one of 'S', 'M', 'H' or 'D' and the date is
!      implicitly 1st January of the year yyyy at midnight.
!      If yyyy is blank, 2000 is assumed.
!   5) one of the following short-hand keywords:
!      a) 'GPS[SECONDS]' eqiv 'seconds since 6-Jan-1980 00:00:00',
!          - the base date for counting GPS time in seconds
!      b) 'MJD' equiv 'days since 16-Nov-1858 00:00:00'
!         - Modified Julian Date (defined as JDF - 2,400,000.5)
!      c) 'UNIX' equiv 'seconds since 1-Jan-1970 00:00:00'
!         - the base for UNIX time counting
!      d) 'ZERO' equiv 'days since 1-Jan-4317 12:00:00'
!         - the base JDF=0 (standard Julian Days); same as CalToJul().
!
!   If the Base units argument is not present, or the string is given
!   but is blank or incorrectly formatted, a default of JD2000
!   (days since 1-Jan-2000 00:00;00) is used.
!   The 'zone' element is ignored for input and set to zero on output
!   (implying UTC).
!
! EXAMPLES
!   1) Current time in seconds since midnight, 1-Jan-2000
!      USE DateTime
!      INTEGER  :: CDT(8)
!      REAL(dp) :: JDF
!      CALL Date_and_Time_UTC ( Values=CDT )
!      --> CDT = 2010,4,9,15,17,14,234
!      CALL TimeSince ( CDT, JDF, Base="JS2000" )
!      --> JDF = 324141434.23402011
!
!   2) Day-of-year for today
!      USE DateTime
!      CHARACTER (LEN=20) :: Tbase
!      INTEGER  :: CDT(8), DoY
!      REAL(dp) :: JDF
!      CALL Date_and_Time_UTC ( Values=CDT )
!      --> CDT = 2010,4,9,15,17,14,234
!      WRITE ( Tbase, "(A2,I4.4)" ) "JD", CDT(1)
!      CALL TimeSince ( CDT, JDF, 1, Base=Tbase )
!      DoY = INT ( JDF1 + HalfDay ) ! allow for JDFs from midday
!      --> DoY = 99
!
!   3) Calendar date for 1 million hours from 11:32 on 8-Nov-1952
!      USE DateTime
!      INTEGER  :: CDT(8)=0
!      REAL(dp) :: OMH=1000000.0_dp
!      CALL TimeSince ( CDT, OMH, -1, Base="Hours since 1952-11-08 11:32" )
!      --> CDT = 2066,12,7,0,3,32,0,0 (03:32 7-Dec-2066)
!
! SEE ALSO
!    CalToJul(), DateTimeOffset()
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

SUBROUTINE TimeSince ( CDT,    & ! (inout)
                       Tsince, & ! (inout)
                       inv,    & ! (in)
                       Base )    ! (in/opt)

  USE DateTimeProgs, ONLY: CalToJul, ConvertDT
  USE DateTimeTypes

  IMPLICIT NONE

! Day-to-units conversion factors

  REAL(dp), PARAMETER :: D2D = 1.0_dp         ! Days
  REAL(dp), PARAMETER :: D2H = D2D * 24.0_dp  ! Days to Hours
  REAL(dp), PARAMETER :: D2M = D2H * 60.0_dp  ! Days to Minutes
  REAL(dp), PARAMETER :: D2S = D2M * 60.0_dp  ! Days to Seconds

! Character case conversion strings

  CHARACTER (LEN=26), PARAMETER :: UPPER="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  CHARACTER (LEN=26), PARAMETER :: lower="abcdefghijklmnopqrstuvwxyz"

! Argument list parameters

  CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: Base ! "<units> SINCE <date>" or
                                                  ! "J<u><yyyy>"
  INTEGER,  INTENT(INOUT) :: CDT(:)   ! Calendar date Y,M,D,Z,h,m,s,t
  REAL(dp), INTENT(INOUT) :: Tsince   ! Time relative to Base
  INTEGER,  INTENT(IN)    :: inv      ! >0: Arr->Val else Val->Arr

!  Local variables

  CHARACTER (LEN=40) :: Lbase      ! Local working copy of Base
  CHARACTER (LEN=14) :: DTshort    ! Date/time in short format
  REAL(dp)           :: BaseTime   ! Base time in Julian Days
  REAL(dp)           :: UserTime   ! User time in Julian Days
  REAL(dp)           :: factor     ! Days-to-units conversion factor
  INTEGER            :: BDT(8)     ! Base date/time array
  INTEGER            :: Yr, Mo, Dy ! Date elements
  INTEGER            :: Hr, Mn, Sc ! Time elements
  INTEGER            :: i, j       ! Indices to string positions
  INTEGER            :: status     ! I/O status

!--------------------------------------------------------------------
! 1. Pre-condition base time & units.
!    If Base is not given or is blank, assume default JD2000
!   (days since 2000-01-01 00:00:00)
!--------------------------------------------------------------------

  factor = D2D                    ! default units (days)
  BDT(:) = (/2000,1,1,0,0,0,0,0/) ! default base date/time

  IF ( PRESENT(Base) ) THEN
    IF ( Base /= " " ) THEN
      Lbase = ADJUSTL ( Base )
      DO i = 1, LEN_TRIM(Lbase)
        j = INDEX ( lower, Lbase(i:i) )
        IF ( j > 0 ) Lbase(i:i) = UPPER(j:j)
      END DO
    ELSE
      Lbase = "JD2000"
    END IF
  ELSE
    Lbase = "JD2000"
  END IF

!--------------------------------------------------------------------
! 2. Parse base date/time to extract units & epoch date/time
!--------------------------------------------------------------------

!--------------------------------------------------------------------
! 2.1 Keyword short-cuts
!--------------------------------------------------------------------

  IF ( Lbase(1:3) == "GPS" ) THEN        ! GPS epoch 00:00 6-Jan-1980 in secs
    factor = D2S
    BDT    = (/1980,1,6,0,0,0,0,0/)

  ELSE IF ( Lbase(1:3) == "MJD" ) THEN   ! Modified JD = JDF-2,400,000.5
    factor = D2D
    BDT    = (/1858,11,17,0,0,0,0,0/)

  ELSE IF ( Lbase(1:5) == "UNIX" ) THEN  ! UNIX epoch 00:00 1-Jan-1970 in secs
    factor = D2S
    BDT    = (/1970,1,1,0,0,0,0,0/)

  ELSE IF ( Lbase(1:4) == "ZERO" ) THEN  ! Standard epoch JDF=0 (allows for
    factor = D2D                         ! Julian/Gregorian calendar change)
    BDT    = (/-4713,11,24,0,12,0,0,0/)

!--------------------------------------------------------------------
! 2.2 Juyyyy format. Extract units & base year (assume midnight
!     1st Jan). Default to days if nothing else recognised, and
!     default year if value is mal-formatted or invalid.
!--------------------------------------------------------------------

  ELSE IF ( Lbase(1:1) == "J" ) THEN
    SELECT CASE (Lbase(2:2))
      CASE ("D")
        factor = D2D
      CASE ("H")
        factor = D2H
      CASE ("M")
        factor = D2M
      CASE ("S")
        factor = D2S
      CASE DEFAULT
        factor = D2D
    END SELECT

    READ ( Lbase(3:6), "(BZ,I4)", IOSTAT=status ) Yr
    IF ( status == 0 .AND. &
         Yr     >= 0 .AND. &
         Yr     <= 3099 ) BDT(IdxYear) = Yr

!--------------------------------------------------------------------
! 2.3 'units SINCE yyyy-mm-dd hh:mm:ss' format
!--------------------------------------------------------------------

  ELSE

!--------------------------------------------------------------------
! 2.3.1 Extract units (be generous in the extact string!)
!       Default to days if nothing else recognized
!--------------------------------------------------------------------

    i = INDEX (Lbase, " ")
    SELECT CASE (Lbase(1:i-1))
      CASE ("DY", "DAY", "DAYS")
        factor = D2D
      CASE ("HR", "HRS", "HOUR", "HOURS" )
        factor = D2H
      CASE ("MN", "MIN", "MINS", "MINUTE", "MINUTES")
        factor = D2M
      CASE ("SC", "SEC", "SECS", "SECOND", "SECONDS")
        factor = D2S
      CASE DEFAULT
        factor = D2D
    END SELECT

!--------------------------------------------------------------------
! 2.3.2 We ought to complain if SINCE is missing but for now just
!       assume SINCE if it's not present and the units is directly
!       followed by the base date/time; if it is present, skip over
!       it anyway
!--------------------------------------------------------------------

    Lbase = ADJUSTL ( Lbase(i:) )
    IF ( Lbase(1:5) == "SINCE" ) &
      Lbase = ADJUSTL ( Lbase(6:) )

!--------------------------------------------------------------------
! 2.3.3 Base date/time. If long1 or long2 format, convert to short.
!       Only over-ride default if *all* elements are valid.
!       TODO: check for correct no. of days in month incl Leap Years
!--------------------------------------------------------------------

    CALL ConvertDT ( Lbase, DTshort, 1 )
    READ ( DTshort, FMT="(BZ,I4,5I2)", IOSTAT=status ) &
                                Yr, Mo, Dy, Hr, Mn, Sc
    IF ( status == 0 .AND.               &
         Yr >=  0 .AND. Yr <= 3099 .AND. &
         Mo >=  1 .AND. Mo <=   12 .AND. &
         Dy >=  0 .AND. Dy <=   31 .AND. &
         Hr >=  0 .AND. Hr <=   23 .AND. &
         Mn >=  0 .AND. Mn <=   59 .AND. &
         Sc >=  0 .AND. Sc <=   59 ) THEN
      BDT = (/Yr,Mo,Dy,0,Hr,Mn,Sc,0/)
    END IF

  ENDIF

!--------------------------------------------------------------------
! 3. Base date/time as Julian date
!--------------------------------------------------------------------

  CALL CalToJul ( BDT, BaseTime, 1 )

!--------------------------------------------------------------------
! 4. Convert user time relative to base in appropriate units
!--------------------------------------------------------------------

  IF ( inv > 0 ) THEN
    CALL CalToJul ( CDT, UserTime, 1 )
    Tsince = ( UserTime - BaseTime ) * factor
  ELSE
    UserTime = Tsince / factor + BaseTime
    CALL CalToJul ( CDT, UserTime, -1 )
  END IF

END SUBROUTINE TimeSince
