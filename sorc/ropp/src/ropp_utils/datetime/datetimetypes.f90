! $Id$

!****m* DateTime/DateTimeTypes *
!
! NAME
!    DateTimeTypes    (datetimetypes.f90) 
!
! SYNOPSIS
!   Module of definitions for date & time manipulation routines
!
!    USE DateTimeTypes
!
! DESCRIPTION
!    This module provides definitions for date & time manipulation 
!    routines
!
! SEE ALSO
!   DateTimeProgs
!   CalToJul
!   ConvertDT
!   Date_and_Time_UTC
!   DateTimeOffset
!   MonthOfYear
!   TimeSince
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

MODULE DateTimeTypes

! Constants

! Define sufficient floating point precison equivalent to
! DOUBLE PRECISON (REAL*8)

  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(P = 13, R = 307)

! Date & Time conversion factors

  REAL(dp), PARAMETER :: HalfDay     = 0.5_dp  ! Half a Day

  INTEGER,  PARAMETER :: nMSecPerSec = 1000    ! no. of msecs   in 1 second
  INTEGER,  PARAMETER :: nSecPerMin  = 60      ! no. of seconds in 1 minute
  INTEGER,  PARAMETER :: nMinPerHour = 60      ! no. of minutes in 1 hour
  INTEGER,  PARAMETER :: nHourPerDay = 24      ! no. of hours   in 1 day
  INTEGER,  PARAMETER :: nMSecPerDay = &       ! no. of msecs   in 1 day
                         nMSecPerSec * nSecPerMin * nMinPerHour * nHourPerDay
  INTEGER,  PARAMETER :: nDayPerMon(0:12) = &  ! no. of days    in each month (leap year @0)
                   (/ 29, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  INTEGER,  PARAMETER :: nMonPerYear = 12      ! no. of months in 1 year
  INTEGER,  PARAMETER :: nDayPerWeek = 7       ! no. of days   in 1 week

! Indices to months

  INTEGER, PARAMETER :: IdxFebLeap = 0 ! February in leap year
  INTEGER, PARAMETER :: IdxJan = 1     ! January
  INTEGER, PARAMETER :: IdxFeb = 2     ! February
  INTEGER, PARAMETER :: IdxMar = 3     ! March
  INTEGER, PARAMETER :: IdxApr = 4     ! April
  INTEGER, PARAMETER :: IdxMay = 5     ! May
  INTEGER, PARAMETER :: IdxJun = 6     ! June
  INTEGER, PARAMETER :: IdxJul = 7     ! July
  INTEGER, PARAMETER :: IdxAug = 8     ! August
  INTEGER, PARAMETER :: IdxSep = 9     ! September
  INTEGER, PARAMETER :: IdxOct = 10    ! October
  INTEGER, PARAMETER :: IdxNov = 11    ! November
  INTEGER, PARAMETER :: IdxDec = 12    ! December

! Indices to date/time elements (as returned by intrinsic DATE_AND_TIME)

  INTEGER, PARAMETER :: IdxYear   = 1  ! index to year     element
  INTEGER, PARAMETER :: IdxMonth  = 2  ! index to month    element
  INTEGER, PARAMETER :: IdxDay    = 3  ! index to day      element
  INTEGER, PARAMETER :: IdxZone   = 4  ! index to zone     element
  INTEGER, PARAMETER :: IdxHour   = 5  ! index to hour     element
  INTEGER, PARAMETER :: IdxMinute = 6  ! index to minute   element
  INTEGER, PARAMETER :: IdxSecond = 7  ! index to second   element
  INTEGER, PARAMETER :: IdxMSec   = 8  ! index to millisec element

! Format specifications

  CHARACTER (LEN=*), PARAMETER :: TimeFmt = &  ! Time format (hh:mm:ss.ttt [UT])
                     "(2(I2.2,':'),I2.2,'.',I3.3,1X,A2)"
  CHARACTER (LEN=*), PARAMETER :: DateFmt = &  ! Date format (dd-mmm-yyyy)
                     "(I2.2,'-',A3,'-',I4.4)"
  CHARACTER (LEN=*), PARAMETER :: DTfmt = &    ! Date/Time format (dd-mmm-yyyy hh:mm:ss.ttt [UT])
                     "(I2.2,'-',A3,'-',I4.4,1X,2(I2.2,':'),I2.2,'.',I3.3,1X,A)"
  CHARACTER (LEN=*), PARAMETER :: DTsfmt = &   ! Date/Time format (yyyymmddhhmmss)
                     "(I4.4,5I2.2)"

END MODULE DateTimeTypes
