! $Id$

!****s* DateTime/Date_and_Time_UTC *
!
! NAME
!   Date_and_Time_UTC     (date_and_time_utc.f90)
!
! SYNOPSIS
!   Mimics the F90 intrinsic DATE_AND_TIME, but returned
!   elements are in UTC
!
!   USE DateTime
!   CHARACTER (LEN=8)  :: Date
!   CHARACTER (LEN=10) :: Time
!   CHARACTER (LEN=5)  :: Zone
!   INTEGER            :: Values(8)
!   CALL Date_and_Time_UTC ( [DATE=Date][, TIME=Time]
!                          [, ZONE=zone][, VALUES=Values] )
!
! INPUTS
!   None
!
! OUTPUTS
!   Date    chr   (Optional) Date (CCYYMMDD)   in UTC
!   Time    chr   (Optional) Time (hhmmss.sss) in UTC
!   Zone    chr   (Optional) Zone (+/-hhmm)    always "+00:00"
!   Values  int   (Optional) Array(8): Calendar date & clock time in UTC
!
! CALLS
!   CalToJul
!
! DESCRIPTION
!   Get current system date/time as YR,MN,DY,ZO,HR,MN,SC,MS.
!   and using the zone element (minutes relative to UTC),
!   offset local time to UTC. NB: 'local' may already in UTC!
!   All arguments are optional, and can be any or all of Date,
!   Time, Zone (not useful!) as strings and Values (integer array
!   of date/time elements), as per the DATE_AND_TIME() intrinsic.
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

SUBROUTINE Date_and_Time_UTC ( Date, &  ! (out/opt)
                               Time, &  ! (out/opt)
                               Zone, &  ! (out/opt)
                               Values ) ! (out/opt)

  USE DateTimeProgs, ONLY: CalToJul
  USE DateTimeTypes

  IMPLICIT NONE

! Fixed parameters

  CHARACTER (LEN=*), PARAMETER :: Dfmt = "(I4,2I2.2)"
  CHARACTER (LEN=*), PARAMETER :: Tfmt = "(3I2.2,'.',I3.3)"
! CHARACTER (LEN=*), PARAMETER :: Zfmt = "(A1,2I2.2)"

! Argument list parameters

  CHARACTER (LEN=*), OPTIONAL, INTENT(OUT) :: Date
  CHARACTER (LEN=*), OPTIONAL, INTENT(OUT) :: Time
  CHARACTER (LEN=*), OPTIONAL, INTENT(OUT) :: Zone
  INTEGER,           OPTIONAL, INTENT(OUT) :: Values(:)

! Local variables

  REAL(dp)  :: JDF
  INTEGER   :: CDT(8)

!--------------------------------------------------------------------
! 1. Get 'local time' (this may already be UTC)
!--------------------------------------------------------------------

  CALL DATE_AND_TIME ( VALUES=CDT )

!--------------------------------------------------------------------
! 2. If time zone is not UTC, convert it to UTC
!--------------------------------------------------------------------

  IF ( CDT(IdxZone) /= 0 ) THEN
    CALL CalToJul (  CDT, JDF, 1 )
    CDT(IdxZone) = 0
    CALL CalToJul ( CDT, JDF, -1 )
  END IF

!--------------------------------------------------------------------
! 3. Convert to & return required optional elements
!--------------------------------------------------------------------

  IF ( PRESENT(Date) ) THEN
    WRITE ( Date, FMT=Dfmt ) CDT(1:3)
  END IF

  IF ( PRESENT(Time) ) THEN
    WRITE ( Time, FMT=Tfmt ) CDT(5:8)
  END IF

  IF ( PRESENT(Zone) ) THEN
    Zone = "+0000"
  END IF

  IF ( PRESENT(Values) ) THEN
    Values = CDT
  END IF

END SUBROUTINE Date_and_Time_UTC
