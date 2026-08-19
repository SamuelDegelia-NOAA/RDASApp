! $Id$

!****s* DateTime/MonthOfYear *
!
! NAME
!   MonthOfYear     (monthofyear.f90)
!
! SYNOPSIS
!   Convert a Month between string and numeric form
!
!   USE DateTime
!   CHARACTER (LEN=10) :: monthname
!   INTEGER            :: monthnumber, inv
!   CALL MonthOfYear ( monthnumber, monthname, inv )
!
! INPUTS
!   MonthNumber  int  Month as number (1-12) {if inv>=0}
!   MonthString  chr  Month as name ("JAN"-"DEC", "January"-"December")
!                     [if inv<0]
!   inv          int  Inversion flag
!                      > 0 : number --> string (full name)
!                      = 0 : number --> string (3chr uppercase)
!                      < 0 : string --> number
!
! OUTPUTS
!   MonthNumber  int  Month as number (1-12 or 0 if string invalid)
!                     [if inv<0]
!   MonthString  chr  Month as name ("JAN"-"DEC", "January"-"December")
!                     [if inv>=0]
!
! DECRIPTION
!   Converts a month name (e.g. "February") to its numeric
!   equivalent (e.g 2) or vice-versa depending on the value
!   of inv.
!   inv = 0 : an input numeric value must be in the range 1-12
!             and is converted to its equivalent month name (returned
!             as a 3-chr uppercase string, e.g. "FEB"). Invalid values
!             return the month name for absolute modulo-12 value (e.g.
!             13 returns "JAN" and 0 returns "DEC").
!   inv > 0 : as above, but returns the full, capitalised, month name
!             (e.g. "February").
!   inv < 0 : For input, month names may be full or abbreviated to a
!             minimum of three characters, and may be in upper,
!             lower or mixed case (the match is done using only
!             the first three significant characters in upper case).
!             Invalid months return a numeric value of zero, else
!             the month in the range 1 to 12.
!
! EXAMPLES
!   USE DateTime
!   CHARACTER (LEN=10) :: monthname
!   INTEGER            :: monthnumber
!   monthnumber = 12
!   CALL MonthOfYear ( monthnumber, monthname, 1 )
!   --> monthname = "December"
!   CALL MonthOfYear ( monthnumber, monthname, 0 )
!   --> monthname = "DEC"
!   monthname = "March"
!   CALL MonthOfYear ( monthnumber, monthname, -1 )
!   --> monthnumber = 3
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

SUBROUTINE MonthOfYear ( MonthNumber, & ! (inout)
                         MonthString, & ! (inout)
                         inv )          ! (in)

  USE DateTimeTypes

  IMPLICIT NONE

! Fixed parameters

  CHARACTER (Len=*), PARAMETER :: UPPER="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  CHARACTER (Len=*), PARAMETER :: lower="abcdefghijklmnopqrstuvwxyz"
  CHARACTER (LEN=*), PARAMETER :: MonthName(nMonPerYear) = & ! month names
                        (/ "January  ", "February ", "March    ", &
                           "April    ", "May      ", "June     ", &
                           "July     ", "August   ", "September", &
                           "October  ", "November ", "December " /)
  CHARACTER (LEN=*), PARAMETER :: ShortMonthNames=&
                           "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"

! Argcument list parameters

  CHARACTER (LEN=*), INTENT(INOUT) :: MonthString
  INTEGER,           INTENT(INOUT) :: MonthNumber
  INTEGER,           INTENT(IN)    :: inv

! Local variables

  CHARACTER (LEN=3) :: MonStr
  INTEGER           :: MonNum
  INTEGER           :: i, j

!--------------------------------------------------------------------
! 1. Number to name. Name index is just Number modulo 12 (1-12)
!    from the month list
!--------------------------------------------------------------------

  IF ( inv == 0 ) THEN

!--------------------------------------------------------------------
! 1.1 Abbreviated 3-chr format
!--------------------------------------------------------------------

    MonNum      = MOD ( ABS(MonthNumber)-1, nMonPerYear ) + 1
    i           = MonNum * 3 - 2
    MonthString = ShortMonthNames(i:i+2)

  ELSE IF ( inv > 0 ) THEN

!--------------------------------------------------------------------
! 1.1 Full name format
!--------------------------------------------------------------------

    MonNum      = MOD ( ABS(MonthNumber)-1, nMonPerYear ) + 1
    MonthString = MonthName(MonNum)

  ELSE

!--------------------------------------------------------------------
! 2. Name to number. Left-justify first 3 chrs, ensure all
!    uppercase, then scan for match in the month list
!--------------------------------------------------------------------

    MonStr = ADJUSTL ( MonthString )
    DO i = 1, 3
      j = INDEX ( lower, MonStr(i:i) )
      IF ( j > 0 ) MonStr(i:i) = UPPER(j:j)
    END DO
    i = INDEX ( ShortMonthNames, MonStr )
    IF ( MOD ( i-1, 3 ) == 0 ) THEN
      MonthNumber = ( i + 2 ) / 3
    ELSE
      MonthNumber = 0
    END IF

  END IF

END SUBROUTINE MonthOfYear
