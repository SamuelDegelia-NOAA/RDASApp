! $Id$

!****s* DateTime/ConvertDT *
!
! NAME
!   ConvertDT     (convertdt.f90)
!
! SYNOPSIS
!   Convert a Date & Time string between long1 or long2 & short formats
!
!   USE DateTime
!   CHARACTER (LEN=24) :: DTLong1
!   CHARACTER (LEN=19) :: DTLong2
!   CHARACTER (LEN=14) :: DTshort
!   INTEGER            :: inv msecs, lfmt
!   CALL ConvertDT ( DTlong, DTshort, inv[, LongFmt=lfmt][, Msec=msecs] )
!
! INPUTS
!   DTlong   chr  Date & Time as dd-MMM-yyyy hh:mm:ss.ttt
!                 or yyyy-MM-dd hh:mm:ss.ttt    [if inv>0]
!   DTshort  chr  Date & Time as yyyyMMddhhmmss [if inv<=0]
!   inv      int  Inversion flag
!                  >0 : long --> short
!                 <=0 : short --> long
!   Msec     int  (Optional) millisecs value (ttt: 0-999)
!   LongFmt  int  (optional) Output long format style. Ignored if inv>0
!                  1 = dd-MMM-yyyy hh:mm:ss.ttt (default)
!                  2 = yyyy-MM-dd hh:mm:ss.ttt
!
! OUTPUTS
!   DTlong   chr  Date & Time as dd-MMM-yyyy hh:mm:ss.ttt [if inv<=0]
!   DTshort  chr  Date & Time as yyyyMMddhhmmss           [if inv>0]
!   Msec     int  (Optional) millisecs value (ttt: 0-999)
!
! CALLS
!   MonthOfYear
!
! DESCRIPTION
!   When inv > 0, converts a date & time string from long1
!   (dd-MMM-yyyy hh:mm:ss.ttt) or long2 ( yyyy-MM-dd hh:mm:ss.ttt)
!   formats to short (yyyymmddhhmmss) format. For both long formats,
!   the time portion may be truncated, in which case the short format
!   will be padded with '0's. The ttt portion is anyway ignored.
!   When inv <= 0, converts a date & time string from short
!   (yyyymmddhhmmss) format to long1 (dd-MMM-yyyy hh:mm:ss.ttt) format.
!   The ttt portion will be '0's.
!   While the long formats are suitable for human reading, the
!   short format is more efficient to use for comparisons and sorting.
!
! EXAMPLES
!   USE DateTime
!   CHARACTER (LEN=24) :: DTlong1
!   CHARACTER (LEN=19) :: DTlong2
!   CHARACTER (LEN=14) :: DTshort
!   INTEGER            :: msecs
!   DTlong2 = "2010-04-14"
!   CALL ConvertDT ( DTlong2, DTshort, 1 )
!   --> DTshort = "20100414000000"
!   DTshort = "20100414093456"
!   msecs = 789
!   CALL ConvertDT ( DTlong1, DTshort, -1, Msec=msecs )
!   --> DTlong = "14-Apr-2010 09:34:56.789"
!   DTshort = "20100414093456"
!   CALL ConvertDT ( DTlong1, DTshort, -1, LongFmt=2 )
!   --> DTlong = "2010-04-14 09:34:56.000"
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

SUBROUTINE ConvertDT ( DTlong,  & ! (inout)
                       DTshort, & ! (inout)
                       inv,     & ! (in)
                       LongFmt, & ! (in/opt)
                       Msec )     ! (inout/opt)

  USE DateTimeProgs, ONLY: MonthOfYear
  
  IMPLICIT NONE

! Argument list parameters

  CHARACTER (LEN=*), INTENT(INOUT) :: DTlong   ! >=24 chrs
  CHARACTER (LEN=*), INTENT(INOUT) :: DTshort  ! >=14 chs
  INTEGER,           INTENT(IN)    :: inv      ! >0 long-->short
                                               !<=0 short-->long
  INTEGER, OPTIONAL, INTENT(IN)    :: LongFmt  ! Long format style
  INTEGER, OPTIONAL, INTENT(INOUT) :: Msec     ! Msecs value

! Local variables

  CHARACTER (LEN=3) :: MonthString, msecs
  INTEGER           :: MonthNumber, i, Lfmt

! Long-->Short

  IF ( inv > 0 ) THEN
    IF ( INDEX ( DTlong, "-" ) == 2 ) DTlong = "0" // DTlong
    i = INDEX ( DTlong, "-" )
    IF ( i == 3 ) THEN
      IF ( DTlong(1:1) == " " ) DTlong(1:1) = "0"
      MonthString = DTlong(4:6)
      CALL MonthOfYear ( MonthNumber, MonthString, -1 )
      WRITE ( DTshort, "(A,I2.2,A)" )                  &
                DTlong(8:11), MonthNumber, DTlong(1:2)// &
                DTlong(13:14)//DTlong(16:17)//DTlong(19:20)
      msecs = DTlong(22:24)
    ELSE IF ( i == 5 ) THEN
      DTshort = DTlong(01:04)//DTlong(06:07)//DTlong(9:10)// &
                DTlong(12:13)//DTlong(15:16)//DTlong(18:19)
      msecs = DTlong(21:23)
    ELSE
      DTshort = DTlong(1:14)
    END IF
    DO i = 8, 14
      IF ( DTshort(i:i) == " " ) DTshort(i:i) = "0"
    END DO
    IF ( PRESENT(Msec) ) READ ( msecs, FMT=* ) Msec

! Short-->Long

  ELSE
    IF ( PRESENT(Msec) ) THEN
      WRITE ( msecs, FMT=* ) Msec
    ELSE
      msecs = "000"
    END IF
    IF ( PRESENT(LongFmt) ) THEN
      Lfmt = MAX(MIN(LongFmt,1),2)
    ELSE
      Lfmt = 1
    END IF
    IF ( Lfmt == 1 ) THEN
      READ ( DTshort(5:6), "(I2)" ) MonthNumber
      CALL MonthOfYear ( MonthNumber, MonthString, 1 )
      DTlong = DTshort(7:8)//"-"//MonthString//"-"//DTshort(1:4)
    ELSE
      DTlong = DTshort(1:4)//"-"//DTshort(5:6)//"-"//DTshort(7:8)
    END IF
    DTlong = TRIM(DTlong)//" "//  &
             DTshort(9:10)//":"//DTshort(11:12)//":"//DTshort(13:14)// &
             "."//msecs

  END IF

END SUBROUTINE ConvertDT
