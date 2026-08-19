! $Id$

SUBROUTINE To_Lower ( string )

!****s* Common/To_Lower *
!
! NAME
!       To_Lower - Convert alpha characters in a string from upper to
!                  lower case
!
! SYNOPSIS
!       use ropp_utils
!        ...
!       call To_Lower( string )
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

  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(INOUT) :: string

  CHARACTER (LEN=26), PARAMETER :: UPPER="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  CHARACTER (LEN=26), PARAMETER :: lower="abcdefghijklmnopqrstuvwxyz"
  INTEGER :: i, j

  DO i = 1, LEN_TRIM(string)
    j = INDEX ( UPPER, string(i:i) )
    IF ( j > 0 ) string(i:i) = lower(j:j)
  END DO

END SUBROUTINE To_Lower
