! $Id$

SUBROUTINE To_Upper ( string )

!****s* Common/To_Upper *
!
! NAME
!       To_Upper - Convert alpha characters in a string from lower to
!                  upper case
!
! SYNOPSIS
!       use ropp_utils
!        ...
!       call To_Upper( string )
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
    j = INDEX ( lower, string(i:i) )
    IF ( j > 0 ) string(i:i) = UPPER(j:j)
  END DO

END SUBROUTINE To_Upper
