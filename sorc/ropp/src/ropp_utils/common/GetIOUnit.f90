! $Id$

function get_io_unit() result(unit)

!****s* Common/get_io_unit *
!
! NAME
!       get_io_unit - Obtain a free Fortran unit number
!
! SYNOPSIS
!       use ropp_utils
!        ...
!       unit = get_io_unit()
!
! DESCRIPTION
!       This function returns the first available unit number larger than 10.
!       Routine aborts the program entirely if there is no free unit less
!       than 1000, with a Fatal Error exit status of 3.
!       BASED ON MARQUARDT COLLECTION FILES LIBRARY ROUTINE
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

  implicit none

  integer :: unit
  logical :: unit_is_open

  unit = 10
  unit_is_open = .true.

  do while(unit_is_open .and. unit < 1000)
     unit = unit+1
     inquire(unit = unit, opened = unit_is_open)
  enddo

  if(unit > 999)then
     write(0, '(a)') 'FATAL ERROR: No free unit found ' // &
                     'between 10 and 999 - aborting.'
     call exit(3)
  endif

end function get_io_unit
