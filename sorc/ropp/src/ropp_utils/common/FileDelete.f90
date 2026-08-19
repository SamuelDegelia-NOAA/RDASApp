! $Id$

SUBROUTINE file_delete(file, ierr)

!****s* Common/file_delete *
!
! NAME
!       file_delete - delete a file if it exists
!
! SYNOPSIS
!       use ropp_utils
!         ...
!       call file_delete(file, ierr)
!
! INPUT
!    file - filename to be deleted
!
! OUTPUT
!    ierr - error message with status
!
! NOTES
!    Output status flag depending on outcome
!    -2 : task failed 
!    -1 : file does not exist - task failed
!     0 : task successful
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

! 1. Declarations

  character(len = *), intent(in)  :: file
  integer,            intent(out) :: ierr
  
  integer                         :: unit, get_io_unit
  integer                         :: flag
  logical                         :: exists

! 2. Check if file exists

  ierr = -1
  
  if ( file /= " " ) then
    inquire(file = file, exist = exists, iostat = flag)

    if(exists)then
     
      unit = get_io_unit()
      open(unit = unit, file = file, iostat = flag, status='old')
      close(unit = unit, status = 'DELETE', iostat = flag)
     
      if(flag /= 0) then
        ierr = -2
      else
        ierr = 0
      endif
    endif
  endif

  return
  
END SUBROUTINE file_delete
     
