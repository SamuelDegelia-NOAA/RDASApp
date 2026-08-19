! $Id$

subroutine message_get_addinfo(addinfo)

!****s* Messages/message_get_addinfo *
!
! NAME
!    message_get_addinfo - Get an additional information string used by the
!                          ropp_utils message anderror handling system.
!
! SYNOPSIS
!    call message_get_addinfo(progname)
! 
! DESCRIPTION
!    This routine gets the additional info string that appears in error messages
!    issued by the message and error handling system of the ropp_utils library.
!
! INPUTS
!    character(len = *) :: addinfo
!
! SEE ALSO
!    assert
!    message
!    message_set_program
!    message_get_program
!    message_set_routine
!    message_get_routine
!    message_set_addinfo
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
! COPYRIGHT
!
!    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
!
!    All rights reserved.
!
!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:
!
!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software as well
!    as in supporting documentation.
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
!    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
!    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!****

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  use messages

  implicit none

  character(len = *), intent(out) :: addinfo

  addinfo = msg_addinfo

end subroutine message_get_addinfo
