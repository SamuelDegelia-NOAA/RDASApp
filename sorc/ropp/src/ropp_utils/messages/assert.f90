! $Id$

subroutine assert(condition, string, severity)

!****s* Messages/assert *
!
! NAME
!    assert - Make sure that a condition is fulfilled.
!
! SYNOPSIS
!    call assert(condition, string [, severity])
! 
! DESCRIPTION
!    This subroutine provides an assertion facility, i.e. it issues a
!    (fatal by default) error message if the condition if the condition
!    is NOT fullfilled.
!
! INPUTS
!    logical condition
!    char*   string
!    int     severity
!
! OUTPUT
!    If the test fails (i.e., the condition evaluates to .false.), the
!    text string is printed as part of a fatal error message. This also
!    terminates the program.
!
! NOTES
!    If an informational or warning message is desired instead of a fatal
!    error, the severity of the failure can be specified with the optional
!    argument. It is intended to receive one of msg_info, msg_warn, or
!    msg_error (all defined in the messages module).
!
! SEE ALSO
!   message
!   message_set_program
!   message_get_program
!   message_set_routine
!   message_get_routine
!   message_set_addinfo
!   message_get_addinfo
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

!--------------------------------------------------------------------------
! 1. Declarations
!--------------------------------------------------------------------------

  use messages, not_this => assert

  implicit none

  logical,            intent(in) :: condition
  character(len = *), intent(in) :: string
  integer,            optional   :: severity

  integer                        :: msg_level

!--------------------------------------------------------------------------
! 2. Default conditions
!--------------------------------------------------------------------------

  if (present(severity)) then
     msg_level = severity
  else
     msg_level = msg_fatal
  endif

!--------------------------------------------------------------------------
! 3. Handle the (error) condition
!--------------------------------------------------------------------------

  if (.not. condition) then
     call message(msg_level, &
          'Assertion failed:\n\t' // trim(string))
  endif
     
end subroutine assert
