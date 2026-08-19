! $Id$

module messages

!****m* Messages/messages *
!
! NAME
!    messages - The tools90 message- and error handling system.
!
! SYNOPSIS
!    use messages
!
! DESCRIPTION
!    This module provides constants and interfaces to the Tools90
!    message and error handling system.
!
! SEE ALSO
!   assert
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

  implicit none

!--------------------------------------------------------------------------
! 2. Preconnected logical unit numbers
!--------------------------------------------------------------------------

  integer, parameter :: stdin  = 5
  integer, parameter :: stdout = 6
  integer, parameter :: stderr = 0

!--------------------------------------------------------------------------
! 2. Error types
!--------------------------------------------------------------------------

  integer, parameter, public :: msg_cont  =  0
  integer, parameter, public :: msg_info  =  1
  integer, parameter, public :: msg_diag  =  3
  integer, parameter, public :: msg_warn  =  2
  integer, parameter, public :: msg_error =  4
  integer, parameter, public :: msg_fatal =  8
  integer, parameter, public :: msg_noin  = 16

!--------------------------------------------------------------------------
! 2. Exit codes
!--------------------------------------------------------------------------

  integer, parameter, public :: msg_exit_ok     =  0
  integer, parameter, public :: msg_exit_warn   =  1
  integer, parameter, public :: msg_exit_error  =  2
  integer, parameter, public :: msg_exit_fatal  =  3

  INTEGER, SAVE, PUBLIC :: msg_EXIT_STATUS = msg_exit_ok

!--------------------------------------------------------------------------
! 3. Global private variables
!--------------------------------------------------------------------------

  character(len = 1024)       :: msg_program = ''
  character(len = 1024)       :: msg_routine = ''
  character(len = 1024)       :: msg_addinfo = ''
  character(len = 1024), save :: msg_logFile = ''

!---------------------------------------------------------------------------
! 4. Operational/Debug mode definitions
!---------------------------------------------------------------------------

!****ip* Initialisation/msg_MODE
!
! NAME
!    msg_MODE - Internal global operating mode definitions
!
! NOTES
!    This parameter controls the level of output diagnostic information
!    output by ROPP routines. This parameter may be used to selectively define
!    required output messages from within sub-routines, or define the
!    appropriate level of info/warning/error message output to print from
!    the messages library.
!
!    The available options are:
!       QuietMode  - only output error messages to stderr, no info/warnings
!       NormalMode - output all info and warnings to stdout, errors to stderr
!       VerboseMode - as NormalMode, but also output diagnostic/debug messages
!                     as specified within an individual subroutine.
!
!    The required msg_MODE may be altered either within a program routine, e.g.
!               msg_MODE = VerboseMode                ! Enable all messages
!               CALL message(msg_diag, "The result is....")
!               msg_MODE = NormalMode                 ! Re-set to normal level
!    or by implicitly setting the default value below and re-compiling, or by
!    setting the environment variable ROPP_MSG_MODE on the command line. The
!    environment variable is checked the first time the messages library is
!    called from within a program (and flag msg_MODE_READ is set to TRUE to
!    prevent further reading of the environment variable on subsequent calls).
!    Any further change to msg_MODE from within a program takes precendent over
!    the environment variable setting, which itself preceeds the default
!    setting in this module at compile-time.
!
! SOURCE
!
  INTEGER, PARAMETER, PUBLIC :: QuietMode   = 0
  INTEGER, PARAMETER, PUBLIC :: NormalMode  = 10
  INTEGER, PARAMETER, PUBLIC :: VerboseMode = 20

  INTEGER, SAVE, PUBLIC :: msg_MODE = NormalMode
  LOGICAL, SAVE, PUBLIC :: msg_MODE_READ = .FALSE.
!
!****

!--------------------------------------------------------------------------
! 5. Interfaces
!--------------------------------------------------------------------------

  interface message
    subroutine message(msgtype, msgtext)
      integer,           intent(in) :: msgtype
      character (len=*), intent(in) :: msgtext
    end subroutine message
  end interface

  interface assert
    subroutine assert(condition, string, severity)
      logical,           intent(in) :: condition
      character (len=*), intent(in) :: string
      integer,           optional   :: severity
    end subroutine assert
   end interface


contains

   subroutine message_set_logFile(logFileName)

    implicit none

    character (len=*), intent(in) :: logFileName

    msg_logFile = logFileName

   end subroutine message_set_logFile


   subroutine message_get_roppVersion(roppMajorNumber, &
                                      roppMinorNumber, &
                                      roppPatchNumber, &
                                      roppErrorCode)

! Note 1: Would be better to automatically generate the following from (eg)
!         the config.am files, which have to include the current version numbers.
! Note 2: Ideally, should allow the flexibility of a different version per module
!         (since this is apparently possible, if not recommended).
!         See ropp_*/common/ropp_*_version.f90 which addresses this issue
!         in the mean time.
! Note 3: Presently ErrorCode (from this routine) must be zero,
!         but could be useful if the routine is extended.
! Code written for ROPP5.0 by Johannes Fritzer, Univ Graz.
! Comments by Ian Culverwell, 8/4/2011.
! Additional comments by Dave Offiler 7/9/2011 (v5.1)

      integer, intent(out) :: roppMajorNumber
      integer, intent(out) :: roppMinorNumber
      integer, intent(out) :: roppPatchNumber
      integer, intent(out) :: roppErrorCode

      roppMajorNumber   = 11
      roppMinorNumber   =  1
      roppPatchNumber   =  0
      roppErrorCode     =  0

   end subroutine message_get_roppVersion


end module messages

