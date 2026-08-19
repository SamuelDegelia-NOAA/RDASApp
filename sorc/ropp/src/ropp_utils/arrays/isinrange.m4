dnl $Id$
dnl
dnl
dnl Process this file with m4 to produce the file isinrange.f90.
dnl
dnl C. Marquardt    <christian@marquardt.fsnet.co.uk>
dnl
dnl
dnl --------------------------------------------------------------------
dnl 1. m4 macro definitions
dnl --------------------------------------------------------------------
dnl
define(`CVSID',`! $'`Id: $')dnl
dnl
define(SINRANGE, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/isinrange_scode.m4)
')dnl
define(AINRANGE, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/isinrange_acode.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

!****s* Arrays/isinrange *
!
! NAME
!    isinrange - Check if data values are within a given range of numbers.
!
! SYNOPSIS
!    ... = isinrange(data, range)
! 
! DESCRIPTION
!    This function returns .true. if all elements of an array are within
!    a given range, .false. if at least one element is outside the given
!    range.
!
! INPUTS
!    ..., dim(:[,:[...]]) :: data
!    ..., dimension(2)    :: range
!
! OUTPUT
!    logical              :: isinrange
!
! NOTES
!    The array range is a two element vector giving the minimum and
!    maximum value for the allowed (valid) range.
!
!    This subroutine supports integer, float and double arguments for
!    up to seven dimensions. Scalar data values can also be tested.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
! MODIFICATION HISTORY
!
!    $Log$
!    Revision 1.1  2005/05/11 11:20:41  frcm
!    Imported from the tools90 library.
!
!    Revision 1.2  2005/05/11 07:37:43  frcm
!    *** empty log message ***
!
!    Revision 1.1  2005/01/26 13:53:38  frcm
!    Added isinrange() function.
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

AINRANGE(1, OneByteInt)
AINRANGE(1, TwoByteInt)
AINRANGE(1, FourByteInt)
AINRANGE(1, EightByteInt)
AINRANGE(1, FourByteReal)
AINRANGE(1, EightByteReal)

!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

AINRANGE(2, OneByteInt)
AINRANGE(2, TwoByteInt)
AINRANGE(2, FourByteInt)
AINRANGE(2, EightByteInt)
AINRANGE(2, FourByteReal)
AINRANGE(2, EightByteReal)

!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

AINRANGE(3, OneByteInt)
AINRANGE(3, TwoByteInt)
AINRANGE(3, FourByteInt)
AINRANGE(3, EightByteInt)
AINRANGE(3, FourByteReal)
AINRANGE(3, EightByteReal)

!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

AINRANGE(4, OneByteInt)
AINRANGE(4, TwoByteInt)
AINRANGE(4, FourByteInt)
AINRANGE(4, EightByteInt)
AINRANGE(4, FourByteReal)
AINRANGE(4, EightByteReal)

!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

AINRANGE(5, OneByteInt)
AINRANGE(5, TwoByteInt)
AINRANGE(5, FourByteInt)
AINRANGE(5, EightByteInt)
AINRANGE(5, FourByteReal)
AINRANGE(5, EightByteReal)

!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

AINRANGE(6, OneByteInt)
AINRANGE(6, TwoByteInt)
AINRANGE(6, FourByteInt)
AINRANGE(6, EightByteInt)
AINRANGE(6, FourByteReal)
AINRANGE(6, EightByteReal)

!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

AINRANGE(7, OneByteInt)
AINRANGE(7, TwoByteInt)
AINRANGE(7, FourByteInt)
AINRANGE(7, EightByteInt)
AINRANGE(7, FourByteReal)
AINRANGE(7, EightByteReal)

!--------------------------------------------------------------------------
! 8. Scalar arguments
!--------------------------------------------------------------------------

SINRANGE(0, OneByteInt)
SINRANGE(0, TwoByteInt)
SINRANGE(0, FourByteInt)
SINRANGE(0, EightByteInt)
SINRANGE(0, FourByteReal)
SINRANGE(0, EightByteReal)
