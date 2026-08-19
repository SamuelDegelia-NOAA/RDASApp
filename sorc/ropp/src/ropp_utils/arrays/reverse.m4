dnl $Id$
dnl
dnl Process this file with m4 to produce the file reverse.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
dnl --------------------------------------------------------------------
dnl 1. m4 macro definitions
dnl --------------------------------------------------------------------
dnl
define(`CVSID',`! $'`Id: $')dnl
dnl
define(REVERS_FUNC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/reverse_code.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

!****f* Arrays/reverse *
!
! NAME
!    reverse - Reverse an array along a given dimension.
!
! SYNOPSIS
!    use arrays
!      ...
!    array = reverse(array [, dim])
!
! INPUTS
!    array   array to be reversed.
!    dim     dimension along which the array is to be reversed.
!
! OUTPUT
!    array   reversed data.
!
! DESCRIPTION
!    This subroutine reverses an array along a given dimension (or along
!    the first dimension is dim is not given.
!
! EXAMPLE
!    To reverse the order of a 1-dimensional array, use
!
!       array = reverse(array)
!
!    which is identical to
!
!       array = array(size(array):1:-1)
!
!    To invert a 2-dimensional array along its second dimension,
!
!       array(:,:) = reverse(array, 2)
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
! MODIFICATION HISTORY
!
!    $Log$
!    Revision 1.1  2005/05/11 11:20:42  frcm
!    Imported from the tools90 library.
!
!    Revision 1.3  2005/05/11 07:37:43  frcm
!    *** empty log message ***
!
!**** 

!--------------------------------------------------------------------------
! 2. 1D array arguments
!--------------------------------------------------------------------------

REVERS_FUNC(1, OneByteInt)
REVERS_FUNC(1, TwoByteInt)
REVERS_FUNC(1, FourByteInt)
REVERS_FUNC(1, EightByteInt)
REVERS_FUNC(1, FourByteReal)
REVERS_FUNC(1, EightByteReal)

!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

REVERS_FUNC(2, OneByteInt)
REVERS_FUNC(2, TwoByteInt)
REVERS_FUNC(2, FourByteInt)
REVERS_FUNC(2, EightByteInt)
REVERS_FUNC(2, FourByteReal)
REVERS_FUNC(2, EightByteReal)

!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

REVERS_FUNC(3, OneByteInt)
REVERS_FUNC(3, TwoByteInt)
REVERS_FUNC(3, FourByteInt)
REVERS_FUNC(3, EightByteInt)
REVERS_FUNC(3, FourByteReal)
REVERS_FUNC(3, EightByteReal)

!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

REVERS_FUNC(4, OneByteInt)
REVERS_FUNC(4, TwoByteInt)
REVERS_FUNC(4, FourByteInt)
REVERS_FUNC(4, EightByteInt)
REVERS_FUNC(4, FourByteReal)
REVERS_FUNC(4, EightByteReal)

!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------

dnl REVERS_FUNC(5, OneByteInt)
dnl REVERS_FUNC(5, TwoByteInt)
dnl REVERS_FUNC(5, FourByteInt)
dnl REVERS_FUNC(5, EightByteInt)
dnl REVERS_FUNC(5, FourByteReal)
dnl REVERS_FUNC(5, EightByteReal)

!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------

dnl REVERS_FUNC(6, OneByteInt)
dnl REVERS_FUNC(6, TwoByteInt)
dnl REVERS_FUNC(6, FourByteInt)
dnl REVERS_FUNC(6, EightByteInt)
dnl REVERS_FUNC(6, FourByteReal)
dnl REVERS_FUNC(6, EightByteReal)

!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

dnl REVERS_FUNC(7, OneByteInt)
dnl REVERS_FUNC(7, TwoByteInt)
dnl REVERS_FUNC(7, FourByteInt)
dnl REVERS_FUNC(7, EightByteInt)
dnl REVERS_FUNC(7, FourByteReal)
dnl REVERS_FUNC(7, EightByteReal)
