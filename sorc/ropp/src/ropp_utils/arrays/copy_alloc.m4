dnl $Id$
dnl
dnl
dnl Process this file with m4 to produce the file copy_and_free.f90.
dnl
dnl C. Marquardt    <christian.marquardt@metoffice.com>
dnl
dnl
dnl --------------------------------------------------------------------
dnl 1. m4 macro definitions
dnl --------------------------------------------------------------------
dnl
define(`CVSID',`! $'`Id: $')dnl
dnl
define(COPYALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/copy_alloc_code.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

!****s* Arrays/copy_alloc *
!
! NAME
!    copy_alloc - Copy data into a newly allocated array.
!
! SYNOPSIS
!    call copy_alloc(array, newarray)
! 
! DESCRIPTION
!    This subroutine copies the values in array into the newly allocated
!    newarray. Previous contents of newaray are lost.
!
! INPUTS
!    ..., dim(:[,:[...]]) :: array
!
! OUTPUT
!    ..., dim(:[,:[...]]), pointer :: newarray
!
! NOTES
!    On exit, newarray will have the same shape as array.
!
!    This subroutine supports integer, float, double, complex,
!    double complex and character/string arguments for up to
!    seven dimensions.
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
!    Revision 1.3  2005/05/11 07:37:43  frcm
!    *** empty log message ***
!
!    Revision 1.2  2003/11/12 15:05:47  frcm
!    Bug in the inclusion of the master m4 file containing the code; this
!    file actually included it itself. Fixed.
!
!    Revision 1.1  2003/11/12 14:46:47  frcm
!    Renamed from copy_and_free().
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

COPYALLOC(1, Text)
COPYALLOC(1, OneByteInt)
COPYALLOC(1, TwoByteInt)
COPYALLOC(1, FourByteInt)
COPYALLOC(1, EightByteInt)
COPYALLOC(1, FourByteReal)
COPYALLOC(1, EightByteReal)

!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

COPYALLOC(2, Text)
COPYALLOC(2, OneByteInt)
COPYALLOC(2, TwoByteInt)
COPYALLOC(2, FourByteInt)
COPYALLOC(2, EightByteInt)
COPYALLOC(2, FourByteReal)
COPYALLOC(2, EightByteReal)

!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

COPYALLOC(3, Text)
COPYALLOC(3, OneByteInt)
COPYALLOC(3, TwoByteInt)
COPYALLOC(3, FourByteInt)
COPYALLOC(3, EightByteInt)
COPYALLOC(3, FourByteReal)
COPYALLOC(3, EightByteReal)

!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

COPYALLOC(4, Text)
COPYALLOC(4, OneByteInt)
COPYALLOC(4, TwoByteInt)
COPYALLOC(4, FourByteInt)
COPYALLOC(4, EightByteInt)
COPYALLOC(4, FourByteReal)
COPYALLOC(4, EightByteReal)

!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

COPYALLOC(5, Text)
COPYALLOC(5, OneByteInt)
COPYALLOC(5, TwoByteInt)
COPYALLOC(5, FourByteInt)
COPYALLOC(5, EightByteInt)
COPYALLOC(5, FourByteReal)
COPYALLOC(5, EightByteReal)

!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

COPYALLOC(6, Text)
COPYALLOC(6, OneByteInt)
COPYALLOC(6, TwoByteInt)
COPYALLOC(6, FourByteInt)
COPYALLOC(6, EightByteInt)
COPYALLOC(6, FourByteReal)
COPYALLOC(6, EightByteReal)

!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

COPYALLOC(7, Text)
COPYALLOC(7, OneByteInt)
COPYALLOC(7, TwoByteInt)
COPYALLOC(7, FourByteInt)
COPYALLOC(7, EightByteInt)
COPYALLOC(7, FourByteReal)
COPYALLOC(7, EightByteReal)
