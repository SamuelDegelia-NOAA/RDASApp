dnl $Id$
dnl
dnl
dnl Process this file with m4 to produce the file callocate.f90.
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
define(CALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/callocate_code.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

!****s* Arrays/callocate *
!
! NAME
!    callocate - Allocate arrays and initialise them with a given value.
!
! SYNOPSIS
!    call callocate(array, newsize, [value])
! 
! DESCRIPTION
!    This subroutine allocates an array or field to the given size and
!    initialises the array with a given value (or zero if no value is
!    given).
!
! INPUTS
!    ..., dim(:[,:[...]]), pointer :: array
!    integer [, dim(:[...])]       :: newsize
!
! OUTPUT
!    ..., dim(:[,:[...]]), pointer :: array
!
! NOTES
!    This subroutine supports integer, float, double, complex,
!    double complex arguments for up to seven dimensions.
!
! SEE ALSO
!    reallocate
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
!    Revision 1.1  2004/10/14 10:43:27  frcm
!    Added callocate().
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(1, Text)
CALLOC(1, OneByteInt)
CALLOC(1, TwoByteInt)
CALLOC(1, FourByteInt)
CALLOC(1, EightByteInt)
CALLOC(1, FourByteReal)
CALLOC(1, EightByteReal)

!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(2, Text)
CALLOC(2, OneByteInt)
CALLOC(2, TwoByteInt)
CALLOC(2, FourByteInt)
CALLOC(2, EightByteInt)
CALLOC(2, FourByteReal)
CALLOC(2, EightByteReal)

!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(3, Text)
CALLOC(3, OneByteInt)
CALLOC(3, TwoByteInt)
CALLOC(3, FourByteInt)
CALLOC(3, EightByteInt)
CALLOC(3, FourByteReal)
CALLOC(3, EightByteReal)

!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(4, Text)
CALLOC(4, OneByteInt)
CALLOC(4, TwoByteInt)
CALLOC(4, FourByteInt)
CALLOC(4, EightByteInt)
CALLOC(4, FourByteReal)
CALLOC(4, EightByteReal)

!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(5, Text)
CALLOC(5, OneByteInt)
CALLOC(5, TwoByteInt)
CALLOC(5, FourByteInt)
CALLOC(5, EightByteInt)
CALLOC(5, FourByteReal)
CALLOC(5, EightByteReal)

!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(6, Text)
CALLOC(6, OneByteInt)
CALLOC(6, TwoByteInt)
CALLOC(6, FourByteInt)
CALLOC(6, EightByteInt)
CALLOC(6, FourByteReal)
CALLOC(6, EightByteReal)

!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

dnl CALLOC(7, Text)
CALLOC(7, OneByteInt)
CALLOC(7, TwoByteInt)
CALLOC(7, FourByteInt)
CALLOC(7, EightByteInt)
CALLOC(7, FourByteReal)
CALLOC(7, EightByteReal)
