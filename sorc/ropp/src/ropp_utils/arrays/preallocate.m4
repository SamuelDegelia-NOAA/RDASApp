dnl $Id$
dnl
dnl Process this file with m4 to produce the file reallocate.f90.
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
define(PREALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/preallocate_code.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

!--------------------------------------------------------------------------
! 1. Reallocation (function)
!--------------------------------------------------------------------------

!****f* Arrays/preallocate *
!
! NAME
!    preallocate - Reallocate 1d arrays (function).
!
! SYNOPSIS
!    use arrays
!      ...
!    array => preallocate(array, newsize)
! 
! DESCRIPTION
!    This subroutine reallocates an array or field to a new size and
!    rescues the previous contents if possible (i.e., the array
!    is enlarged, the old values will be available in the first
!    elements of the new, enlarged array. If an array is shrinked,
!    superfluous elements will be lost. If an array is enlarged,
!    the additional elements are undetermined.
!
! INPUTS
!    ..., dim(:[,:[...]]), pointer :: array
!    integer [, dim(:[...])]       :: newsize
!
! OUTPUT
!    ..., dim(:[,:[...]]), pointer :: array
!
! NOTES
!    If an array is shrinked, superfluous elements will be lost.
!    If an array is enlarged, the compiler will fill in whatever
!    is available into the additional elements; what is filled in
!    depends solely on the state of the heap and the compiler's
!    internals and cannot be relied on.
!    If the pointer receiving the reallocated arrays is different from
!    from the argument given to the function, preallocate will allocate
!    a new pointer, copy newsize elements from the old pointer to the
!    new one and destroy the old pointer. Any previous contents of the
!    receiving pointer will be lost.
!    Make sure to use pointer notation for receiving the reallocated
!    pointer array, or a memory leak and performance reduction (due to
!    copying the old data twice) will almost certainly occure.
!    This function supports integer, float, double, complex,
!    double complex and character/string arguments for up to
!    seven dimensions.
!
! EXAMPLE
!    To enlarge a 1d vector with m elements to n elements (with n > m),
!    try
!
!      use arrays
!         ...
!      real, dimension(:), pointer :: vector
!      integer                     :: m, n
!         ...
!      allocate(vector(m))
!         ...
!      vector => preallocate(vector, n)
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
!    Revision 1.1  2005/05/11 11:20:42  frcm
!    Imported from the tools90 library.
!
!    Revision 1.2  2005/05/11 07:37:43  frcm
!    *** empty log message ***
!
!    Revision 1.1  2001/03/07 08:24:05  marq
!    Rewrote reallocate and preallocate; now all datatypes and up
!    to seven dimensions are hendled. Also completed the documentation.
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

PREALLOC(1, Text)
PREALLOC(1, OneByteInt)
PREALLOC(1, TwoByteInt)
PREALLOC(1, FourByteInt)
PREALLOC(1, EightByteInt)
PREALLOC(1, FourByteReal)
PREALLOC(1, EightByteReal)

!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

PREALLOC(2, Text)
PREALLOC(2, OneByteInt)
PREALLOC(2, TwoByteInt)
PREALLOC(2, FourByteInt)
PREALLOC(2, EightByteInt)
PREALLOC(2, FourByteReal)
PREALLOC(2, EightByteReal)

!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

PREALLOC(3, Text)
PREALLOC(3, OneByteInt)
PREALLOC(3, TwoByteInt)
PREALLOC(3, FourByteInt)
PREALLOC(3, EightByteInt)
PREALLOC(3, FourByteReal)
PREALLOC(3, EightByteReal)

!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

PREALLOC(4, Text)
PREALLOC(4, OneByteInt)
PREALLOC(4, TwoByteInt)
PREALLOC(4, FourByteInt)
PREALLOC(4, EightByteInt)
PREALLOC(4, FourByteReal)
PREALLOC(4, EightByteReal)

!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

PREALLOC(5, Text)
PREALLOC(5, OneByteInt)
PREALLOC(5, TwoByteInt)
PREALLOC(5, FourByteInt)
PREALLOC(5, EightByteInt)
PREALLOC(5, FourByteReal)
PREALLOC(5, EightByteReal)

!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

PREALLOC(6, Text)
PREALLOC(6, OneByteInt)
PREALLOC(6, TwoByteInt)
PREALLOC(6, FourByteInt)
PREALLOC(6, EightByteInt)
PREALLOC(6, FourByteReal)
PREALLOC(6, EightByteReal)

!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

PREALLOC(7, Text)
PREALLOC(7, OneByteInt)
PREALLOC(7, TwoByteInt)
PREALLOC(7, FourByteInt)
PREALLOC(7, EightByteInt)
PREALLOC(7, FourByteReal)
PREALLOC(7, EightByteReal)
