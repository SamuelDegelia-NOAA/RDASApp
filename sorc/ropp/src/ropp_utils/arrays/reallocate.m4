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
define(REALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/reallocate_code.m4)
')dnl
dnl
dnl --------------------------------------------------------------------
dnl 2. Header of the Fortran 90 source file
dnl --------------------------------------------------------------------
dnl
CVSID

!****f* Arrays/reallocate [0.1] *
!
! NAME
!    reallocate - Reallocate 1d arrays (subroutine).
!
! SYNOPSIS
!    use arrays
!      ...
!    call reallocate(array, newsize)
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
!    This subroutine supports integer, float, double, complex,
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
!      call reallocate(vector, n)
!
! SEE ALSO
!    preallocate
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
!    Revision 1.1  2001/03/07 08:24:15  marq
!    Rewrote reallocate and preallocate; now all datatypes and up
!    to seven dimensions are hendled. Also completed the documentation.
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

REALLOC(1, Text)
REALLOC(1, OneByteInt)
REALLOC(1, TwoByteInt)
REALLOC(1, FourByteInt)
REALLOC(1, EightByteInt)
REALLOC(1, FourByteReal)
REALLOC(1, EightByteReal)

!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

REALLOC(2, Text)
REALLOC(2, OneByteInt)
REALLOC(2, TwoByteInt)
REALLOC(2, FourByteInt)
REALLOC(2, EightByteInt)
REALLOC(2, FourByteReal)
REALLOC(2, EightByteReal)

!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

REALLOC(3, Text)
REALLOC(3, OneByteInt)
REALLOC(3, TwoByteInt)
REALLOC(3, FourByteInt)
REALLOC(3, EightByteInt)
REALLOC(3, FourByteReal)
REALLOC(3, EightByteReal)

!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

REALLOC(4, Text)
REALLOC(4, OneByteInt)
REALLOC(4, TwoByteInt)
REALLOC(4, FourByteInt)
REALLOC(4, EightByteInt)
REALLOC(4, FourByteReal)
REALLOC(4, EightByteReal)

!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

REALLOC(5, Text)
REALLOC(5, OneByteInt)
REALLOC(5, TwoByteInt)
REALLOC(5, FourByteInt)
REALLOC(5, EightByteInt)
REALLOC(5, FourByteReal)
REALLOC(5, EightByteReal)

!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

REALLOC(6, Text)
REALLOC(6, OneByteInt)
REALLOC(6, TwoByteInt)
REALLOC(6, FourByteInt)
REALLOC(6, EightByteInt)
REALLOC(6, FourByteReal)
REALLOC(6, EightByteReal)

!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

REALLOC(7, Text)
REALLOC(7, OneByteInt)
REALLOC(7, TwoByteInt)
REALLOC(7, FourByteInt)
REALLOC(7, EightByteInt)
REALLOC(7, FourByteReal)
REALLOC(7, EightByteReal)
