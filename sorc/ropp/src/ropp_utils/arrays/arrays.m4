dnl
dnl process this file m4 to generate arrays.f90
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
define(`CVSID',`! $'`Id: $')dnl
dnl
define(COPYALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/copy_alloc_itfc.m4)
')dnl
define(SINRANGE, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/isinrange_sitfc.m4)
')dnl
define(AINRANGE, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/isinrange_aitfc.m4)
')dnl
define(CALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/callocate_itfc.m4)
')dnl
define(REALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/reallocate_itfc.m4)
')dnl
define(PREALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/preallocate_itfc.m4)
')dnl
define(REVERS_FUNC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
include(../arrays/auxiliary.m4)dnl
include(../arrays/reverse_itfc.m4)
')dnl
CVSID

module arrays

!****m* Arrays/arrays *
!
! NAME
!    arrays - Tools for handling arrays in Fortran 90.
!
! SYNOPSIS
!    use arrays
!
! DESCRIPTION
!    This module provides interfaces to several arrays related
!    functions and subroutines within the tools90 library. This
!    does not only include utilities to get indices of array
!    elements fulfilling certain conditions, but also tools for
!    sorting and unifying values of arrays, reversing fields
!    along one of their coordinates, reallocation of fields,
!    and simple mathematical operations like vector products
!    or the calculation of the norm and unit vector from a given
!    one dimensional vector.
!
! NOTES
!    All of the routines are at least implemented for float and
!    double, some also for single and double complex arguments
!    as well as for integer and string arguments (if applicable).
!    Routines returning different numbers of array elements or
!    changing the size of arrays and fields implemented via
!    pointers. This may cause significant performance problems
!    with some compilers. Also make sure that functions returning
!    pointers are not used with normal arrays, as this is likely
!    to cause memory leaks.
!
! SEE ALSO
!    Allocation:             copy_allocate, callocate
!    Reallocation:           reallocate, preallocate
!    Index functions:        where, nruns, getrun, uniq, unique, locate,
!                              iminloc, imaxloc, setminus
!    Sorting et al.:         sort, sorted, quick_sort
!    Array manipulation:     reverse, blend, swap, isinrange
!    Simple maths:           cross_product, outer_product, outer_and,
!                              l2norm, unit_vector
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

! The line below is needed for the PGI 12.x compiler.
  use typeSizes

  implicit none

!-----------------------------------------------------------------------
! 1. Utility routines
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! 1.1 Copy and free an array
!-----------------------------------------------------------------------

  interface copy_alloc
COPYALLOC(1, Text)dnl
COPYALLOC(1, OneByteInt)dnl
COPYALLOC(1, TwoByteInt)dnl
COPYALLOC(1, FourByteInt)dnl
COPYALLOC(1, EightByteInt)dnl
COPYALLOC(1, FourByteReal)dnl
COPYALLOC(1, EightByteReal)dnl
COPYALLOC(2, Text)dnl
COPYALLOC(2, OneByteInt)dnl
COPYALLOC(2, TwoByteInt)dnl
COPYALLOC(2, FourByteInt)dnl
COPYALLOC(2, EightByteInt)dnl
COPYALLOC(2, FourByteReal)dnl
COPYALLOC(2, EightByteReal)dnl
COPYALLOC(3, Text)dnl
COPYALLOC(3, OneByteInt)dnl
COPYALLOC(3, TwoByteInt)dnl
COPYALLOC(3, FourByteInt)dnl
COPYALLOC(3, EightByteInt)dnl
COPYALLOC(3, FourByteReal)dnl
COPYALLOC(3, EightByteReal)dnl
COPYALLOC(4, Text)dnl
COPYALLOC(4, OneByteInt)dnl
COPYALLOC(4, TwoByteInt)dnl
COPYALLOC(4, FourByteInt)dnl
COPYALLOC(4, EightByteInt)dnl
COPYALLOC(4, FourByteReal)dnl
COPYALLOC(4, EightByteReal)dnl
COPYALLOC(5, Text)dnl
COPYALLOC(5, OneByteInt)dnl
COPYALLOC(5, TwoByteInt)dnl
COPYALLOC(5, FourByteInt)dnl
COPYALLOC(5, EightByteInt)dnl
COPYALLOC(5, FourByteReal)dnl
COPYALLOC(5, EightByteReal)dnl
COPYALLOC(6, Text)dnl
COPYALLOC(6, OneByteInt)dnl
COPYALLOC(6, TwoByteInt)dnl
COPYALLOC(6, FourByteInt)dnl
COPYALLOC(6, EightByteInt)dnl
COPYALLOC(6, FourByteReal)dnl
COPYALLOC(6, EightByteReal)dnl
COPYALLOC(7, Text)dnl
COPYALLOC(7, OneByteInt)dnl
COPYALLOC(7, TwoByteInt)dnl
COPYALLOC(7, FourByteInt)dnl
COPYALLOC(7, EightByteInt)dnl
COPYALLOC(7, FourByteReal)dnl
COPYALLOC(7, EightByteReal)dnl
  end interface

!-----------------------------------------------------------------------
! 1.2 Reallocation (subroutine)
!-----------------------------------------------------------------------

  interface callocate
CALLOC(1, Text)dnl
CALLOC(1, OneByteInt)dnl
CALLOC(1, TwoByteInt)dnl
CALLOC(1, FourByteInt)dnl
CALLOC(1, EightByteInt)dnl
CALLOC(1, FourByteReal)dnl
CALLOC(1, EightByteReal)dnl
CALLOC(2, Text)dnl
CALLOC(2, OneByteInt)dnl
CALLOC(2, TwoByteInt)dnl
CALLOC(2, FourByteInt)dnl
CALLOC(2, EightByteInt)dnl
CALLOC(2, FourByteReal)dnl
CALLOC(2, EightByteReal)dnl
CALLOC(3, Text)dnl
CALLOC(3, OneByteInt)dnl
CALLOC(3, TwoByteInt)dnl
CALLOC(3, FourByteInt)dnl
CALLOC(3, EightByteInt)dnl
CALLOC(3, FourByteReal)dnl
CALLOC(3, EightByteReal)dnl
CALLOC(4, Text)dnl
CALLOC(4, OneByteInt)dnl
CALLOC(4, TwoByteInt)dnl
CALLOC(4, FourByteInt)dnl
CALLOC(4, EightByteInt)dnl
CALLOC(4, FourByteReal)dnl
CALLOC(4, EightByteReal)dnl
CALLOC(5, Text)dnl
CALLOC(5, OneByteInt)dnl
CALLOC(5, TwoByteInt)dnl
CALLOC(5, FourByteInt)dnl
CALLOC(5, EightByteInt)dnl
CALLOC(5, FourByteReal)dnl
CALLOC(5, EightByteReal)dnl
CALLOC(6, Text)dnl
CALLOC(6, OneByteInt)dnl
CALLOC(6, TwoByteInt)dnl
CALLOC(6, FourByteInt)dnl
CALLOC(6, EightByteInt)dnl
CALLOC(6, FourByteReal)dnl
CALLOC(6, EightByteReal)dnl
CALLOC(7, Text)dnl
CALLOC(7, OneByteInt)dnl
CALLOC(7, TwoByteInt)dnl
CALLOC(7, FourByteInt)dnl
CALLOC(7, EightByteInt)dnl
CALLOC(7, FourByteReal)dnl
CALLOC(7, EightByteReal)dnl
  end interface


!-----------------------------------------------------------------------
! 1.3 Reallocation (subroutine)
!-----------------------------------------------------------------------

  interface reallocate
REALLOC(1, Text)dnl
REALLOC(1, OneByteInt)dnl
REALLOC(1, TwoByteInt)dnl
REALLOC(1, FourByteInt)dnl
REALLOC(1, EightByteInt)dnl
REALLOC(1, FourByteReal)dnl
REALLOC(1, EightByteReal)dnl
REALLOC(2, Text)dnl
REALLOC(2, OneByteInt)dnl
REALLOC(2, TwoByteInt)dnl
REALLOC(2, FourByteInt)dnl
REALLOC(2, EightByteInt)dnl
REALLOC(2, FourByteReal)dnl
REALLOC(2, EightByteReal)dnl
REALLOC(3, Text)dnl
REALLOC(3, OneByteInt)dnl
REALLOC(3, TwoByteInt)dnl
REALLOC(3, FourByteInt)dnl
REALLOC(3, EightByteInt)dnl
REALLOC(3, FourByteReal)dnl
REALLOC(3, EightByteReal)dnl
REALLOC(4, Text)dnl
REALLOC(4, OneByteInt)dnl
REALLOC(4, TwoByteInt)dnl
REALLOC(4, FourByteInt)dnl
REALLOC(4, EightByteInt)dnl
REALLOC(4, FourByteReal)dnl
REALLOC(4, EightByteReal)dnl
REALLOC(5, Text)dnl
REALLOC(5, OneByteInt)dnl
REALLOC(5, TwoByteInt)dnl
REALLOC(5, FourByteInt)dnl
REALLOC(5, EightByteInt)dnl
REALLOC(5, FourByteReal)dnl
REALLOC(5, EightByteReal)dnl
REALLOC(6, Text)dnl
REALLOC(6, OneByteInt)dnl
REALLOC(6, TwoByteInt)dnl
REALLOC(6, FourByteInt)dnl
REALLOC(6, EightByteInt)dnl
REALLOC(6, FourByteReal)dnl
REALLOC(6, EightByteReal)dnl
REALLOC(7, Text)dnl
REALLOC(7, OneByteInt)dnl
REALLOC(7, TwoByteInt)dnl
REALLOC(7, FourByteInt)dnl
REALLOC(7, EightByteInt)dnl
REALLOC(7, FourByteReal)dnl
REALLOC(7, EightByteReal)dnl
  end interface


!-----------------------------------------------------------------------
! 1.4 Reallocation (function)
!-----------------------------------------------------------------------

  interface preallocate
PREALLOC(1, Text)dnl
PREALLOC(1, OneByteInt)dnl
PREALLOC(1, TwoByteInt)dnl
PREALLOC(1, FourByteInt)dnl
PREALLOC(1, EightByteInt)dnl
PREALLOC(1, FourByteReal)dnl
PREALLOC(1, EightByteReal)dnl
PREALLOC(2, Text)dnl
PREALLOC(2, OneByteInt)dnl
PREALLOC(2, TwoByteInt)dnl
PREALLOC(2, FourByteInt)dnl
PREALLOC(2, EightByteInt)dnl
PREALLOC(2, FourByteReal)dnl
PREALLOC(2, EightByteReal)dnl
PREALLOC(3, Text)dnl
PREALLOC(3, OneByteInt)dnl
PREALLOC(3, TwoByteInt)dnl
PREALLOC(3, FourByteInt)dnl
PREALLOC(3, EightByteInt)dnl
PREALLOC(3, FourByteReal)dnl
PREALLOC(3, EightByteReal)dnl
PREALLOC(4, Text)dnl
PREALLOC(4, OneByteInt)dnl
PREALLOC(4, TwoByteInt)dnl
PREALLOC(4, FourByteInt)dnl
PREALLOC(4, EightByteInt)dnl
PREALLOC(4, FourByteReal)dnl
PREALLOC(4, EightByteReal)dnl
PREALLOC(5, Text)dnl
PREALLOC(5, OneByteInt)dnl
PREALLOC(5, TwoByteInt)dnl
PREALLOC(5, FourByteInt)dnl
PREALLOC(5, EightByteInt)dnl
PREALLOC(5, FourByteReal)dnl
PREALLOC(5, EightByteReal)dnl
PREALLOC(6, Text)dnl
PREALLOC(6, OneByteInt)dnl
PREALLOC(6, TwoByteInt)dnl
PREALLOC(6, FourByteInt)dnl
PREALLOC(6, EightByteInt)dnl
PREALLOC(6, FourByteReal)dnl
PREALLOC(6, EightByteReal)dnl
PREALLOC(7, Text)dnl
PREALLOC(7, OneByteInt)dnl
PREALLOC(7, TwoByteInt)dnl
PREALLOC(7, FourByteInt)dnl
PREALLOC(7, EightByteInt)dnl
PREALLOC(7, FourByteReal)dnl
PREALLOC(7, EightByteReal)dnl
  end interface


!-----------------------------------------------------------------------
! 1.5 isinrange
!-----------------------------------------------------------------------

  interface isinrange
SINRANGE(0, OneByteInt)
SINRANGE(0, TwoByteInt)
SINRANGE(0, FourByteInt)
SINRANGE(0, EightByteInt)
SINRANGE(0, FourByteReal)
SINRANGE(0, EightByteReal)
AINRANGE(1, OneByteInt)
AINRANGE(1, TwoByteInt)
AINRANGE(1, FourByteInt)
AINRANGE(1, EightByteInt)
AINRANGE(1, FourByteReal)
AINRANGE(1, EightByteReal)
AINRANGE(2, OneByteInt)
AINRANGE(2, TwoByteInt)
AINRANGE(2, FourByteInt)
AINRANGE(2, EightByteInt)
AINRANGE(2, FourByteReal)
AINRANGE(2, EightByteReal)
AINRANGE(3, OneByteInt)
AINRANGE(3, TwoByteInt)
AINRANGE(3, FourByteInt)
AINRANGE(3, EightByteInt)
AINRANGE(3, FourByteReal)
AINRANGE(3, EightByteReal)
AINRANGE(4, OneByteInt)
AINRANGE(4, TwoByteInt)
AINRANGE(4, FourByteInt)
AINRANGE(4, EightByteInt)
AINRANGE(4, FourByteReal)
AINRANGE(4, EightByteReal)
AINRANGE(5, OneByteInt)
AINRANGE(5, TwoByteInt)
AINRANGE(5, FourByteInt)
AINRANGE(5, EightByteInt)
AINRANGE(5, FourByteReal)
AINRANGE(5, EightByteReal)
AINRANGE(6, OneByteInt)
AINRANGE(6, TwoByteInt)
AINRANGE(6, FourByteInt)
AINRANGE(6, EightByteInt)
AINRANGE(6, FourByteReal)
AINRANGE(6, EightByteReal)
AINRANGE(7, OneByteInt)
AINRANGE(7, TwoByteInt)
AINRANGE(7, FourByteInt)
AINRANGE(7, EightByteInt)
AINRANGE(7, FourByteReal)
AINRANGE(7, EightByteReal)
  end interface

!-----------------------------------------------------------------------
! 1.6 where
!-----------------------------------------------------------------------

  interface where_indices
     function where_indices(mask, n) result (indices)
       logical, dimension(:), intent(in) :: mask
       integer,               optional   :: n
       integer, dimension(:), pointer    :: indices
     end function where_indices
  end interface where_indices


!-----------------------------------------------------------------------
! 1.7 setminus
!-----------------------------------------------------------------------

  interface setminus
     function setminus_int(a, b) result (c)
	integer, dimension(:), intent(in) :: a
  	integer, dimension(:), intent(in) :: b
  	integer, dimension(:), pointer    :: c
     end function setminus_int
     function setminus_float(a, b) result (c)
        use typesizes, only: wp => FourByteReal
        real(wp), dimension(:), intent(in) :: a
        real(wp), dimension(:), intent(in) :: b
        real(wp), dimension(:), pointer    :: c
     end function setminus_float
     function setminus_double(a, b) result (c)
        use typesizes, only: wp => EightByteReal
        real(wp), dimension(:), intent(in) :: a
        real(wp), dimension(:), intent(in) :: b
        real(wp), dimension(:), pointer    :: c
     end function setminus_double
  end interface


!-----------------------------------------------------------------------
! 1.8 nruns
!-----------------------------------------------------------------------

  interface nruns
     function nruns(iarray) result (n_runs)
       integer, dimension(:), intent(in) :: iarray
       integer                           :: n_runs
     end function nruns
  end interface


!-----------------------------------------------------------------------
! 1.9 getrun
!-----------------------------------------------------------------------

  interface getrun
     function getrun(iarray, m, n, longest, last) result (run)
       integer, dimension(:), intent(in) :: iarray
       integer,               optional   :: n
       integer,               optional   :: m
       logical,               optional   :: longest
       logical,               optional   :: last
       integer, dimension(:), pointer    :: run
     end function getrun
  end interface


!-----------------------------------------------------------------------
! 1.10 uniq (indices of unique values in an array)
!-----------------------------------------------------------------------

  interface uniq
     function uniqs(array, idx) result (indices)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: array
       integer, dimension(:),  intent(in), optional   :: idx
       integer, dimension(:),  pointer    :: indices
     end function uniqs
     function uniqd(array, idx) result (indices)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: array
       integer,  dimension(:), intent(in), optional   :: idx
       integer,  dimension(:), pointer    :: indices
     end function uniqd
  end interface


!-----------------------------------------------------------------------
! 1.11 unique (values of unique values in an array)
!-----------------------------------------------------------------------

  interface unique
     function uniques(array, idx) result (values)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: array
       integer,  dimension(:), intent(in), optional   :: idx
       real(wp), dimension(:), pointer    :: values
     end function uniques
     function uniqued(array, idx) result (values)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: array
       integer,  dimension(:), intent(in), optional   :: idx
       real(wp), dimension(:), pointer    :: values
     end function uniqued
  end interface


!-----------------------------------------------------------------------
! 1.12 Reverse
!-----------------------------------------------------------------------

  interface reverse
REVERS_FUNC(1, OneByteInt)dnl
REVERS_FUNC(1, TwoByteInt)dnl
REVERS_FUNC(1, FourByteInt)dnl
REVERS_FUNC(1, EightByteInt)dnl
REVERS_FUNC(1, FourByteReal)dnl
REVERS_FUNC(1, EightByteReal)dnl
REVERS_FUNC(2, OneByteInt)dnl
REVERS_FUNC(2, TwoByteInt)dnl
REVERS_FUNC(2, FourByteInt)dnl
REVERS_FUNC(2, EightByteInt)dnl
REVERS_FUNC(2, FourByteReal)dnl
REVERS_FUNC(2, EightByteReal)dnl
REVERS_FUNC(3, OneByteInt)dnl
REVERS_FUNC(3, TwoByteInt)dnl
REVERS_FUNC(3, FourByteInt)dnl
REVERS_FUNC(3, EightByteInt)dnl
REVERS_FUNC(3, FourByteReal)dnl
REVERS_FUNC(3, EightByteReal)dnl
REVERS_FUNC(4, OneByteInt)dnl
REVERS_FUNC(4, TwoByteInt)dnl
REVERS_FUNC(4, FourByteInt)dnl
REVERS_FUNC(4, EightByteInt)dnl
REVERS_FUNC(4, FourByteReal)dnl
REVERS_FUNC(4, EightByteReal)dnl
dnl REVERS_FUNC(5, OneByteInt)dnl
dnl REVERS_FUNC(5, TwoByteInt)dnl
dnl REVERS_FUNC(5, FourByteInt)dnl
dnl REVERS_FUNC(5, EightByteInt)dnl
dnl REVERS_FUNC(5, FourByteReal)dnl
dnl REVERS_FUNC(5, EightByteReal)dnl
dnl REVERS_FUNC(6, OneByteInt)dnl
dnl REVERS_FUNC(6, TwoByteInt)dnl
dnl REVERS_FUNC(6, FourByteInt)dnl
dnl REVERS_FUNC(6, EightByteInt)dnl
dnl REVERS_FUNC(6, FourByteReal)dnl
dnl REVERS_FUNC(6, EightByteReal)dnl
dnl REVERS_FUNC(7, OneByteInt)dnl
dnl REVERS_FUNC(7, TwoByteInt)dnl
dnl REVERS_FUNC(7, FourByteInt)dnl
dnl REVERS_FUNC(7, EightByteInt)dnl
dnl REVERS_FUNC(7, FourByteReal)dnl
dnl REVERS_FUNC(7, EightByteReal)dnl
  end interface

!-----------------------------------------------------------------------
! 1.13 Blend
!-----------------------------------------------------------------------

  interface blend
     function blend(n, i, j) result (weights)
       use typesizes, only: wp => EightByteReal
       integer,  intent(in)   :: n
       integer,  intent(in)   :: i
       integer,  intent(in)   :: j
       real(wp), dimension(n) :: weights
     end function blend
  end interface

!-----------------------------------------------------------------------
! 1.14 Location of one or more numbers within an array
!-----------------------------------------------------------------------

  interface locate
     function locate_single_float(array, point) result(index)
       use typesizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: array
       real(wp),               intent(in) :: point
       integer                            :: index
     end function locate_single_float
     function locate_multi_float(array, points) result(index)
       use typesizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: array
       real(wp), dimension(:), intent(in) :: points
       integer,  dimension(size(points))  :: index
     end function locate_multi_float
     function locate_single_double(array, point) result(index)
       use typesizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: array
       real(wp),               intent(in) :: point
       integer                            :: index
     end function locate_single_double
     function locate_multi_double(array, points) result(index)
       use typesizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: array
       real(wp), dimension(:), intent(in) :: points
       integer,  dimension(size(points))  :: index
     end function locate_multi_double
  end interface

!-----------------------------------------------------------------------
! 1.15 Location of the minimum element of an array
!-----------------------------------------------------------------------

  interface iminloc
     function iminloc_int(array) result(idx)
       integer, dimension(:), intent(in) :: array
       integer                           :: idx
     end function iminloc_int
     function iminloc_float(array) result(idx)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: array
       integer                            :: idx
     end function iminloc_float
     function iminloc_double(array) result(idx)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: array
       integer                            :: idx
     end function iminloc_double
  end interface

!-----------------------------------------------------------------------
! 1.16 Location of the maximum element of an array
!-----------------------------------------------------------------------

  interface imaxloc
     function imaxloc_int(array) result(idx)
       integer, dimension(:), intent(in) :: array
       integer                           :: idx
     end function imaxloc_int
     function imaxloc_float(array) result(idx)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: array
       integer                            :: idx
     end function imaxloc_float
     function imaxloc_double(array) result(idx)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: array
       integer                            :: idx
     end function imaxloc_double
  end interface

!-----------------------------------------------------------------------
! 1.17 Swap elements
!-----------------------------------------------------------------------

  interface swap
     elemental subroutine swap_int(a, b)
       integer, intent(inout) :: a
       integer, intent(inout) :: b
     end subroutine swap_int
     elemental subroutine swap_float(a, b)
       use typesizes, only: wp => FourByteReal
       real(wp), intent(inout) :: a
       real(wp), intent(inout) :: b
     end subroutine swap_float
     elemental subroutine swap_double(a, b)
       use typesizes, only: wp => EightByteReal
       real(wp), intent(inout) :: a
       real(wp), intent(inout) :: b
     end subroutine swap_double
     elemental subroutine swap_complex_float(a, b)
       use typesizes, only: wp => FourByteReal
       complex(wp), intent(inout) :: a
       complex(wp), intent(inout) :: b
     end subroutine swap_complex_float
     elemental subroutine swap_complex_double(a, b)
       use typesizes, only: wp => EightByteReal
       complex(wp), intent(inout) :: a
       complex(wp), intent(inout) :: b
     end subroutine swap_complex_double
  end interface

!-----------------------------------------------------------------------
! 2. Sorting
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! 2.1 sort - sort an array (and return indices)
!-----------------------------------------------------------------------

  interface sort
     function sorts(list, reverse) result (indices)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:),         intent(in) :: list
       integer,  dimension(size(list))            :: indices
       integer,                        optional   :: reverse
     end function sorts
     function sortd(list, reverse) result (indices)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:),         intent(in) :: list
       integer,  dimension(size(list))            :: indices
       integer,                        optional   :: reverse
     end function sortd
  end interface


!-----------------------------------------------------------------------
! 2.2 sorted - sort an array (and return values)
!-----------------------------------------------------------------------

  interface sorted
     function sorteds(list, reverse) result (values)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:),         intent(in) :: list
       real(wp), dimension(size(list))            :: values
       integer,                        optional   :: reverse
     end function sorteds
     function sortedd(list, reverse) result (values)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:),         intent(in) :: list
       real(wp), dimension(size(list))            :: values
       integer,                        optional   :: reverse
     end function sortedd
  end interface


!-----------------------------------------------------------------------
! 2.3 Quick sort
!-----------------------------------------------------------------------

  interface quick_sort
     subroutine quick_sorts(list, order)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(inout) :: list
       integer,  dimension(:), intent  (out) :: order
     end subroutine quick_sorts
     subroutine quick_sortd(list, order)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(inout) :: list
       integer,  dimension(:), intent  (out) :: order
     end subroutine quick_sortd
  end interface


!-----------------------------------------------------------------------
! 3. Geometry
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! 3.1 Cross product
!-----------------------------------------------------------------------

  interface cross_product
     function cross_product_float(x, y) result(z)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(3), intent(in) :: x, y
       real(wp), dimension(3)             :: z
     end function cross_product_float
     function cross_product_double(x, y) result(z)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(3), intent(in) :: x, y
       real(wp), dimension(3)             :: z
     end function cross_product_double
  end interface


!-----------------------------------------------------------------------
! 3.2 Outer product
!-----------------------------------------------------------------------

  interface outer_product
     function outer_product_float(x, y) result(z)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in)    :: x, y
       real(wp), dimension(size(x), size(y)) :: z
     end function outer_product_float
     function outer_product_double(x, y) result(z)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in)    :: x, y
       real(wp), dimension(size(x), size(y)) :: z
     end function outer_product_double
  end interface

!-----------------------------------------------------------------------
! 3.3 Outer and
!-----------------------------------------------------------------------

  interface outer_and
     function outer_and(x, y) result(z)
       logical, dimension(:), intent(in)    :: x, y
       logical, dimension(size(x), size(y)) :: z
     end function outer_and
  end interface

!-----------------------------------------------------------------------
! 3.3 L2 Norm
!-----------------------------------------------------------------------

  interface l2norm
     function l2norm_float(vector) result(norm)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: vector
       real(wp)                           :: norm
     end function l2norm_float
     function l2norm_double(vector) result(norm)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: vector
       real(wp)                           :: norm
     end function l2norm_double
     function l2norm_complex(vector) result(norm)
       use typeSizes, only: wp => FourByteReal
       complex(wp), dimension(:), intent(in) :: vector
       complex(wp)                           :: norm
     end function l2norm_complex
     function l2norm_doublecomplex(vector) result(norm)
       use typeSizes, only: wp => EightByteReal
       complex(wp), dimension(:), intent(in) :: vector
       complex(wp)                           :: norm
     end function l2norm_doublecomplex
  end interface


!-----------------------------------------------------------------------
! 3.4 Unit vector
!-----------------------------------------------------------------------

  interface unit_vector
     function unit_vector_float(vector) result(uvector)
       use typeSizes, only: wp => FourByteReal
       real(wp), dimension(:), intent(in) :: vector
       real(wp), dimension(size(vector))  :: uvector
     end function unit_vector_float
     function unit_vector_double(vector) result(uvector)
       use typeSizes, only: wp => EightByteReal
       real(wp), dimension(:), intent(in) :: vector
       real(wp), dimension(size(vector))  :: uvector
     end function unit_vector_double
     function unit_vector_complex(vector) result(uvector)
       use typeSizes, only: wp => FourByteReal
       complex(wp), dimension(:), intent(in) :: vector
       complex(wp), dimension(size(vector))  :: uvector
     end function unit_vector_complex
     function unit_vector_doublecomplex(vector) result(uvector)
       use typeSizes, only: wp => EightByteReal
       complex(wp), dimension(:), intent(in) :: vector
       complex(wp), dimension(size(vector))  :: uvector
     end function unit_vector_doublecomplex
  end interface

end module arrays
