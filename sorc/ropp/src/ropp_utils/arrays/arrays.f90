! $Id: $

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
    subroutine copy_alloc_1D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:), &
                             intent(in) :: array
      character(len = *), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_Text 
    subroutine copy_alloc_1D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_OneByteInt 
    subroutine copy_alloc_1D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_TwoByteInt 
    subroutine copy_alloc_1D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_FourByteInt 
    subroutine copy_alloc_1D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_EightByteInt 
    subroutine copy_alloc_1D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_FourByteReal 
    subroutine copy_alloc_1D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:), &
                             pointer    :: newarray
    end subroutine copy_alloc_1D_EightByteReal 
    subroutine copy_alloc_2D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:, :), &
                             intent(in) :: array
      character(len = *), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_Text 
    subroutine copy_alloc_2D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_OneByteInt 
    subroutine copy_alloc_2D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_TwoByteInt 
    subroutine copy_alloc_2D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_FourByteInt 
    subroutine copy_alloc_2D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_EightByteInt 
    subroutine copy_alloc_2D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_FourByteReal 
    subroutine copy_alloc_2D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_2D_EightByteReal 
    subroutine copy_alloc_3D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:, :, :), &
                             intent(in) :: array
      character(len = *), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_Text 
    subroutine copy_alloc_3D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_OneByteInt 
    subroutine copy_alloc_3D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_TwoByteInt 
    subroutine copy_alloc_3D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_FourByteInt 
    subroutine copy_alloc_3D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_EightByteInt 
    subroutine copy_alloc_3D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_FourByteReal 
    subroutine copy_alloc_3D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_3D_EightByteReal 
    subroutine copy_alloc_4D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :), &
                             intent(in) :: array
      character(len = *), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_Text 
    subroutine copy_alloc_4D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_OneByteInt 
    subroutine copy_alloc_4D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_TwoByteInt 
    subroutine copy_alloc_4D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_FourByteInt 
    subroutine copy_alloc_4D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_EightByteInt 
    subroutine copy_alloc_4D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_FourByteReal 
    subroutine copy_alloc_4D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_4D_EightByteReal 
    subroutine copy_alloc_5D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :), &
                             intent(in) :: array
      character(len = *), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_Text 
    subroutine copy_alloc_5D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_OneByteInt 
    subroutine copy_alloc_5D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_TwoByteInt 
    subroutine copy_alloc_5D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_FourByteInt 
    subroutine copy_alloc_5D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_EightByteInt 
    subroutine copy_alloc_5D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_FourByteReal 
    subroutine copy_alloc_5D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_5D_EightByteReal 
    subroutine copy_alloc_6D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      character(len = *), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_Text 
    subroutine copy_alloc_6D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_OneByteInt 
    subroutine copy_alloc_6D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_TwoByteInt 
    subroutine copy_alloc_6D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_FourByteInt 
    subroutine copy_alloc_6D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_EightByteInt 
    subroutine copy_alloc_6D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_FourByteReal 
    subroutine copy_alloc_6D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_6D_EightByteReal 
    subroutine copy_alloc_7D_Text (array, newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      character(len = *), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_Text 
    subroutine copy_alloc_7D_OneByteInt (array, newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_OneByteInt 
    subroutine copy_alloc_7D_TwoByteInt (array, newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_TwoByteInt 
    subroutine copy_alloc_7D_FourByteInt (array, newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_FourByteInt 
    subroutine copy_alloc_7D_EightByteInt (array, newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_EightByteInt 
    subroutine copy_alloc_7D_FourByteReal (array, newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_FourByteReal 
    subroutine copy_alloc_7D_EightByteReal (array, newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end subroutine copy_alloc_7D_EightByteReal 
  end interface

!-----------------------------------------------------------------------
! 1.2 Reallocation (subroutine)
!-----------------------------------------------------------------------

  interface callocate
    subroutine callocate_1D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_1D_Text 
    subroutine callocate_1D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_1D_OneByteInt 
    subroutine callocate_1D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_1D_TwoByteInt 
    subroutine callocate_1D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_1D_FourByteInt 
    subroutine callocate_1D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_1D_EightByteInt 
    subroutine callocate_1D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_1D_FourByteReal 
    subroutine callocate_1D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_1D_EightByteReal 
    subroutine callocate_2D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_2D_Text 
    subroutine callocate_2D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_2D_OneByteInt 
    subroutine callocate_2D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_2D_TwoByteInt 
    subroutine callocate_2D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_2D_FourByteInt 
    subroutine callocate_2D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_2D_EightByteInt 
    subroutine callocate_2D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_2D_FourByteReal 
    subroutine callocate_2D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_2D_EightByteReal 
    subroutine callocate_3D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_3D_Text 
    subroutine callocate_3D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_3D_OneByteInt 
    subroutine callocate_3D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_3D_TwoByteInt 
    subroutine callocate_3D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_3D_FourByteInt 
    subroutine callocate_3D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_3D_EightByteInt 
    subroutine callocate_3D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_3D_FourByteReal 
    subroutine callocate_3D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_3D_EightByteReal 
    subroutine callocate_4D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_4D_Text 
    subroutine callocate_4D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_4D_OneByteInt 
    subroutine callocate_4D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_4D_TwoByteInt 
    subroutine callocate_4D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_4D_FourByteInt 
    subroutine callocate_4D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_4D_EightByteInt 
    subroutine callocate_4D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_4D_FourByteReal 
    subroutine callocate_4D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_4D_EightByteReal 
    subroutine callocate_5D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_5D_Text 
    subroutine callocate_5D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_5D_OneByteInt 
    subroutine callocate_5D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_5D_TwoByteInt 
    subroutine callocate_5D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_5D_FourByteInt 
    subroutine callocate_5D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_5D_EightByteInt 
    subroutine callocate_5D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_5D_FourByteReal 
    subroutine callocate_5D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_5D_EightByteReal 
    subroutine callocate_6D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_6D_Text 
    subroutine callocate_6D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_6D_OneByteInt 
    subroutine callocate_6D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_6D_TwoByteInt 
    subroutine callocate_6D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_6D_FourByteInt 
    subroutine callocate_6D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_6D_EightByteInt 
    subroutine callocate_6D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_6D_FourByteReal 
    subroutine callocate_6D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_6D_EightByteReal 
    subroutine callocate_7D_Text (array, cnts, value)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      character(len = *),                  optional   :: value
    end subroutine callocate_7D_Text 
    subroutine callocate_7D_OneByteInt (array, cnts, value)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = OneByteInt),                  optional   :: value
    end subroutine callocate_7D_OneByteInt 
    subroutine callocate_7D_TwoByteInt (array, cnts, value)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = TwoByteInt),                  optional   :: value
    end subroutine callocate_7D_TwoByteInt 
    subroutine callocate_7D_FourByteInt (array, cnts, value)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = FourByteInt),                  optional   :: value
    end subroutine callocate_7D_FourByteInt 
    subroutine callocate_7D_EightByteInt (array, cnts, value)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = EightByteInt),                  optional   :: value
    end subroutine callocate_7D_EightByteInt 
    subroutine callocate_7D_FourByteReal (array, cnts, value)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      real(kind = FourByteReal),                  optional   :: value
    end subroutine callocate_7D_FourByteReal 
    subroutine callocate_7D_EightByteReal (array, cnts, value)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      real(kind = EightByteReal),                  optional   :: value
    end subroutine callocate_7D_EightByteReal 
  end interface


!-----------------------------------------------------------------------
! 1.3 Reallocation (subroutine)
!-----------------------------------------------------------------------

  interface reallocate
    subroutine reallocate_1D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_Text 
    subroutine reallocate_1D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_OneByteInt 
    subroutine reallocate_1D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_TwoByteInt 
    subroutine reallocate_1D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_FourByteInt 
    subroutine reallocate_1D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_EightByteInt 
    subroutine reallocate_1D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_FourByteReal 
    subroutine reallocate_1D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
    end subroutine reallocate_1D_EightByteReal 
    subroutine reallocate_2D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_Text 
    subroutine reallocate_2D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_OneByteInt 
    subroutine reallocate_2D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_TwoByteInt 
    subroutine reallocate_2D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_FourByteInt 
    subroutine reallocate_2D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_EightByteInt 
    subroutine reallocate_2D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_FourByteReal 
    subroutine reallocate_2D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
    end subroutine reallocate_2D_EightByteReal 
    subroutine reallocate_3D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_Text 
    subroutine reallocate_3D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_OneByteInt 
    subroutine reallocate_3D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_TwoByteInt 
    subroutine reallocate_3D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_FourByteInt 
    subroutine reallocate_3D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_EightByteInt 
    subroutine reallocate_3D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_FourByteReal 
    subroutine reallocate_3D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
    end subroutine reallocate_3D_EightByteReal 
    subroutine reallocate_4D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_Text 
    subroutine reallocate_4D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_OneByteInt 
    subroutine reallocate_4D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_TwoByteInt 
    subroutine reallocate_4D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_FourByteInt 
    subroutine reallocate_4D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_EightByteInt 
    subroutine reallocate_4D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_FourByteReal 
    subroutine reallocate_4D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
    end subroutine reallocate_4D_EightByteReal 
    subroutine reallocate_5D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_Text 
    subroutine reallocate_5D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_OneByteInt 
    subroutine reallocate_5D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_TwoByteInt 
    subroutine reallocate_5D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_FourByteInt 
    subroutine reallocate_5D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_EightByteInt 
    subroutine reallocate_5D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_FourByteReal 
    subroutine reallocate_5D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
    end subroutine reallocate_5D_EightByteReal 
    subroutine reallocate_6D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_Text 
    subroutine reallocate_6D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_OneByteInt 
    subroutine reallocate_6D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_TwoByteInt 
    subroutine reallocate_6D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_FourByteInt 
    subroutine reallocate_6D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_EightByteInt 
    subroutine reallocate_6D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_FourByteReal 
    subroutine reallocate_6D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
    end subroutine reallocate_6D_EightByteReal 
    subroutine reallocate_7D_Text (array, cnts)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_Text 
    subroutine reallocate_7D_OneByteInt (array, cnts)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_OneByteInt 
    subroutine reallocate_7D_TwoByteInt (array, cnts)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_TwoByteInt 
    subroutine reallocate_7D_FourByteInt (array, cnts)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_FourByteInt 
    subroutine reallocate_7D_EightByteInt (array, cnts)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_EightByteInt 
    subroutine reallocate_7D_FourByteReal (array, cnts)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_FourByteReal 
    subroutine reallocate_7D_EightByteReal (array, cnts)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
    end subroutine reallocate_7D_EightByteReal 
  end interface


!-----------------------------------------------------------------------
! 1.4 Reallocation (function)
!-----------------------------------------------------------------------

  interface preallocate
    function preallocate_1D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      character(len = 1024), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_Text 
    function preallocate_1D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_OneByteInt 
    function preallocate_1D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_TwoByteInt 
    function preallocate_1D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_FourByteInt 
    function preallocate_1D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_EightByteInt 
    function preallocate_1D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      real(kind = FourByteReal), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_FourByteReal 
    function preallocate_1D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:), &
                             pointer    :: array
      integer,               intent(in) :: cnts
      real(kind = EightByteReal), dimension(:), &
                             pointer    :: newarray
    end function preallocate_1D_EightByteReal 
    function preallocate_2D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      character(len = 1024), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_Text 
    function preallocate_2D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_OneByteInt 
    function preallocate_2D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_TwoByteInt 
    function preallocate_2D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_FourByteInt 
    function preallocate_2D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_EightByteInt 
    function preallocate_2D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      real(kind = FourByteReal), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_FourByteReal 
    function preallocate_2D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :), &
                             pointer    :: array
      integer, dimension(2), intent(in) :: cnts
      real(kind = EightByteReal), dimension(:, :), &
                             pointer    :: newarray
    end function preallocate_2D_EightByteReal 
    function preallocate_3D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      character(len = 1024), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_Text 
    function preallocate_3D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_OneByteInt 
    function preallocate_3D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_TwoByteInt 
    function preallocate_3D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_FourByteInt 
    function preallocate_3D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_EightByteInt 
    function preallocate_3D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      real(kind = FourByteReal), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_FourByteReal 
    function preallocate_3D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :), &
                             pointer    :: array
      integer, dimension(3), intent(in) :: cnts
      real(kind = EightByteReal), dimension(:, :, :), &
                             pointer    :: newarray
    end function preallocate_3D_EightByteReal 
    function preallocate_4D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      character(len = 1024), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_Text 
    function preallocate_4D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_OneByteInt 
    function preallocate_4D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_TwoByteInt 
    function preallocate_4D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_FourByteInt 
    function preallocate_4D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_EightByteInt 
    function preallocate_4D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_FourByteReal 
    function preallocate_4D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer    :: array
      integer, dimension(4), intent(in) :: cnts
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer    :: newarray
    end function preallocate_4D_EightByteReal 
    function preallocate_5D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      character(len = 1024), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_Text 
    function preallocate_5D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_OneByteInt 
    function preallocate_5D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_TwoByteInt 
    function preallocate_5D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_FourByteInt 
    function preallocate_5D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_EightByteInt 
    function preallocate_5D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_FourByteReal 
    function preallocate_5D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer    :: array
      integer, dimension(5), intent(in) :: cnts
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_5D_EightByteReal 
    function preallocate_6D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      character(len = 1024), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_Text 
    function preallocate_6D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_OneByteInt 
    function preallocate_6D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_TwoByteInt 
    function preallocate_6D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_FourByteInt 
    function preallocate_6D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_EightByteInt 
    function preallocate_6D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_FourByteReal 
    function preallocate_6D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(6), intent(in) :: cnts
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_6D_EightByteReal 
    function preallocate_7D_Text (array, cnts) result (newarray)
      use typeSizes
      character(len = *), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      character(len = 1024), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_Text 
    function preallocate_7D_OneByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_OneByteInt 
    function preallocate_7D_TwoByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_TwoByteInt 
    function preallocate_7D_FourByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_FourByteInt 
    function preallocate_7D_EightByteInt (array, cnts) result (newarray)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_EightByteInt 
    function preallocate_7D_FourByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_FourByteReal 
    function preallocate_7D_EightByteReal (array, cnts) result (newarray)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: array
      integer, dimension(7), intent(in) :: cnts
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer    :: newarray
    end function preallocate_7D_EightByteReal 
  end interface


!-----------------------------------------------------------------------
! 1.5 isinrange
!-----------------------------------------------------------------------

  interface isinrange
    function isinrange_0D_OneByteInt (value, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), &
                             intent(in) :: value
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_0D_OneByteInt 

    function isinrange_0D_TwoByteInt (value, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), &
                             intent(in) :: value
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_0D_TwoByteInt 

    function isinrange_0D_FourByteInt (value, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), &
                             intent(in) :: value
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_0D_FourByteInt 

    function isinrange_0D_EightByteInt (value, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), &
                             intent(in) :: value
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_0D_EightByteInt 

    function isinrange_0D_FourByteReal (value, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), &
                             intent(in) :: value
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_0D_FourByteReal 

    function isinrange_0D_EightByteReal (value, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), &
                             intent(in) :: value
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_0D_EightByteReal 

    function isinrange_1D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_1D_OneByteInt 

    function isinrange_1D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_1D_TwoByteInt 

    function isinrange_1D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_1D_FourByteInt 

    function isinrange_1D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_1D_EightByteInt 

    function isinrange_1D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_1D_FourByteReal 

    function isinrange_1D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_1D_EightByteReal 

    function isinrange_2D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_2D_OneByteInt 

    function isinrange_2D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_2D_TwoByteInt 

    function isinrange_2D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_2D_FourByteInt 

    function isinrange_2D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_2D_EightByteInt 

    function isinrange_2D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_2D_FourByteReal 

    function isinrange_2D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_2D_EightByteReal 

    function isinrange_3D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_3D_OneByteInt 

    function isinrange_3D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_3D_TwoByteInt 

    function isinrange_3D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_3D_FourByteInt 

    function isinrange_3D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_3D_EightByteInt 

    function isinrange_3D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_3D_FourByteReal 

    function isinrange_3D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_3D_EightByteReal 

    function isinrange_4D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_4D_OneByteInt 

    function isinrange_4D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_4D_TwoByteInt 

    function isinrange_4D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_4D_FourByteInt 

    function isinrange_4D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_4D_EightByteInt 

    function isinrange_4D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_4D_FourByteReal 

    function isinrange_4D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_4D_EightByteReal 

    function isinrange_5D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_5D_OneByteInt 

    function isinrange_5D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_5D_TwoByteInt 

    function isinrange_5D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_5D_FourByteInt 

    function isinrange_5D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_5D_EightByteInt 

    function isinrange_5D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_5D_FourByteReal 

    function isinrange_5D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_5D_EightByteReal 

    function isinrange_6D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_6D_OneByteInt 

    function isinrange_6D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_6D_TwoByteInt 

    function isinrange_6D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_6D_FourByteInt 

    function isinrange_6D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_6D_EightByteInt 

    function isinrange_6D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_6D_FourByteReal 

    function isinrange_6D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_6D_EightByteReal 

    function isinrange_7D_OneByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = OneByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_7D_OneByteInt 

    function isinrange_7D_TwoByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = TwoByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_7D_TwoByteInt 

    function isinrange_7D_FourByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = FourByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_7D_FourByteInt 

    function isinrange_7D_EightByteInt (array, range) result (inrange)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      integer(kind = EightByteInt), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_7D_EightByteInt 

    function isinrange_7D_FourByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = FourByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_7D_FourByteReal 

    function isinrange_7D_EightByteReal (array, range) result (inrange)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(in) :: array
      real(kind = EightByteReal), dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function isinrange_7D_EightByteReal 

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
    function reverse_1D_OneByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = OneByteInt), dimension(:), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = OneByteInt), dimension(size(array, 1)) &
                                      :: reversed
    end function reverse_1D_OneByteInt
    function reverse_1D_TwoByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = TwoByteInt), dimension(size(array, 1)) &
                                      :: reversed
    end function reverse_1D_TwoByteInt
    function reverse_1D_FourByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = FourByteInt), dimension(:), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = FourByteInt), dimension(size(array, 1)) &
                                      :: reversed
    end function reverse_1D_FourByteInt
    function reverse_1D_EightByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = EightByteInt), dimension(:), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = EightByteInt), dimension(size(array, 1)) &
                                      :: reversed
    end function reverse_1D_EightByteInt
    function reverse_1D_FourByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = FourByteReal), dimension(:), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = FourByteReal), dimension(size(array, 1)) &
                                      :: reversed
    end function reverse_1D_FourByteReal
    function reverse_1D_EightByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = EightByteReal), dimension(:), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = EightByteReal), dimension(size(array, 1)) &
                                      :: reversed
    end function reverse_1D_EightByteReal
    function reverse_2D_OneByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = OneByteInt), dimension(size(array, 1), size(array, 2)) &
                                      :: reversed
    end function reverse_2D_OneByteInt
    function reverse_2D_TwoByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = TwoByteInt), dimension(size(array, 1), size(array, 2)) &
                                      :: reversed
    end function reverse_2D_TwoByteInt
    function reverse_2D_FourByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = FourByteInt), dimension(size(array, 1), size(array, 2)) &
                                      :: reversed
    end function reverse_2D_FourByteInt
    function reverse_2D_EightByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = EightByteInt), dimension(size(array, 1), size(array, 2)) &
                                      :: reversed
    end function reverse_2D_EightByteInt
    function reverse_2D_FourByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = FourByteReal), dimension(size(array, 1), size(array, 2)) &
                                      :: reversed
    end function reverse_2D_FourByteReal
    function reverse_2D_EightByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = EightByteReal), dimension(size(array, 1), size(array, 2)) &
                                      :: reversed
    end function reverse_2D_EightByteReal
    function reverse_3D_OneByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = OneByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                      :: reversed
    end function reverse_3D_OneByteInt
    function reverse_3D_TwoByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = TwoByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                      :: reversed
    end function reverse_3D_TwoByteInt
    function reverse_3D_FourByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = FourByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                      :: reversed
    end function reverse_3D_FourByteInt
    function reverse_3D_EightByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = EightByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                      :: reversed
    end function reverse_3D_EightByteInt
    function reverse_3D_FourByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = FourByteReal), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                      :: reversed
    end function reverse_3D_FourByteReal
    function reverse_3D_EightByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = EightByteReal), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                      :: reversed
    end function reverse_3D_EightByteReal
    function reverse_4D_OneByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = OneByteInt), dimension(:, :, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = OneByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                      :: reversed
    end function reverse_4D_OneByteInt
    function reverse_4D_TwoByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = TwoByteInt), dimension(:, :, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = TwoByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                      :: reversed
    end function reverse_4D_TwoByteInt
    function reverse_4D_FourByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = FourByteInt), dimension(:, :, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = FourByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                      :: reversed
    end function reverse_4D_FourByteInt
    function reverse_4D_EightByteInt (array, dim) result (reversed)
      use typeSizes
      integer(kind = EightByteInt), dimension(:, :, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      integer(kind = EightByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                      :: reversed
    end function reverse_4D_EightByteInt
    function reverse_4D_FourByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = FourByteReal), dimension(:, :, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = FourByteReal), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                      :: reversed
    end function reverse_4D_FourByteReal
    function reverse_4D_EightByteReal (array, dim) result (reversed)
      use typeSizes
      real(kind = EightByteReal), dimension(:, :, :, :), &
                           intent(in) :: array
      integer,             optional   :: dim
      real(kind = EightByteReal), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                      :: reversed
    end function reverse_4D_EightByteReal
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
