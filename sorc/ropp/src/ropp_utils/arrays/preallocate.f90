! $Id: $

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

function preallocate_1D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_Text

  implicit none

  character(len = *), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  character(len = 1024), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_Text


function preallocate_1D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_OneByteInt


function preallocate_1D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_TwoByteInt


function preallocate_1D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_FourByteInt


function preallocate_1D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_EightByteInt


function preallocate_1D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  real(kind = FourByteReal), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_FourByteReal


function preallocate_1D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_1D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts
  real(kind = EightByteReal), dimension(:), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts))

  newarray(1:min(cnts,size(array))) &
   = array(1:min(cnts,size(array)))

  deallocate(array)

end function preallocate_1D_EightByteReal



!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

function preallocate_2D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_Text

  implicit none

  character(len = *), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  character(len = 1024), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_Text


function preallocate_2D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_OneByteInt


function preallocate_2D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_TwoByteInt


function preallocate_2D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_FourByteInt


function preallocate_2D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_EightByteInt


function preallocate_2D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  real(kind = FourByteReal), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_FourByteReal


function preallocate_2D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_2D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts
  real(kind = EightByteReal), dimension(:, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)))

  deallocate(array)

end function preallocate_2D_EightByteReal



!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

function preallocate_3D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_Text

  implicit none

  character(len = *), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  character(len = 1024), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_Text


function preallocate_3D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_OneByteInt


function preallocate_3D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_TwoByteInt


function preallocate_3D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_FourByteInt


function preallocate_3D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_EightByteInt


function preallocate_3D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  real(kind = FourByteReal), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_FourByteReal


function preallocate_3D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_3D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts
  real(kind = EightByteReal), dimension(:, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)))

  deallocate(array)

end function preallocate_3D_EightByteReal



!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

function preallocate_4D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_Text

  implicit none

  character(len = *), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  character(len = 1024), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_Text


function preallocate_4D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_OneByteInt


function preallocate_4D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_TwoByteInt


function preallocate_4D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_FourByteInt


function preallocate_4D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_EightByteInt


function preallocate_4D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  real(kind = FourByteReal), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_FourByteReal


function preallocate_4D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_4D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts
  real(kind = EightByteReal), dimension(:, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)))

  deallocate(array)

end function preallocate_4D_EightByteReal



!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

function preallocate_5D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_Text

  implicit none

  character(len = *), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  character(len = 1024), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_Text


function preallocate_5D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_OneByteInt


function preallocate_5D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_TwoByteInt


function preallocate_5D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_FourByteInt


function preallocate_5D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_EightByteInt


function preallocate_5D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_FourByteReal


function preallocate_5D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_5D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)))

  deallocate(array)

end function preallocate_5D_EightByteReal



!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

function preallocate_6D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_Text

  implicit none

  character(len = *), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  character(len = 1024), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_Text


function preallocate_6D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_OneByteInt


function preallocate_6D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_TwoByteInt


function preallocate_6D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_FourByteInt


function preallocate_6D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_EightByteInt


function preallocate_6D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_FourByteReal


function preallocate_6D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_6D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)))

  deallocate(array)

end function preallocate_6D_EightByteReal



!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

function preallocate_7D_Text (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_Text

  implicit none

  character(len = *), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  character(len = 1024), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_Text


function preallocate_7D_OneByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_OneByteInt


function preallocate_7D_TwoByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_TwoByteInt


function preallocate_7D_FourByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_FourByteInt


function preallocate_7D_EightByteInt (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_EightByteInt


function preallocate_7D_FourByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_FourByteReal


function preallocate_7D_EightByteReal (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => preallocate_7D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

  newarray(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7))) &
   = array(1:min(cnts(1),size(array, 1)), &
           1:min(cnts(2),size(array, 2)), &
           1:min(cnts(3),size(array, 3)), &
           1:min(cnts(4),size(array, 4)), &
           1:min(cnts(5),size(array, 5)), &
           1:min(cnts(6),size(array, 6)), &
           1:min(cnts(7),size(array, 7)))

  deallocate(array)

end function preallocate_7D_EightByteReal


