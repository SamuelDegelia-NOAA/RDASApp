! $Id: $

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

subroutine copy_alloc_1D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_Text

  implicit none

  character(len = *), dimension(:), &
                         intent(in) :: array
  character(len = *), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_Text


subroutine copy_alloc_1D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_OneByteInt


subroutine copy_alloc_1D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_TwoByteInt


subroutine copy_alloc_1D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_FourByteInt


subroutine copy_alloc_1D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_EightByteInt


subroutine copy_alloc_1D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_FourByteReal


subroutine copy_alloc_1D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_1D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1)))

! Copy data
! ---------

  newarray(:) = array(:)

end subroutine copy_alloc_1D_EightByteReal



!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

subroutine copy_alloc_2D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_Text

  implicit none

  character(len = *), dimension(:, :), &
                         intent(in) :: array
  character(len = *), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_Text


subroutine copy_alloc_2D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_OneByteInt


subroutine copy_alloc_2D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_TwoByteInt


subroutine copy_alloc_2D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_FourByteInt


subroutine copy_alloc_2D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_EightByteInt


subroutine copy_alloc_2D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_FourByteReal


subroutine copy_alloc_2D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_2D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2)))

! Copy data
! ---------

  newarray(:, :) = array(:, :)

end subroutine copy_alloc_2D_EightByteReal



!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

subroutine copy_alloc_3D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_Text

  implicit none

  character(len = *), dimension(:, :, :), &
                         intent(in) :: array
  character(len = *), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_Text


subroutine copy_alloc_3D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_OneByteInt


subroutine copy_alloc_3D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_TwoByteInt


subroutine copy_alloc_3D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_FourByteInt


subroutine copy_alloc_3D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_EightByteInt


subroutine copy_alloc_3D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_FourByteReal


subroutine copy_alloc_3D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_3D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3)))

! Copy data
! ---------

  newarray(:, :, :) = array(:, :, :)

end subroutine copy_alloc_3D_EightByteReal



!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

subroutine copy_alloc_4D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_Text

  implicit none

  character(len = *), dimension(:, :, :, :), &
                         intent(in) :: array
  character(len = *), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_Text


subroutine copy_alloc_4D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_OneByteInt


subroutine copy_alloc_4D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_TwoByteInt


subroutine copy_alloc_4D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_FourByteInt


subroutine copy_alloc_4D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_EightByteInt


subroutine copy_alloc_4D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_FourByteReal


subroutine copy_alloc_4D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_4D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4)))

! Copy data
! ---------

  newarray(:, :, :, :) = array(:, :, :, :)

end subroutine copy_alloc_4D_EightByteReal



!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

subroutine copy_alloc_5D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_Text

  implicit none

  character(len = *), dimension(:, :, :, :, :), &
                         intent(in) :: array
  character(len = *), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_Text


subroutine copy_alloc_5D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_OneByteInt


subroutine copy_alloc_5D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_TwoByteInt


subroutine copy_alloc_5D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_FourByteInt


subroutine copy_alloc_5D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_EightByteInt


subroutine copy_alloc_5D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_FourByteReal


subroutine copy_alloc_5D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_5D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5)))

! Copy data
! ---------

  newarray(:, :, :, :, :) = array(:, :, :, :, :)

end subroutine copy_alloc_5D_EightByteReal



!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

subroutine copy_alloc_6D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_Text

  implicit none

  character(len = *), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  character(len = *), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_Text


subroutine copy_alloc_6D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_OneByteInt


subroutine copy_alloc_6D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_TwoByteInt


subroutine copy_alloc_6D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_FourByteInt


subroutine copy_alloc_6D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_EightByteInt


subroutine copy_alloc_6D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_FourByteReal


subroutine copy_alloc_6D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_6D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :) = array(:, :, :, :, :, :)

end subroutine copy_alloc_6D_EightByteReal



!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

subroutine copy_alloc_7D_Text (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_Text

  implicit none

  character(len = *), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  character(len = *), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_Text


subroutine copy_alloc_7D_OneByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_OneByteInt


subroutine copy_alloc_7D_TwoByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_TwoByteInt


subroutine copy_alloc_7D_FourByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_FourByteInt


subroutine copy_alloc_7D_EightByteInt (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_EightByteInt


subroutine copy_alloc_7D_FourByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_FourByteReal


subroutine copy_alloc_7D_EightByteReal (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => copy_alloc_7D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(size(array, 1), size(array, 2), size(array, 3), size(array, 4), size(array, 5), size(array, 6), size(array, 7)))

! Copy data
! ---------

  newarray(:, :, :, :, :, :, :) = array(:, :, :, :, :, :, :)

end subroutine copy_alloc_7D_EightByteReal


