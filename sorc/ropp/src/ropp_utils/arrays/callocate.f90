! $Id: $

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

subroutine callocate_1D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_1D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_1D_OneByteInt


subroutine callocate_1D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_1D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_1D_TwoByteInt


subroutine callocate_1D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_1D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_1D_FourByteInt


subroutine callocate_1D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_1D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_1D_EightByteInt


subroutine callocate_1D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_1D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_1D_FourByteReal


subroutine callocate_1D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_1D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:), &
                         pointer    :: array
  integer,               intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_1D_EightByteReal



!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

subroutine callocate_2D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_2D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_2D_OneByteInt


subroutine callocate_2D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_2D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_2D_TwoByteInt


subroutine callocate_2D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_2D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_2D_FourByteInt


subroutine callocate_2D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_2D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_2D_EightByteInt


subroutine callocate_2D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_2D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_2D_FourByteReal


subroutine callocate_2D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_2D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :), &
                         pointer    :: array
  integer, dimension(2), intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_2D_EightByteReal



!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

subroutine callocate_3D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_3D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_3D_OneByteInt


subroutine callocate_3D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_3D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_3D_TwoByteInt


subroutine callocate_3D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_3D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_3D_FourByteInt


subroutine callocate_3D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_3D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_3D_EightByteInt


subroutine callocate_3D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_3D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_3D_FourByteReal


subroutine callocate_3D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_3D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :), &
                         pointer    :: array
  integer, dimension(3), intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_3D_EightByteReal



!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

subroutine callocate_4D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_4D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_4D_OneByteInt


subroutine callocate_4D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_4D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_4D_TwoByteInt


subroutine callocate_4D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_4D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_4D_FourByteInt


subroutine callocate_4D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_4D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_4D_EightByteInt


subroutine callocate_4D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_4D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_4D_FourByteReal


subroutine callocate_4D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_4D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :), &
                         pointer    :: array
  integer, dimension(4), intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_4D_EightByteReal



!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

subroutine callocate_5D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_5D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_5D_OneByteInt


subroutine callocate_5D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_5D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_5D_TwoByteInt


subroutine callocate_5D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_5D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_5D_FourByteInt


subroutine callocate_5D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_5D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_5D_EightByteInt


subroutine callocate_5D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_5D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_5D_FourByteReal


subroutine callocate_5D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_5D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                         pointer    :: array
  integer, dimension(5), intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_5D_EightByteReal



!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

subroutine callocate_6D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_6D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_6D_OneByteInt


subroutine callocate_6D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_6D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_6D_TwoByteInt


subroutine callocate_6D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_6D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_6D_FourByteInt


subroutine callocate_6D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_6D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_6D_EightByteInt


subroutine callocate_6D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_6D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_6D_FourByteReal


subroutine callocate_6D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_6D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(6), intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_6D_EightByteReal



!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

subroutine callocate_7D_OneByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_7D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts

  integer(kind = OneByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_7D_OneByteInt


subroutine callocate_7D_TwoByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_7D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts

  integer(kind = TwoByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_7D_TwoByteInt


subroutine callocate_7D_FourByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_7D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts

  integer(kind = FourByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_7D_FourByteInt


subroutine callocate_7D_EightByteInt (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_7D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts

  integer(kind = EightByteInt),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_7D_EightByteInt


subroutine callocate_7D_FourByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_7D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts

  real(kind = FourByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_7D_FourByteReal


subroutine callocate_7D_EightByteReal (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => callocate_7D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                         pointer    :: array
  integer, dimension(7), intent(in) :: cnts

  real(kind = EightByteReal),                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine callocate_7D_EightByteReal


