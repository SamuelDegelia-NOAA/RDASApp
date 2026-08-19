! $Id: $

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

function reverse_1D_OneByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_1D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = OneByteInt), dimension(size(array, 1)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1)
     case(2)
        reversed = array(:)
     case(3)
        reversed = array(:)
     case(4)
        reversed = array(:)
     case(5)
        reversed = array(:)
     case(6)
        reversed = array(:)
     case(7)
        reversed = array(:)
  end select

  deallocate(idx)

end function reverse_1D_OneByteInt


function reverse_1D_TwoByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_1D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = TwoByteInt), dimension(size(array, 1)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1)
     case(2)
        reversed = array(:)
     case(3)
        reversed = array(:)
     case(4)
        reversed = array(:)
     case(5)
        reversed = array(:)
     case(6)
        reversed = array(:)
     case(7)
        reversed = array(:)
  end select

  deallocate(idx)

end function reverse_1D_TwoByteInt


function reverse_1D_FourByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_1D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = FourByteInt), dimension(size(array, 1)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1)
     case(2)
        reversed = array(:)
     case(3)
        reversed = array(:)
     case(4)
        reversed = array(:)
     case(5)
        reversed = array(:)
     case(6)
        reversed = array(:)
     case(7)
        reversed = array(:)
  end select

  deallocate(idx)

end function reverse_1D_FourByteInt


function reverse_1D_EightByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_1D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = EightByteInt), dimension(size(array, 1)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1)
     case(2)
        reversed = array(:)
     case(3)
        reversed = array(:)
     case(4)
        reversed = array(:)
     case(5)
        reversed = array(:)
     case(6)
        reversed = array(:)
     case(7)
        reversed = array(:)
  end select

  deallocate(idx)

end function reverse_1D_EightByteInt


function reverse_1D_FourByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_1D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = FourByteReal), dimension(size(array, 1)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1)
     case(2)
        reversed = array(:)
     case(3)
        reversed = array(:)
     case(4)
        reversed = array(:)
     case(5)
        reversed = array(:)
     case(6)
        reversed = array(:)
     case(7)
        reversed = array(:)
  end select

  deallocate(idx)

end function reverse_1D_FourByteReal


function reverse_1D_EightByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_1D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = EightByteReal), dimension(size(array, 1)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1)
     case(2)
        reversed = array(:)
     case(3)
        reversed = array(:)
     case(4)
        reversed = array(:)
     case(5)
        reversed = array(:)
     case(6)
        reversed = array(:)
     case(7)
        reversed = array(:)
  end select

  deallocate(idx)

end function reverse_1D_EightByteReal



!--------------------------------------------------------------------------
! 3. 2D array arguments
!--------------------------------------------------------------------------

function reverse_2D_OneByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_2D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = OneByteInt), dimension(size(array, 1), size(array, 2)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :)
     case(2)
        reversed = array(:, size(array,2):1:-1)
     case(3)
        reversed = array(:, :)
     case(4)
        reversed = array(:, :)
     case(5)
        reversed = array(:, :)
     case(6)
        reversed = array(:, :)
     case(7)
        reversed = array(:, :)
  end select

  deallocate(idx)

end function reverse_2D_OneByteInt


function reverse_2D_TwoByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_2D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = TwoByteInt), dimension(size(array, 1), size(array, 2)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :)
     case(2)
        reversed = array(:, size(array,2):1:-1)
     case(3)
        reversed = array(:, :)
     case(4)
        reversed = array(:, :)
     case(5)
        reversed = array(:, :)
     case(6)
        reversed = array(:, :)
     case(7)
        reversed = array(:, :)
  end select

  deallocate(idx)

end function reverse_2D_TwoByteInt


function reverse_2D_FourByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_2D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = FourByteInt), dimension(size(array, 1), size(array, 2)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :)
     case(2)
        reversed = array(:, size(array,2):1:-1)
     case(3)
        reversed = array(:, :)
     case(4)
        reversed = array(:, :)
     case(5)
        reversed = array(:, :)
     case(6)
        reversed = array(:, :)
     case(7)
        reversed = array(:, :)
  end select

  deallocate(idx)

end function reverse_2D_FourByteInt


function reverse_2D_EightByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_2D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = EightByteInt), dimension(size(array, 1), size(array, 2)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :)
     case(2)
        reversed = array(:, size(array,2):1:-1)
     case(3)
        reversed = array(:, :)
     case(4)
        reversed = array(:, :)
     case(5)
        reversed = array(:, :)
     case(6)
        reversed = array(:, :)
     case(7)
        reversed = array(:, :)
  end select

  deallocate(idx)

end function reverse_2D_EightByteInt


function reverse_2D_FourByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_2D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = FourByteReal), dimension(size(array, 1), size(array, 2)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :)
     case(2)
        reversed = array(:, size(array,2):1:-1)
     case(3)
        reversed = array(:, :)
     case(4)
        reversed = array(:, :)
     case(5)
        reversed = array(:, :)
     case(6)
        reversed = array(:, :)
     case(7)
        reversed = array(:, :)
  end select

  deallocate(idx)

end function reverse_2D_FourByteReal


function reverse_2D_EightByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_2D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = EightByteReal), dimension(size(array, 1), size(array, 2)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :)
     case(2)
        reversed = array(:, size(array,2):1:-1)
     case(3)
        reversed = array(:, :)
     case(4)
        reversed = array(:, :)
     case(5)
        reversed = array(:, :)
     case(6)
        reversed = array(:, :)
     case(7)
        reversed = array(:, :)
  end select

  deallocate(idx)

end function reverse_2D_EightByteReal



!--------------------------------------------------------------------------
! 4. 3D array arguments
!--------------------------------------------------------------------------

function reverse_3D_OneByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_3D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = OneByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1)
     case(4)
        reversed = array(:, :, :)
     case(5)
        reversed = array(:, :, :)
     case(6)
        reversed = array(:, :, :)
     case(7)
        reversed = array(:, :, :)
  end select

  deallocate(idx)

end function reverse_3D_OneByteInt


function reverse_3D_TwoByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_3D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = TwoByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1)
     case(4)
        reversed = array(:, :, :)
     case(5)
        reversed = array(:, :, :)
     case(6)
        reversed = array(:, :, :)
     case(7)
        reversed = array(:, :, :)
  end select

  deallocate(idx)

end function reverse_3D_TwoByteInt


function reverse_3D_FourByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_3D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = FourByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1)
     case(4)
        reversed = array(:, :, :)
     case(5)
        reversed = array(:, :, :)
     case(6)
        reversed = array(:, :, :)
     case(7)
        reversed = array(:, :, :)
  end select

  deallocate(idx)

end function reverse_3D_FourByteInt


function reverse_3D_EightByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_3D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = EightByteInt), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1)
     case(4)
        reversed = array(:, :, :)
     case(5)
        reversed = array(:, :, :)
     case(6)
        reversed = array(:, :, :)
     case(7)
        reversed = array(:, :, :)
  end select

  deallocate(idx)

end function reverse_3D_EightByteInt


function reverse_3D_FourByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_3D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = FourByteReal), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1)
     case(4)
        reversed = array(:, :, :)
     case(5)
        reversed = array(:, :, :)
     case(6)
        reversed = array(:, :, :)
     case(7)
        reversed = array(:, :, :)
  end select

  deallocate(idx)

end function reverse_3D_FourByteReal


function reverse_3D_EightByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_3D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = EightByteReal), dimension(size(array, 1), size(array, 2), size(array, 3)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1)
     case(4)
        reversed = array(:, :, :)
     case(5)
        reversed = array(:, :, :)
     case(6)
        reversed = array(:, :, :)
     case(7)
        reversed = array(:, :, :)
  end select

  deallocate(idx)

end function reverse_3D_EightByteReal



!--------------------------------------------------------------------------
! 5. 4D array arguments
!--------------------------------------------------------------------------

function reverse_4D_OneByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_4D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = OneByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1, :)
     case(4)
        reversed = array(:, :, :, size(array,4):1:-1)
     case(5)
        reversed = array(:, :, :, :)
     case(6)
        reversed = array(:, :, :, :)
     case(7)
        reversed = array(:, :, :, :)
  end select

  deallocate(idx)

end function reverse_4D_OneByteInt


function reverse_4D_TwoByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_4D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = TwoByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1, :)
     case(4)
        reversed = array(:, :, :, size(array,4):1:-1)
     case(5)
        reversed = array(:, :, :, :)
     case(6)
        reversed = array(:, :, :, :)
     case(7)
        reversed = array(:, :, :, :)
  end select

  deallocate(idx)

end function reverse_4D_TwoByteInt


function reverse_4D_FourByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_4D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = FourByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1, :)
     case(4)
        reversed = array(:, :, :, size(array,4):1:-1)
     case(5)
        reversed = array(:, :, :, :)
     case(6)
        reversed = array(:, :, :, :)
     case(7)
        reversed = array(:, :, :, :)
  end select

  deallocate(idx)

end function reverse_4D_FourByteInt


function reverse_4D_EightByteInt (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_4D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  integer(kind = EightByteInt), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1, :)
     case(4)
        reversed = array(:, :, :, size(array,4):1:-1)
     case(5)
        reversed = array(:, :, :, :)
     case(6)
        reversed = array(:, :, :, :)
     case(7)
        reversed = array(:, :, :, :)
  end select

  deallocate(idx)

end function reverse_4D_EightByteInt


function reverse_4D_FourByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_4D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = FourByteReal), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1, :)
     case(4)
        reversed = array(:, :, :, size(array,4):1:-1)
     case(5)
        reversed = array(:, :, :, :)
     case(6)
        reversed = array(:, :, :, :)
     case(7)
        reversed = array(:, :, :, :)
  end select

  deallocate(idx)

end function reverse_4D_FourByteReal


function reverse_4D_EightByteReal (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => reverse_4D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :), &
                          intent(in) :: array
  integer,                optional   :: dim
  real(kind = EightByteReal), dimension(size(array, 1), size(array, 2), size(array, 3), size(array, 4)) &
                                     :: reversed

  integer                            :: i, idim
  integer, dimension(:), allocatable :: idx

! Check arguments
! ---------------

  if (present(dim)) then
     if (dim > size(shape(array))) then
        print *, 'reverse: dim > #dims.'
        call exit(-1)
     endif
     idim = dim
  else
     idim = 1
  endif

! Revert the array
! ----------------

  allocate(idx(size(array,idim)))

  idx = (/ (i, i = size(array,idim), 1, -1) /)

  select case(idim)
     case(1)
        reversed = array(size(array,1):1:-1, :, :, :)
     case(2)
        reversed = array(:, size(array,2):1:-1, :, :)
     case(3)
        reversed = array(:, :, size(array,3):1:-1, :)
     case(4)
        reversed = array(:, :, :, size(array,4):1:-1)
     case(5)
        reversed = array(:, :, :, :)
     case(6)
        reversed = array(:, :, :, :)
     case(7)
        reversed = array(:, :, :, :)
  end select

  deallocate(idx)

end function reverse_4D_EightByteReal



!--------------------------------------------------------------------------
! 6. 5D array arguments
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
! 7. 6D array arguments
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
! 8. 7D array arguments
!--------------------------------------------------------------------------

