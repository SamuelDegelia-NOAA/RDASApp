dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl reverse.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
function REVERSE (array, dim) result (reversed)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => REVERSE

  implicit none

  TYPE, dimension(COLONS), &
                          intent(in) :: array
  integer,                optional   :: dim
  TYPE, dimension(SIZES) &
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
        reversed = array(IDX_ARGS(`1'))
     case(2)
        reversed = array(IDX_ARGS(`2'))
     case(3)
        reversed = array(IDX_ARGS(`3'))
     case(4)
        reversed = array(IDX_ARGS(`4'))
     case(5)
        reversed = array(IDX_ARGS(`5'))
     case(6)
        reversed = array(IDX_ARGS(`6'))
     case(7)
        reversed = array(IDX_ARGS(`7'))
  end select

  deallocate(idx)

end function REVERSE
