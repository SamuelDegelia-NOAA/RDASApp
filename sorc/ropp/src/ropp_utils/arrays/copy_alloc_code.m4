dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl copy_alloc.f90.
dnl
dnl C. Marquardt   <christian.marquardt@metoffice.com>
dnl
dnl
subroutine COPY_ALLOC (array, newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => COPY_ALLOC

  implicit none

  TYPE, dimension(COLONS), &
                         intent(in) :: array
  TYPE, dimension(COLONS), &
                         pointer    :: newarray

! Clear newarray
! --------------

  if (associated(newarray)) deallocate(newarray)

! Allocate newarray
! -----------------

  allocate(newarray(SIZES))

! Copy data
! ---------

  newarray(COLONS) = array(COLONS)

end subroutine COPY_ALLOC
