dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl callocate.f90.
dnl
dnl C. Marquardt   <christian@marquardt.fsnet.co.uk>
dnl
dnl
subroutine CALLOCATE (array, cnts, value)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => CALLOCATE

  implicit none

  TYPE, dimension(COLONS), &
                         pointer    :: array
  integer, DIM_CNTS intent(in) :: cnts

  TYPE,                  optional   :: value

! Allocation
! ----------

  if (associated(array)) deallocate(array)
  allocate(array(ALLOC_ARGS))

! Presetting values
! -----------------

  if (present(value)) then
     array = value
  else
     array = 0
  endif

end subroutine CALLOCATE
