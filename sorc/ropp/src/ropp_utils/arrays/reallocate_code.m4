dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl reallocate.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
subroutine REALLOCATE (array, cnts)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => REALLOCATE

  implicit none

  TYPE, dimension(COLONS), &
                         pointer    :: array
  integer, DIM_CNTS intent(in) :: cnts

  OTYPE, dimension(COLONS), &
                         pointer    :: newarray

  nullify(newarray)

! Reallocation
! ------------

  allocate(newarray(ALLOC_ARGS))

  newarray(COPY_ARGS) &
   = array(COPY_ARGS)

  deallocate(array)

  array => newarray

end subroutine REALLOCATE
