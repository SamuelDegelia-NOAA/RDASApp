dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl preallocate.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
function PREALLOCATE (array, cnts) result (newarray)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => PREALLOCATE

  implicit none

  TYPE, dimension(COLONS), &
                         pointer    :: array
  integer, DIM_CNTS intent(in) :: cnts
  OTYPE, dimension(COLONS), &
                         pointer    :: newarray

! Reallocation
! ------------

  allocate(newarray(ALLOC_ARGS))

  newarray(COPY_ARGS) &
   = array(COPY_ARGS)

  deallocate(array)

end function PREALLOCATE
