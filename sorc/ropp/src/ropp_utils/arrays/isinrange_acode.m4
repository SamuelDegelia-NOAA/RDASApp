dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl isinrange.f90.
dnl
dnl C. Marquardt   <christian@marquardt.fsnet.co.uk>
dnl
dnl
function ISINRANGE (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => ISINRANGE

  implicit none

  TYPE, dimension(COLONS), &
                         intent(in) :: array
  TYPE, dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function ISINRANGE
