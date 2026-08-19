dnl $Id :$
dnl
dnl This file is used for the automatic generation of the file
dnl isinrange.f90.
dnl
dnl C. Marquardt   <christian@marquardt.fsnet.co.uk>
dnl
dnl
function ISINRANGE (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => ISINRANGE

  implicit none

  TYPE, &
                         intent(in) :: value
  TYPE, dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function ISINRANGE
