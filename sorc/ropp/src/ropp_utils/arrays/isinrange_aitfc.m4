dnl $Id :$
dnl
dnl This file is used for the automatic generation of interfaces for
dnl isinrange.f90.
dnl
dnl C. Marquardt, West Hill, UK   <christian@marquardt.fsnet.co.uk>
dnl
dnl
    function ISINRANGE (array, range) result (inrange)
      use typeSizes
      TYPE, dimension(COLONS), &
                             intent(in) :: array
      TYPE, dimension(2),      &
                             intent(in) :: range
      logical                           :: inrange
    end function ISINRANGE dnl
dnl
