dnl $Id :$
dnl
dnl This file is used for the automatic generation of interfaces for
dnl reverse.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
    function REVERSE (array, dim) result (reversed)
      use typeSizes
      TYPE, dimension(COLONS), &
                           intent(in) :: array
      integer,             optional   :: dim
      TYPE, dimension(SIZES) &
                                      :: reversed
    end function REVERSE
dnl