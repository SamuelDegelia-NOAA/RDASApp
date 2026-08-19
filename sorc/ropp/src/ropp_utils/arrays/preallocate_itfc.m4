dnl $Id :$
dnl
dnl This file is used for the automatic generation of interfaces for
dnl preallocate.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
    function PREALLOCATE (array, cnts) result (newarray)
      use typeSizes
      TYPE, dimension(COLONS), &
                             pointer    :: array
      integer, DIM_CNTS intent(in) :: cnts
      OTYPE, dimension(COLONS), &
                             pointer    :: newarray
    end function PREALLOCATE dnl
dnl
