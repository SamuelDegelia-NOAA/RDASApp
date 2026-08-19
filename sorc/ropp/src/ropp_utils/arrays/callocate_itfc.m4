dnl $Id :$
dnl
dnl This file is used for the automatic generation of interfaces for
dnl callocate.f90.
dnl
dnl C. Marquardt, West Hill, UK   <christian@marquardt.fsnet.co.uk>
dnl
dnl
    subroutine CALLOCATE (array, cnts, value)
      use typeSizes
      TYPE, dimension(COLONS), &
                             pointer    :: array
      integer, DIM_CNTS intent(in) :: cnts
      TYPE,                  optional   :: value
    end subroutine CALLOCATE dnl
dnl
