dnl $Id :$
dnl
dnl This file is used for the automatic generation of interfaces for
dnl reallocate.f90.
dnl
dnl C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
dnl
dnl
    subroutine REALLOCATE (array, cnts)
      use typeSizes
      TYPE, dimension(COLONS), &
                             pointer    :: array
      integer, DIM_CNTS intent(in) :: cnts
    end subroutine REALLOCATE dnl
dnl
