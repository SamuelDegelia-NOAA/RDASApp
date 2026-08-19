dnl $Id :$
dnl
dnl This file is used for the automatic generation of interfaces for
dnl copy_alloc.f90.
dnl
dnl C. Marquardt   <christian.marquardt@metoffice.com>
dnl
dnl
    subroutine COPY_ALLOC (array, newarray)
      use typeSizes
      TYPE, dimension(COLONS), &
                             intent(in) :: array
      TYPE, dimension(COLONS), &
                             pointer    :: newarray
    end subroutine COPY_ALLOC dnl
dnl
