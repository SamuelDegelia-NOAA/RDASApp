dnl $Id: unitconvert.m4 2237 2009-09-14 15:03:55Z frhl $
dnl
dnl
dnl Process this file with m4 to produce a working unitconvert.f90 file.
dnl
dnl AUTHOR
dnl    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
dnl
dnl COPYRIGHT
dnl
dnl    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
dnl
dnl    All rights reserved.
dnl
dnl    Permission is hereby granted, free of charge, to any person obtaining
dnl    a copy of this software and associated documentation files (the
dnl    "Software"), to deal in the Software without restriction, including
dnl    without limitation the rights to use, copy, modify, merge, publish,
dnl    distribute, sublicense, and/or sell copies of the Software, and to
dnl    permit persons to whom the Software is furnished to do so, subject to
dnl    the following conditions:
dnl
dnl    The above copyright notice and this permission notice shall be
dnl    included in all copies or substantial portions of the Software as well
dnl    as in supporting documentation.
dnl
dnl    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
dnl    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
dnl    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
dnl    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
dnl    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
dnl    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
dnl    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
dnl
dnl --------------------------------------------------------------------
dnl 1. m4 macro definitions
dnl --------------------------------------------------------------------
dnl
define(`CVSID',`! $'`Id: $')dnl
dnl
define(UT_CONVERT_SCA_ITFC, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4
define(`KINDVALUE',$1)dnl
define(`PUTORGET', `put')dnl
include(unitconvert_aux.m4)dnl
include(ut_convert_sitfc.m4)
')dnl
dnl
define(UT_CONVERT_ARR_ITFC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', `put')dnl
include(unitconvert_aux.m4)dnl
include(ut_convert_aitfc.m4)
')dnl
dnl
define(UT_CONVERT_ARR_ALLOC_ITFC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', `put')dnl
define(`POINTER', `yes')dnl
include(unitconvert_aux.m4)dnl
include(ut_convert_aitfc.m4)
')dnl
CVSID

!****m* Modules/unitconvert
!
! NAME
!    unitconvert - A simple interface to the unit conversion library.
!                 --> BASED ON MARQUARDT COLLECTION LIBRARY ROUTINE
!                     FOR INTERFACE TO THIRD-PARTY UDUNITS LIBRARY
!
! SYNOPSIS
!    use unitconvert
!
! DESCRIPTION
!    This module provides a simple interface to a unit conversion library and
!    allows straightford conversion between physical units.
!
! SEE ALSO
!    ut_convert
!    ropp_unit_conversion
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
!
! COPYRIGHT
!
!    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
!
!    All rights reserved.
!
!    Permission is hereby granted, free of charge, to any person obtaining
!    a copy of this software and associated documentation files (the
!    "Software"), to deal in the Software without restriction, including
!    without limitation the rights to use, copy, modify, merge, publish,
!    distribute, sublicense, and/or sell copies of the Software, and to
!    permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:
!
!    The above copyright notice and this permission notice shall be
!    included in all copies or substantial portions of the Software as well
!    as in supporting documentation.
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
!    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
!    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
!    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!
!****

module unitconvert

!-----------------------------------------------------------------------
! 1. Some parameters
!-----------------------------------------------------------------------

  integer, parameter :: UT_EOF      =   1
  integer, parameter :: UT_ENOFILE  =  -1
  integer, parameter :: UT_ESYNTAX  =  -2
  integer, parameter :: UT_EUNKNOWN =  -3
  integer, parameter :: UT_EIO      =  -4
  integer, parameter :: UT_EINVALID =  -5
  integer, parameter :: UT_ENOINIT  =  -6
  integer, parameter :: UT_ECONVERT =  -7
  integer, parameter :: UT_EALLOC   =  -8
  integer, parameter :: UT_ENOROOM  =  -9
  integer, parameter :: UT_ENOTTIME = -10

  integer, parameter :: UT_MAXNUM_BASE_QUANTITIES = 10

!-----------------------------------------------------------------------
! 2. Interfaces
!-----------------------------------------------------------------------
 
  interface 	
     SUBROUTINE ropp_unit_conversion ( from_unit, to_unit, slope, intercept )
	USE typesizes, ONLY: wp => EightByteReal
        CHARACTER(len = *), INTENT(in)  :: from_unit
        CHARACTER(len = *), INTENT(in)  :: to_unit
        REAL(wp),           INTENT(out) :: slope
        REAL(wp),           INTENT(out) :: intercept
     END SUBROUTINE ropp_unit_conversion
   end interface

  interface ut_convert
UT_CONVERT_SCA_ITFC(OneByteInt)
UT_CONVERT_SCA_ITFC(TwoByteInt)
UT_CONVERT_SCA_ITFC(FourByteInt)
UT_CONVERT_SCA_ITFC(EightByteInt)
UT_CONVERT_SCA_ITFC(FourByteReal)
UT_CONVERT_SCA_ITFC(EightByteReal)

UT_CONVERT_ARR_ITFC(1, OneByteInt)
UT_CONVERT_ARR_ITFC(2, OneByteInt)
UT_CONVERT_ARR_ITFC(3, OneByteInt)
UT_CONVERT_ARR_ITFC(4, OneByteInt)
UT_CONVERT_ARR_ITFC(5, OneByteInt)
UT_CONVERT_ARR_ITFC(6, OneByteInt)
UT_CONVERT_ARR_ITFC(7, OneByteInt)

UT_CONVERT_ARR_ITFC(1, TwoByteInt)
UT_CONVERT_ARR_ITFC(2, TwoByteInt)
UT_CONVERT_ARR_ITFC(3, TwoByteInt)
UT_CONVERT_ARR_ITFC(4, TwoByteInt)
UT_CONVERT_ARR_ITFC(5, TwoByteInt)
UT_CONVERT_ARR_ITFC(6, TwoByteInt)
UT_CONVERT_ARR_ITFC(7, TwoByteInt)

UT_CONVERT_ARR_ITFC(1, FourByteInt)
UT_CONVERT_ARR_ITFC(2, FourByteInt)
UT_CONVERT_ARR_ITFC(3, FourByteInt)
UT_CONVERT_ARR_ITFC(4, FourByteInt)
UT_CONVERT_ARR_ITFC(5, FourByteInt)
UT_CONVERT_ARR_ITFC(6, FourByteInt)
UT_CONVERT_ARR_ITFC(7, FourByteInt)

UT_CONVERT_ARR_ITFC(1, EightByteInt)
UT_CONVERT_ARR_ITFC(2, EightByteInt)
UT_CONVERT_ARR_ITFC(3, EightByteInt)
UT_CONVERT_ARR_ITFC(4, EightByteInt)
UT_CONVERT_ARR_ITFC(5, EightByteInt)
UT_CONVERT_ARR_ITFC(6, EightByteInt)
UT_CONVERT_ARR_ITFC(7, EightByteInt)

UT_CONVERT_ARR_ITFC(1, FourByteReal)
UT_CONVERT_ARR_ITFC(2, FourByteReal)
UT_CONVERT_ARR_ITFC(3, FourByteReal)
UT_CONVERT_ARR_ITFC(4, FourByteReal)
UT_CONVERT_ARR_ITFC(5, FourByteReal)
UT_CONVERT_ARR_ITFC(6, FourByteReal)
UT_CONVERT_ARR_ITFC(7, FourByteReal)

UT_CONVERT_ARR_ITFC(1, EightByteReal)
UT_CONVERT_ARR_ITFC(2, EightByteReal)
UT_CONVERT_ARR_ITFC(3, EightByteReal)
UT_CONVERT_ARR_ITFC(4, EightByteReal)
UT_CONVERT_ARR_ITFC(5, EightByteReal)
UT_CONVERT_ARR_ITFC(6, EightByteReal)
UT_CONVERT_ARR_ITFC(7, EightByteReal)

dnl UT_CONVERT_ARR_ALLOC_ITFC(1, OneByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(2, OneByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(3, OneByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(4, OneByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(5, OneByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(6, OneByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(7, OneByteInt)

dnl UT_CONVERT_ARR_ALLOC_ITFC(1, TwoByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(2, TwoByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(3, TwoByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(4, TwoByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(5, TwoByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(6, TwoByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(7, TwoByteInt)

dnl UT_CONVERT_ARR_ALLOC_ITFC(1, FourByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(2, FourByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(3, FourByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(4, FourByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(5, FourByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(6, FourByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(7, FourByteInt)

dnl UT_CONVERT_ARR_ALLOC_ITFC(1, EightByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(2, EightByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(3, EightByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(4, EightByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(5, EightByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(6, EightByteInt)
dnl UT_CONVERT_ARR_ALLOC_ITFC(7, EightByteInt)

dnl UT_CONVERT_ARR_ALLOC_ITFC(1, FourByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(2, FourByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(3, FourByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(4, FourByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(5, FourByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(6, FourByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(7, FourByteReal)

dnl UT_CONVERT_ARR_ALLOC_ITFC(1, EightByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(2, EightByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(3, EightByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(4, EightByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(5, EightByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(6, EightByteReal)
dnl UT_CONVERT_ARR_ALLOC_ITFC(7, EightByteReal)
  end interface

end module unitconvert
