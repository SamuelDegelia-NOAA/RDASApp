dnl $Id$
dnl
dnl
dnl Process this file with m4 to produce a working ut_convert.f90 file.
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
define(UTCONVERT_SCA, dnl
`define(`NUMDIMS',0)dnl # not used, but permits sharing ncdf_aux.m4
define(`KINDVALUE',$1)dnl
define(`PUTORGET', `put')dnl
include(unitconvert_aux.m4)dnl
include(ut_convert_scode.m4)
')dnl
dnl
define(UTCONVERT_ARR, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', `put')dnl
include(unitconvert_aux.m4)dnl
include(ut_convert_acode.m4)
')dnl
dnl
define(UTCONVERT_ARR_ALLOC, dnl
`define(`NUMDIMS',$1)dnl
define(`KINDVALUE',$2)dnl
define(`PUTORGET', `put')dnl
define(`POINTER', `yes')dnl
include(unitconvert_aux.m4)dnl
include(ut_convert_acode.m4)
')dnl
CVSID

!****s* Units/Conversion
!
! DESCRIPTION
!    The interface to a ROPP-specific unit conversion routine
!    is contained in a single subroutine, ut_convert(), and has been coded
!    with simplicity in mind, not efficiency.
!
! SEE ALSO
!    ut_convert
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

!****s* Conversion/ut_convert
!
! NAME
!    ut_convert - Convert between physical units.
!
! SYNOPSIS
!    use unitconvert
!      ...
!    call ut_convert(data, from_unit, converted, to_unit)
!
! DESCRIPTION
!    This module provides a simple interface to the udunits library and
!    allows straightford conversion between physical units. If either
!    from_unit or to_unit is blank or '1' (indicating no or dimensionless
!    quantities) or are the same, output values are merely copied from
!    the inputs.
!
! INPUTS
!    data       Integer, real or double precision scalar or array.
!    from_unit  Udunits conformant unit specification of the data to be
!                  converted.
!    to_unit    Udunits conformant unit specification of the target unit.
!
! OUTPUT
!    converted  Converted data; must have same type and shape as data.
!
! EXAMPLE
!    To convert a length from mm to km:
!
!       call ut_convert(length, 'mm', length, 'km')
!
! AUTHOR
!    C. Marquardt, Darmstadt, Germany             <christian@marquardt.sc>
!
!****

!-----------------------------------------------------------------------
! 1. Convert scalars
!-----------------------------------------------------------------------

UTCONVERT_SCA(OneByteInt)
UTCONVERT_SCA(TwoByteInt)
UTCONVERT_SCA(FourByteInt)
UTCONVERT_SCA(EightByteInt)
UTCONVERT_SCA(FourByteReal)
UTCONVERT_SCA(EightByteReal)

!-----------------------------------------------------------------------
! 2. Convert arrays
!-----------------------------------------------------------------------

UTCONVERT_ARR(1, OneByteInt)
UTCONVERT_ARR(2, OneByteInt)
UTCONVERT_ARR(3, OneByteInt)
UTCONVERT_ARR(4, OneByteInt)
UTCONVERT_ARR(5, OneByteInt)
UTCONVERT_ARR(6, OneByteInt)
UTCONVERT_ARR(7, OneByteInt)

UTCONVERT_ARR(1, TwoByteInt)
UTCONVERT_ARR(2, TwoByteInt)
UTCONVERT_ARR(3, TwoByteInt)
UTCONVERT_ARR(4, TwoByteInt)
UTCONVERT_ARR(5, TwoByteInt)
UTCONVERT_ARR(6, TwoByteInt)
UTCONVERT_ARR(7, TwoByteInt)

UTCONVERT_ARR(1, FourByteInt)
UTCONVERT_ARR(2, FourByteInt)
UTCONVERT_ARR(3, FourByteInt)
UTCONVERT_ARR(4, FourByteInt)
UTCONVERT_ARR(5, FourByteInt)
UTCONVERT_ARR(6, FourByteInt)
UTCONVERT_ARR(7, FourByteInt)

UTCONVERT_ARR(1, EightByteInt)
UTCONVERT_ARR(2, EightByteInt)
UTCONVERT_ARR(3, EightByteInt)
UTCONVERT_ARR(4, EightByteInt)
UTCONVERT_ARR(5, EightByteInt)
UTCONVERT_ARR(6, EightByteInt)
UTCONVERT_ARR(7, EightByteInt)

UTCONVERT_ARR(1, FourByteReal)
UTCONVERT_ARR(2, FourByteReal)
UTCONVERT_ARR(3, FourByteReal)
UTCONVERT_ARR(4, FourByteReal)
UTCONVERT_ARR(5, FourByteReal)
UTCONVERT_ARR(6, FourByteReal)
UTCONVERT_ARR(7, FourByteReal)

UTCONVERT_ARR(1, EightByteReal)
UTCONVERT_ARR(2, EightByteReal)
UTCONVERT_ARR(3, EightByteReal)
UTCONVERT_ARR(4, EightByteReal)
UTCONVERT_ARR(5, EightByteReal)
UTCONVERT_ARR(6, EightByteReal)
UTCONVERT_ARR(7, EightByteReal)

!-----------------------------------------------------------------------
! 3. Convert pointers to arrays
!-----------------------------------------------------------------------

UTCONVERT_ARR_ALLOC(1, OneByteInt)
UTCONVERT_ARR_ALLOC(2, OneByteInt)
UTCONVERT_ARR_ALLOC(3, OneByteInt)
UTCONVERT_ARR_ALLOC(4, OneByteInt)
UTCONVERT_ARR_ALLOC(5, OneByteInt)
UTCONVERT_ARR_ALLOC(6, OneByteInt)
UTCONVERT_ARR_ALLOC(7, OneByteInt)

UTCONVERT_ARR_ALLOC(1, TwoByteInt)
UTCONVERT_ARR_ALLOC(2, TwoByteInt)
UTCONVERT_ARR_ALLOC(3, TwoByteInt)
UTCONVERT_ARR_ALLOC(4, TwoByteInt)
UTCONVERT_ARR_ALLOC(5, TwoByteInt)
UTCONVERT_ARR_ALLOC(6, TwoByteInt)
UTCONVERT_ARR_ALLOC(7, TwoByteInt)

UTCONVERT_ARR_ALLOC(1, FourByteInt)
UTCONVERT_ARR_ALLOC(2, FourByteInt)
UTCONVERT_ARR_ALLOC(3, FourByteInt)
UTCONVERT_ARR_ALLOC(4, FourByteInt)
UTCONVERT_ARR_ALLOC(5, FourByteInt)
UTCONVERT_ARR_ALLOC(6, FourByteInt)
UTCONVERT_ARR_ALLOC(7, FourByteInt)

UTCONVERT_ARR_ALLOC(1, EightByteInt)
UTCONVERT_ARR_ALLOC(2, EightByteInt)
UTCONVERT_ARR_ALLOC(3, EightByteInt)
UTCONVERT_ARR_ALLOC(4, EightByteInt)
UTCONVERT_ARR_ALLOC(5, EightByteInt)
UTCONVERT_ARR_ALLOC(6, EightByteInt)
UTCONVERT_ARR_ALLOC(7, EightByteInt)

UTCONVERT_ARR_ALLOC(1, FourByteReal)
UTCONVERT_ARR_ALLOC(2, FourByteReal)
UTCONVERT_ARR_ALLOC(3, FourByteReal)
UTCONVERT_ARR_ALLOC(4, FourByteReal)
UTCONVERT_ARR_ALLOC(5, FourByteReal)
UTCONVERT_ARR_ALLOC(6, FourByteReal)
UTCONVERT_ARR_ALLOC(7, FourByteReal)

UTCONVERT_ARR_ALLOC(1, EightByteReal)
UTCONVERT_ARR_ALLOC(2, EightByteReal)
UTCONVERT_ARR_ALLOC(3, EightByteReal)
UTCONVERT_ARR_ALLOC(4, EightByteReal)
UTCONVERT_ARR_ALLOC(5, EightByteReal)
UTCONVERT_ARR_ALLOC(6, EightByteReal)
UTCONVERT_ARR_ALLOC(7, EightByteReal)
