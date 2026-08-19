dnl $Id$
dnl
dnl This file is used for the automatic generation of the file
dnl ut_convert.f90.
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
subroutine UT_AFUN (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  TYPE, dimension(COLONS), &
                             INTENT_OR_POINTER :: from
  TYPE, dimension(COLONS), &
                             OUTENT_OR_POINTER :: values

  real(kind = EightByteReal)             :: slope, intercept

ifelse(POINTER, `yes',dnl
  integer                                :: cnts(NUMDIMS`')
)dnl
! Get conversion factors
! ----------------------

  if ( trim(from_unit) == " " .or. trim(from_unit) == "1" .or. &
       trim(to_unit) == " " .or. trim(to_unit) == "1" .or. &
       trim(from_unit) == trim(to_unit) ) then
    slope     = 1.0
    intercept = 0.0
    to_unit = from_unit
  else
    call ropp_unit_conversion(from_unit, to_unit, slope, intercept)
  endif
dnl
dnl Note: The following stuff is an m4 contruct!
dnl
ifelse(POINTER, `yes',dnl
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(ALLOC_ARGS`'))

)dnl
! Convert units
! -------------

  values = slope * from + intercept

end subroutine UT_AFUN
