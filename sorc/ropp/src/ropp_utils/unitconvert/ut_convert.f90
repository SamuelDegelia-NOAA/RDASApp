! $Id: $

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

subroutine ut_converts_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), &
                             intent( in) :: from
  integer(kind = OneByteInt), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converts_OneByteInt


subroutine ut_converts_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), &
                             intent( in) :: from
  integer(kind = TwoByteInt), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converts_TwoByteInt


subroutine ut_converts_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), &
                             intent( in) :: from
  integer(kind = FourByteInt), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converts_FourByteInt


subroutine ut_converts_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), &
                             intent( in) :: from
  integer(kind = EightByteInt), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converts_EightByteInt


subroutine ut_converts_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), &
                             intent( in) :: from
  real(kind = FourByteReal), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converts_FourByteReal


subroutine ut_converts_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), &
                             intent( in) :: from
  real(kind = EightByteReal), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converts_EightByteReal



!-----------------------------------------------------------------------
! 2. Convert arrays
!-----------------------------------------------------------------------

subroutine ut_converta_1D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_1D_OneByteInt


subroutine ut_converta_2D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_2D_OneByteInt


subroutine ut_converta_3D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_3D_OneByteInt


subroutine ut_converta_4D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_4D_OneByteInt


subroutine ut_converta_5D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_5D_OneByteInt


subroutine ut_converta_6D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_6D_OneByteInt


subroutine ut_converta_7D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_7D_OneByteInt



subroutine ut_converta_1D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_1D_TwoByteInt


subroutine ut_converta_2D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_2D_TwoByteInt


subroutine ut_converta_3D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_3D_TwoByteInt


subroutine ut_converta_4D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_4D_TwoByteInt


subroutine ut_converta_5D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_5D_TwoByteInt


subroutine ut_converta_6D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_6D_TwoByteInt


subroutine ut_converta_7D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_7D_TwoByteInt



subroutine ut_converta_1D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_1D_FourByteInt


subroutine ut_converta_2D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_2D_FourByteInt


subroutine ut_converta_3D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_3D_FourByteInt


subroutine ut_converta_4D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_4D_FourByteInt


subroutine ut_converta_5D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_5D_FourByteInt


subroutine ut_converta_6D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_6D_FourByteInt


subroutine ut_converta_7D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_7D_FourByteInt



subroutine ut_converta_1D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_1D_EightByteInt


subroutine ut_converta_2D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_2D_EightByteInt


subroutine ut_converta_3D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_3D_EightByteInt


subroutine ut_converta_4D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_4D_EightByteInt


subroutine ut_converta_5D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_5D_EightByteInt


subroutine ut_converta_6D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_6D_EightByteInt


subroutine ut_converta_7D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             intent( in) :: from
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_7D_EightByteInt



subroutine ut_converta_1D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_1D_FourByteReal


subroutine ut_converta_2D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_2D_FourByteReal


subroutine ut_converta_3D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_3D_FourByteReal


subroutine ut_converta_4D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_4D_FourByteReal


subroutine ut_converta_5D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_5D_FourByteReal


subroutine ut_converta_6D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_6D_FourByteReal


subroutine ut_converta_7D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             intent( in) :: from
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_7D_FourByteReal



subroutine ut_converta_1D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_1D_EightByteReal


subroutine ut_converta_2D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_2D_EightByteReal


subroutine ut_converta_3D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_3D_EightByteReal


subroutine ut_converta_4D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_4D_EightByteReal


subroutine ut_converta_5D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_5D_EightByteReal


subroutine ut_converta_6D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_6D_EightByteReal


subroutine ut_converta_7D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             intent( in) :: from
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             intent(out) :: values

  real(kind = EightByteReal)             :: slope, intercept

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
! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_converta_7D_EightByteReal



!-----------------------------------------------------------------------
! 3. Convert pointers to arrays
!-----------------------------------------------------------------------

subroutine ut_convertp_1D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(1)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_1D_OneByteInt


subroutine ut_convertp_2D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(2)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_2D_OneByteInt


subroutine ut_convertp_3D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(3)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_3D_OneByteInt


subroutine ut_convertp_4D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(4)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_4D_OneByteInt


subroutine ut_convertp_5D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(5)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_5D_OneByteInt


subroutine ut_convertp_6D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(6)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_6D_OneByteInt


subroutine ut_convertp_7D_OneByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: from
  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(7)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_7D_OneByteInt



subroutine ut_convertp_1D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(1)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_1D_TwoByteInt


subroutine ut_convertp_2D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(2)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_2D_TwoByteInt


subroutine ut_convertp_3D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(3)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_3D_TwoByteInt


subroutine ut_convertp_4D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(4)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_4D_TwoByteInt


subroutine ut_convertp_5D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(5)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_5D_TwoByteInt


subroutine ut_convertp_6D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(6)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_6D_TwoByteInt


subroutine ut_convertp_7D_TwoByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: from
  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(7)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_7D_TwoByteInt



subroutine ut_convertp_1D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(1)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_1D_FourByteInt


subroutine ut_convertp_2D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(2)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_2D_FourByteInt


subroutine ut_convertp_3D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(3)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_3D_FourByteInt


subroutine ut_convertp_4D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(4)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_4D_FourByteInt


subroutine ut_convertp_5D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(5)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_5D_FourByteInt


subroutine ut_convertp_6D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(6)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_6D_FourByteInt


subroutine ut_convertp_7D_FourByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: from
  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(7)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_7D_FourByteInt



subroutine ut_convertp_1D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(1)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_1D_EightByteInt


subroutine ut_convertp_2D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(2)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_2D_EightByteInt


subroutine ut_convertp_3D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(3)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_3D_EightByteInt


subroutine ut_convertp_4D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(4)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_4D_EightByteInt


subroutine ut_convertp_5D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(5)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_5D_EightByteInt


subroutine ut_convertp_6D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(6)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_6D_EightByteInt


subroutine ut_convertp_7D_EightByteInt (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: from
  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(7)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_7D_EightByteInt



subroutine ut_convertp_1D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(1)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_1D_FourByteReal


subroutine ut_convertp_2D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(2)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_2D_FourByteReal


subroutine ut_convertp_3D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(3)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_3D_FourByteReal


subroutine ut_convertp_4D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(4)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_4D_FourByteReal


subroutine ut_convertp_5D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(5)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_5D_FourByteReal


subroutine ut_convertp_6D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(6)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_6D_FourByteReal


subroutine ut_convertp_7D_FourByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer :: from
  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(7)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_7D_FourByteReal



subroutine ut_convertp_1D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(1)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_1D_EightByteReal


subroutine ut_convertp_2D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(2)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_2D_EightByteReal


subroutine ut_convertp_3D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(3)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_3D_EightByteReal


subroutine ut_convertp_4D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(4)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_4D_EightByteReal


subroutine ut_convertp_5D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(5)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_5D_EightByteReal


subroutine ut_convertp_6D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(6)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_6D_EightByteReal


subroutine ut_convertp_7D_EightByteReal (from, from_unit, values, to_unit)

  use typeSizes
  use unitconvert, only: ropp_unit_conversion

  implicit none

  character(len = *),        intent( in) :: from_unit
  character(len = *),        intent( inout) :: to_unit
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer :: from
  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                             pointer :: values

  real(kind = EightByteReal)             :: slope, intercept

  integer                                :: cnts(7)
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
! Allocate Memory
! ---------------

  cnts(:) = shape(from)

  allocate(values(cnts(1), cnts(2), cnts(3), cnts(4), cnts(5), cnts(6), cnts(7)))

! Convert units
! -------------

  values = slope * from + intercept

end subroutine ut_convertp_7D_EightByteReal


