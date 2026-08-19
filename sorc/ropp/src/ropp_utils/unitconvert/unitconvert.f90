! $Id: $

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
      subroutine ut_converts_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), &
                                   intent( in) :: from
        integer(kind = OneByteInt), &
                                   intent(out) :: values
      end subroutine ut_converts_OneByteInt


      subroutine ut_converts_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), &
                                   intent(out) :: values
      end subroutine ut_converts_TwoByteInt


      subroutine ut_converts_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), &
                                   intent( in) :: from
        integer(kind = FourByteInt), &
                                   intent(out) :: values
      end subroutine ut_converts_FourByteInt


      subroutine ut_converts_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), &
                                   intent( in) :: from
        integer(kind = EightByteInt), &
                                   intent(out) :: values
      end subroutine ut_converts_EightByteInt


      subroutine ut_converts_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), &
                                   intent( in) :: from
        real(kind = FourByteReal), &
                                   intent(out) :: values
      end subroutine ut_converts_FourByteReal


      subroutine ut_converts_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), &
                                   intent( in) :: from
        real(kind = EightByteReal), &
                                   intent(out) :: values
      end subroutine ut_converts_EightByteReal



      subroutine ut_converta_1D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:), &
                                   intent(out) :: values
      end subroutine ut_converta_1D_OneByteInt


      subroutine ut_converta_2D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:, :), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:, :), &
                                   intent(out) :: values
      end subroutine ut_converta_2D_OneByteInt


      subroutine ut_converta_3D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:, :, :), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_3D_OneByteInt


      subroutine ut_converta_4D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:, :, :, :), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_4D_OneByteInt


      subroutine ut_converta_5D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_5D_OneByteInt


      subroutine ut_converta_6D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_6D_OneByteInt


      subroutine ut_converta_7D_OneByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_7D_OneByteInt



      subroutine ut_converta_1D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:), &
                                   intent(out) :: values
      end subroutine ut_converta_1D_TwoByteInt


      subroutine ut_converta_2D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:, :), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:, :), &
                                   intent(out) :: values
      end subroutine ut_converta_2D_TwoByteInt


      subroutine ut_converta_3D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:, :, :), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_3D_TwoByteInt


      subroutine ut_converta_4D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:, :, :, :), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_4D_TwoByteInt


      subroutine ut_converta_5D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_5D_TwoByteInt


      subroutine ut_converta_6D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_6D_TwoByteInt


      subroutine ut_converta_7D_TwoByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_7D_TwoByteInt



      subroutine ut_converta_1D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:), &
                                   intent(out) :: values
      end subroutine ut_converta_1D_FourByteInt


      subroutine ut_converta_2D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:, :), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:, :), &
                                   intent(out) :: values
      end subroutine ut_converta_2D_FourByteInt


      subroutine ut_converta_3D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:, :, :), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_3D_FourByteInt


      subroutine ut_converta_4D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:, :, :, :), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_4D_FourByteInt


      subroutine ut_converta_5D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_5D_FourByteInt


      subroutine ut_converta_6D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_6D_FourByteInt


      subroutine ut_converta_7D_FourByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_7D_FourByteInt



      subroutine ut_converta_1D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:), &
                                   intent(out) :: values
      end subroutine ut_converta_1D_EightByteInt


      subroutine ut_converta_2D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:, :), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:, :), &
                                   intent(out) :: values
      end subroutine ut_converta_2D_EightByteInt


      subroutine ut_converta_3D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:, :, :), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_3D_EightByteInt


      subroutine ut_converta_4D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:, :, :, :), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_4D_EightByteInt


      subroutine ut_converta_5D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_5D_EightByteInt


      subroutine ut_converta_6D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_6D_EightByteInt


      subroutine ut_converta_7D_EightByteInt (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent( in) :: from
        integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_7D_EightByteInt



      subroutine ut_converta_1D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:), &
                                   intent(out) :: values
      end subroutine ut_converta_1D_FourByteReal


      subroutine ut_converta_2D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:, :), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:, :), &
                                   intent(out) :: values
      end subroutine ut_converta_2D_FourByteReal


      subroutine ut_converta_3D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:, :, :), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_3D_FourByteReal


      subroutine ut_converta_4D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:, :, :, :), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_4D_FourByteReal


      subroutine ut_converta_5D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:, :, :, :, :), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_5D_FourByteReal


      subroutine ut_converta_6D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_6D_FourByteReal


      subroutine ut_converta_7D_FourByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                                   intent( in) :: from
        real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_7D_FourByteReal



      subroutine ut_converta_1D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:), &
                                   intent(out) :: values
      end subroutine ut_converta_1D_EightByteReal


      subroutine ut_converta_2D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:, :), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:, :), &
                                   intent(out) :: values
      end subroutine ut_converta_2D_EightByteReal


      subroutine ut_converta_3D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:, :, :), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_3D_EightByteReal


      subroutine ut_converta_4D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:, :, :, :), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_4D_EightByteReal


      subroutine ut_converta_5D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:, :, :, :, :), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_5D_EightByteReal


      subroutine ut_converta_6D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_6D_EightByteReal


      subroutine ut_converta_7D_EightByteReal (from, from_unit, values, to_unit)
        use typeSizes
        character(len = *),        intent( in) :: from_unit
        character(len = *),        intent( inout) :: to_unit
        real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                                   intent( in) :: from
        real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                                   intent(out) :: values
      end subroutine ut_converta_7D_EightByteReal








  end interface

end module unitconvert
