! $Id: $

!****s* Arrays/isinrange *
!
! NAME
!    isinrange - Check if data values are within a given range of numbers.
!
! SYNOPSIS
!    ... = isinrange(data, range)
! 
! DESCRIPTION
!    This function returns .true. if all elements of an array are within
!    a given range, .false. if at least one element is outside the given
!    range.
!
! INPUTS
!    ..., dim(:[,:[...]]) :: data
!    ..., dimension(2)    :: range
!
! OUTPUT
!    logical              :: isinrange
!
! NOTES
!    The array range is a two element vector giving the minimum and
!    maximum value for the allowed (valid) range.
!
!    This subroutine supports integer, float and double arguments for
!    up to seven dimensions. Scalar data values can also be tested.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
! MODIFICATION HISTORY
!
!    $Log$
!    Revision 1.1  2005/05/11 11:20:41  frcm
!    Imported from the tools90 library.
!
!    Revision 1.2  2005/05/11 07:37:43  frcm
!    *** empty log message ***
!
!    Revision 1.1  2005/01/26 13:53:38  frcm
!    Added isinrange() function.
!
!****

!--------------------------------------------------------------------------
! 1. 1D array arguments
!--------------------------------------------------------------------------

function isinrange_1D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_1D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_1D_OneByteInt


function isinrange_1D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_1D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_1D_TwoByteInt


function isinrange_1D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_1D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_1D_FourByteInt


function isinrange_1D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_1D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_1D_EightByteInt


function isinrange_1D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_1D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_1D_FourByteReal


function isinrange_1D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_1D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_1D_EightByteReal



!--------------------------------------------------------------------------
! 2. 2D array arguments
!--------------------------------------------------------------------------

function isinrange_2D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_2D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_2D_OneByteInt


function isinrange_2D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_2D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_2D_TwoByteInt


function isinrange_2D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_2D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_2D_FourByteInt


function isinrange_2D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_2D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_2D_EightByteInt


function isinrange_2D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_2D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_2D_FourByteReal


function isinrange_2D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_2D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_2D_EightByteReal



!--------------------------------------------------------------------------
! 3. 3D array arguments
!--------------------------------------------------------------------------

function isinrange_3D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_3D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_3D_OneByteInt


function isinrange_3D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_3D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_3D_TwoByteInt


function isinrange_3D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_3D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_3D_FourByteInt


function isinrange_3D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_3D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_3D_EightByteInt


function isinrange_3D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_3D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_3D_FourByteReal


function isinrange_3D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_3D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_3D_EightByteReal



!--------------------------------------------------------------------------
! 4. 4D array arguments
!--------------------------------------------------------------------------

function isinrange_4D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_4D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_4D_OneByteInt


function isinrange_4D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_4D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_4D_TwoByteInt


function isinrange_4D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_4D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_4D_FourByteInt


function isinrange_4D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_4D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_4D_EightByteInt


function isinrange_4D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_4D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_4D_FourByteReal


function isinrange_4D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_4D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_4D_EightByteReal



!--------------------------------------------------------------------------
! 5. 5D array arguments
!--------------------------------------------------------------------------

function isinrange_5D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_5D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_5D_OneByteInt


function isinrange_5D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_5D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_5D_TwoByteInt


function isinrange_5D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_5D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_5D_FourByteInt


function isinrange_5D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_5D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_5D_EightByteInt


function isinrange_5D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_5D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_5D_FourByteReal


function isinrange_5D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_5D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_5D_EightByteReal



!--------------------------------------------------------------------------
! 6. 6D array arguments
!--------------------------------------------------------------------------

function isinrange_6D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_6D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_6D_OneByteInt


function isinrange_6D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_6D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_6D_TwoByteInt


function isinrange_6D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_6D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_6D_FourByteInt


function isinrange_6D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_6D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_6D_EightByteInt


function isinrange_6D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_6D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_6D_FourByteReal


function isinrange_6D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_6D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_6D_EightByteReal



!--------------------------------------------------------------------------
! 7. 7D array arguments
!--------------------------------------------------------------------------

function isinrange_7D_OneByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_7D_OneByteInt

  implicit none

  integer(kind = OneByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = OneByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_7D_OneByteInt


function isinrange_7D_TwoByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_7D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = TwoByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_7D_TwoByteInt


function isinrange_7D_FourByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_7D_FourByteInt

  implicit none

  integer(kind = FourByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = FourByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_7D_FourByteInt


function isinrange_7D_EightByteInt (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_7D_EightByteInt

  implicit none

  integer(kind = EightByteInt), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  integer(kind = EightByteInt), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_7D_EightByteInt


function isinrange_7D_FourByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_7D_FourByteReal

  implicit none

  real(kind = FourByteReal), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = FourByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_7D_FourByteReal


function isinrange_7D_EightByteReal (array, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_7D_EightByteReal

  implicit none

  real(kind = EightByteReal), dimension(:, :, :, :, :, :, :), &
                         intent(in) :: array
  real(kind = EightByteReal), dimension(2), &
                         intent(in) :: range
  logical                           :: inrange

! Check if all data points are within the range
! ---------------------------------------------

  inrange = all(array >= range(1) .and. array <= range(2))

end function isinrange_7D_EightByteReal



!--------------------------------------------------------------------------
! 8. Scalar arguments
!--------------------------------------------------------------------------

function isinrange_0D_OneByteInt (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_0D_OneByteInt

  implicit none

  integer(kind = OneByteInt), &
                         intent(in) :: value
  integer(kind = OneByteInt), dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function isinrange_0D_OneByteInt


function isinrange_0D_TwoByteInt (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_0D_TwoByteInt

  implicit none

  integer(kind = TwoByteInt), &
                         intent(in) :: value
  integer(kind = TwoByteInt), dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function isinrange_0D_TwoByteInt


function isinrange_0D_FourByteInt (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_0D_FourByteInt

  implicit none

  integer(kind = FourByteInt), &
                         intent(in) :: value
  integer(kind = FourByteInt), dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function isinrange_0D_FourByteInt


function isinrange_0D_EightByteInt (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_0D_EightByteInt

  implicit none

  integer(kind = EightByteInt), &
                         intent(in) :: value
  integer(kind = EightByteInt), dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function isinrange_0D_EightByteInt


function isinrange_0D_FourByteReal (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_0D_FourByteReal

  implicit none

  real(kind = FourByteReal), &
                         intent(in) :: value
  real(kind = FourByteReal), dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function isinrange_0D_FourByteReal


function isinrange_0D_EightByteReal (value, range) result (inrange)

! Declarations
! ------------

  use typeSizes
  use arrays, not_this => isinrange_0D_EightByteReal

  implicit none

  real(kind = EightByteReal), &
                         intent(in) :: value
  real(kind = EightByteReal), dimension(2),      &
                         intent(in) :: range
  logical                           :: inrange

! Check if the value is within the range
! --------------------------------------

  inrange = (value >= range(1) .and. value <= range(2))

end function isinrange_0D_EightByteReal


