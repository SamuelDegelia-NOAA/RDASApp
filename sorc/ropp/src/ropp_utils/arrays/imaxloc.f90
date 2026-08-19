! $Id$

!****f* Arrays/imaxloc *
!
! NAME
!    imaxloc - Index of first maximum value.
!
! SYNOPSIS
!    idx = imaxloc(array)
! 
! DESCRIPTION
!    This function returns the index of the first maximum value.
!
! INPUTS
!    array - 1D array of type real, double, or integer.
!
! OUTPUT
!    idx   - index position of the first maximum in array.
!
! NOTES
!    The routine is based on the Fortran 90 intrinsic function maxloc;
!    it just returns a scalar index for 1 dimensional arrays instead of
!    a rank one array.
!
! SEE ALSO
!    iminloc
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!-------------------------------------------------------------------------------
! 1. Integer arrays
!-------------------------------------------------------------------------------

function imaxloc_int(array) result(idx)

  implicit none

  integer, dimension(:), intent(in) :: array
  integer                           :: idx

  integer, dimension(1)             :: ii

  ii  = maxloc(array)
  idx = ii(1)

end function imaxloc_int

!-------------------------------------------------------------------------------
! 2. Float arrays
!-------------------------------------------------------------------------------

function imaxloc_float(array) result(idx)

  use typesizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: array
  integer                            :: idx

  integer,  dimension(1)             :: ii

  ii  = maxloc(array)
  idx = ii(1)

end function imaxloc_float

!-------------------------------------------------------------------------------
! 3. Double arrays
!-------------------------------------------------------------------------------

function imaxloc_double(array) result(idx)

  use typesizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: array
  integer                            :: idx

  integer,  dimension(1)             :: ii

  ii  = maxloc(array)
  idx = ii(1)

end function imaxloc_double
