! $Id$

!****f* Arrays/iminloc *
!
! NAME
!    iminloc - Index of first minimum value.
!
! SYNOPSIS
!    idx = iminloc(array)
! 
! DESCRIPTION
!    This function returns the index of the first minimum value.
!
! INPUTS
!    array - 1D array of type real, double, or integer.
!
! OUTPUT
!    idx   - index position of the first maximum in array.
!
! NOTES
!    The routine is based on the Fortran 90 intrinsic function minloc;
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

function iminloc_int(array) result(idx)

  implicit none

  integer, dimension(:), intent(in) :: array
  integer                           :: idx

  integer, dimension(1)             :: ii

  ii  = minloc(array)
  idx = ii(1)

end function iminloc_int

!-------------------------------------------------------------------------------
! 2. Float arrays
!-------------------------------------------------------------------------------

function iminloc_float(array) result(idx)

  use typesizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: array
  integer                            :: idx

  integer,  dimension(1)             :: ii

  ii  = minloc(array)
  idx = ii(1)

end function iminloc_float

!-------------------------------------------------------------------------------
! 3. Double arrays
!-------------------------------------------------------------------------------

function iminloc_double(array) result(idx)

  use typesizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: array
  integer                            :: idx

  integer,  dimension(1)             :: ii

  ii  = minloc(array)
  idx = ii(1)

end function iminloc_double
