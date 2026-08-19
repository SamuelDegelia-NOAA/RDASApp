! $Id$

!****f* Arrays/uniq *
!
! NAME
!    uniq - Return the subscripts of the unique elements in an array.
!
! SYNOPSIS
!    idx => uniq(array [, index])
! 
! DESCRIPTION
!    This function returns the subscripts of the unique elements in an
!    array. Note that repeated elements must be adjacent in order to be
!    found.  This routine is intended to be used with the sort function;
!    see the discussion of the index argument below.
!
! INPUTS
!    ...          :: array(:)   The array to be scanned.
!    int          :: index(:)   An array of indices into array that order
!                                 the elements into monotonic order.
!
! OUTPUT
!    ..., pointer :: idx(:)     An array of indicies into array.
!
! SEE ALSO
!    unique
!
! REFERENCES
!    This is a Fortran 90 implementation of IDL's uniq() function.
!
! AUTHOR
!    C. Marquardt, West Hill, UK     <christian@marquardt.fsnet.co.uk>
!
!**** 


!****f* Arrays/unique *
!
! NAME
!    uniq - Return the unique elements in an array.
!
! SYNOPSIS
!    array => uniq(array [, index])
! 
! DESCRIPTION
!    This function returns the unique elements in an array. Note that
!    repeated elements must be adjacent in order to be found.  This
!    routine is intended to be used with the sort function; see the
!    discussion of the index argument below.
!
! INPUTS
!    ...          :: array(:)   The array to be scanned.
!    int          :: index(:)   An array of indices into array that order
!                                 the elements into monotonic order.
!
! OUTPUT
!    ..., pointer :: uarray(:)  An array with the unique elements of array.
!
! SEE ALSO
!    uniq
!
! REFERENCES
!    This function was inspired by the unique() function in Ray Sterner's 
!    JHUA library.
!
! AUTHOR
!    C. Marquardt, West Hill, UK     <christian@marquardt.fsnet.co.uk>
!
!**** 


!--------------------------------------------------------------------------
! 1. Return index array (single precision)
!--------------------------------------------------------------------------

function uniqs (array, idx) result (indices)

  use typesizes, only: wp => FourByteReal
  use arrays,    only: where_indices

  implicit none

  real(wp), dimension(:), intent(in)           :: array
  integer,  dimension(:), intent(in), optional :: idx
  integer,  dimension(:), pointer              :: indices

  real(wp), dimension(size(array))             :: harray
  integer                                      :: nc = 0

  if (present(idx)) then
     harray = array(idx)
     indices => where_indices(harray /= cshift(harray, -1), nc)
     if (nc == 0) then
        indices(1) = size(array)
     else
        indices = idx(indices)
     endif
  else
     indices => where_indices(array /= cshift(array, -1), nc)
     if (nc == 0) then
        indices(1) = size(array)
     endif
  endif
  
end function uniqs


!--------------------------------------------------------------------------
! 2. Return index array (double precision)
!--------------------------------------------------------------------------

function uniqd (array, idx) result (indices)

  use typesizes, only: wp => EightByteReal
  use arrays,    only: where_indices

  implicit none

  real(wp), dimension(:), intent(in)           :: array
  integer,  dimension(:), intent(in), optional :: idx
  integer,  dimension(:), pointer              :: indices

  real(wp), dimension(size(array))             :: harray
  integer                                      :: nc = 0

  if (present(idx)) then
     harray = array(idx)
     indices => where_indices(harray /= cshift(harray, -1), nc)
     if (nc == 0) then
        indices(1) = size(array)
     else
        indices = idx(indices)
     endif
  else
     indices => where_indices(array /= cshift(array, -1), nc)
     if (nc == 0) then
        indices(1) = size(array)
     endif
  endif
  
end function uniqd


!--------------------------------------------------------------------------
! 3. Return unique array elements (single precision)
!--------------------------------------------------------------------------

function uniques(array, idx) result (values)

  use typesizes, only: wp => FourByteReal
  use arrays,    only: where_indices

  implicit none

  real(wp), dimension(:), intent(in)           :: array
  integer,  dimension(:), intent(in), optional :: idx
  integer,  dimension(:), pointer              :: indices
  real(wp), dimension(:), pointer              :: values

  real(wp), dimension(:), allocatable          :: harray
  integer                                      :: nc = 0

  if (present(idx)) then
     allocate(harray(size(idx)))
     harray = array(idx)
     indices => where_indices(harray /= cshift(harray, -1), nc)
     if (nc == 0) then
        allocate(values(1))
        values = harray(1)
     else
        allocate(values(nc))
        values = harray(indices)
     endif
     deallocate(harray)
  else
     indices => where_indices(array /= cshift(array, -1), nc)
     if (nc == 0) then
        allocate(values(1))
        values = array(1)
     else
        allocate(values(nc))
        values = array(indices)
     endif
  endif
  deallocate(indices)

end function uniques


!--------------------------------------------------------------------------
! 4. Return unique array elements (double precision)
!--------------------------------------------------------------------------

function uniqued(array, idx) result (values)

  use typesizes, only: wp => EightByteReal
  use arrays,    only: where_indices

  implicit none

  real(wp), dimension(:), intent(in)           :: array
  integer,  dimension(:), intent(in), optional :: idx
  integer,  dimension(:), pointer              :: indices
  real(wp), dimension(:), pointer              :: values

  real(wp), dimension(:), allocatable          :: harray
  integer                                      :: nc = 0

  if (present(idx)) then
     allocate(harray(size(idx)))
     harray = array(idx)
     indices => where_indices(harray /= cshift(harray, -1), nc)
     if (nc == 0) then
        allocate(values(1))
        values = harray(1)
     else
        allocate(values(nc))
        values = harray(indices)
     endif
     deallocate(harray)
  else
     indices => where_indices(array /= cshift(array, -1), nc)
     if (nc == 0) then
        allocate(values(1))
        values = array(1)
     else
        allocate(values(nc))
        values = array(indices)
     endif
  endif
  deallocate(indices)

end function uniqued
