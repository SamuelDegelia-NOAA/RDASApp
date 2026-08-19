! $Id$

!****f* Arrays/setminus *
!
! NAME
!    setminus - Eliminate elements from a set that are also in
!               another set.
!
! SYNOPSIS
!    c = setminus(a, b)
! 
! DESCRIPTION
!    This function removes all elements in a which are also part of b. It
!    is intended to be used with index arrays.
!
! INPUTS
!    a = set (array) to process.
!    b = set (array) of elements to eliminate.
!
! OUTPUT
!    c = resulting set (array).
!
! NOTES
!    This function should work for all data types, however: an equality
!    constraint for floating point data, regardless of single of double
!    precision, is a somewhat suspect concept.
!
! REFERENCES
!    This function is reimplemented in Fortran 90 from the original
!    IDL function setminus.pro written by R. Sterner; this function is
!    is part of the John Hopkins University / Applied Physics
!    Laboratory (JHU/APL) IDL library.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!--------------------------------------------------------------------------
! 1. Integer
!--------------------------------------------------------------------------

function setminus_int(a, b) result (c)

! Declarations
! ------------

  use arrays, not_this => setminus_int

  implicit none

  integer, dimension(:), intent(in) :: a
  integer, dimension(:), intent(in) :: b
  integer, dimension(:), pointer    :: c

  integer, dimension(:), pointer    :: idx
  integer                           :: i
  integer                           :: cnt = 0

! Allocate pointer
! ----------------

  allocate(c(size(a)))

  c = a

  do i = 1, size(b)
     idx => where_indices(c /= b(i), cnt)
     if (cnt > 0) then
        c(1:cnt) = c(idx) ; call reallocate(c, cnt)
     endif
     deallocate(idx)
  enddo

end function setminus_int


!--------------------------------------------------------------------------
! 2. Single
!--------------------------------------------------------------------------

function setminus_float(a, b) result (c)

! Declarations
! ------------

  use typesizes, only: wp => FourByteReal
  use arrays, not_this => setminus_float

  implicit none

  real(wp), dimension(:), intent(in) :: a
  real(wp), dimension(:), intent(in) :: b
  real(wp), dimension(:), pointer    :: c

  integer,  dimension(:), pointer    :: idx
  integer                            :: i
  integer                            :: cnt = 0

! Allocate pointer
! ----------------

  allocate(c(size(a)))

  c = a

  do i = 1, size(b)
     idx => where_indices(abs(c - b(i)) > 2.0_wp * tiny(1.0_wp), cnt)
     if (cnt > 0) then
        c(1:cnt) = c(idx) ; call reallocate(c, cnt)
     endif
     deallocate(idx)
  enddo

end function setminus_float


!--------------------------------------------------------------------------
! 3. Double
!--------------------------------------------------------------------------

function setminus_double(a, b) result (c)

! Declarations
! ------------

  use typesizes, only: wp => EightByteReal
  use arrays, not_this => setminus_double

  implicit none

  real(wp), dimension(:), intent(in) :: a
  real(wp), dimension(:), intent(in) :: b
  real(wp), dimension(:), pointer    :: c

  integer,  dimension(:), pointer    :: idx
  integer                            :: i
  integer                            :: cnt = 0

! Allocate pointer
! ----------------

  allocate(c(size(a)))

  c = a

  do i = 1, size(b)
     idx => where_indices(abs(c - b(i)) > 2.0_wp * tiny(1.0_wp), cnt)
     if (cnt > 0) then
        c(1:cnt) = c(idx) ; call reallocate(c, cnt)
     endif
     deallocate(idx)
  enddo

end function setminus_double
