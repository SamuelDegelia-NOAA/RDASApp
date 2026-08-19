! $Id$

!****f* Arrays/locate *
!
! NAME
!    locate - locate where a (list of) number(s) can be found in an array.
!
! SYNOPSIS
!    index = locate(array, point(s))
! 
! DESCRIPTION
!    This function returns the index or indices of point(s) within an array.
!    If the point is outside the array bounds, either 0 or n + 1 will be
!    returned, n being the number of elements in array. If the point is
!    within the range spanned by the array, the returned index will give the
!    position of the sought for point such that
!
!       array(i) <= point <= array(i+1)
!
! INPUTS
!    ...,    :: array(:)
!    ...,    :: points(:)
!
! OUTPUT
!    integer :: index(:)
!
! NOTES
!    The data array must be either monotonically increasing or decreasing
!    for the algorithm to work.
!
!    The index array must have the same number of elements as the point(s)
!    array.
!
! REFERENCES
!    This routine has been copied (by hand) and slightly edited from the
!    fgauss routine found in
!
!       Press, W.H, Teukolsky, S.A. Vetterling, W.T. and Flannery, B.P.,
!          Numerical Recipes in Fortran 90, Cambridge University Press, 
!          Cambridge, 1999.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!-------------------------------------------------------------------------------
! 1. Float, single output value
!-------------------------------------------------------------------------------

function locate_single_float(array, point) result(index)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: array
  real(wp),               intent(in) :: point
  integer                            :: index

  integer                            :: n, jl, jm, ju
  logical                            :: ascnd

! 1.2 Find index
! --------------

  n     = size(array)
  ascnd = (array(n) >= array(1))

  jl = 0
  ju = n + 1

  do
     if (ju - jl <= 1) exit
     jm = (ju + jl) / 2
     if (ascnd .eqv. (point >= array(jm))) then
        jl = jm
     else
        ju = jm
     end if
  end do

  if (point == array(1)) then
     index = 1
  else if (point == array(n)) then
     index = n - 1
  else
     index = jl
  end if

end function locate_single_float

!-------------------------------------------------------------------------------
! 2. Float, multiple output values
!-------------------------------------------------------------------------------

function locate_multi_float(array, points) result(index)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => FourByteReal
  use arrays, not_this => locate_multi_float

  implicit none

  real(wp), dimension(:), intent(in) :: array
  real(wp), dimension(:), intent(in) :: points
  integer,  dimension(size(points))  :: index

  real(wp)                           :: pp

  integer                            :: i

! 2.2 Find indeces
! ----------------

  do i = 1, size(points)
     pp = points(i)
     index(i) = locate_single_float(array, pp)
  enddo

end function locate_multi_float

!-------------------------------------------------------------------------------
! 3. Double, single output value
!-------------------------------------------------------------------------------

function locate_single_double(array, point) result(index)

! 3.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension(:), intent(in) :: array
  real(wp),               intent(in) :: point
  integer                            :: index

  integer                            :: n, jl, jm, ju
  logical                            :: ascnd

! 3.2 Find index
! --------------

  n     = size(array)
  ascnd = (array(n) >= array(1))

  jl = 0
  ju = n + 1

  do
     if (ju - jl <= 1) exit
     jm = (ju + jl) / 2
     if (ascnd .eqv. (point >= array(jm))) then
        jl = jm
     else
        ju = jm
     end if
  end do

  if (point == array(1)) then
     index = 1
  else if (point == array(n)) then
     index = n - 1
  else
     index = jl
  end if

end function locate_single_double

!-------------------------------------------------------------------------------
! 4. Double, multiple output values
!-------------------------------------------------------------------------------

function locate_multi_double(array, points) result(index)

! 2.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use arrays, not_this => locate_multi_double

  implicit none

  real(wp), dimension(:), intent(in) :: array
  real(wp), dimension(:), intent(in) :: points
  integer,  dimension(size(points))  :: index

  real(wp)                           :: pp

  integer                            :: i

! 2.2 Find indices
! ----------------

  do i = 1, size(points)
     pp = points(i)
     index(i) = locate(array, pp)
  enddo

end function locate_multi_double

