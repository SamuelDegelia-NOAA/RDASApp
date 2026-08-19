! $Id$

!****f* Arrays/sort *
!
! NAME
!    sort - Indices for a sorted array. 
!
! SYNOPSIS
!    idx = sort(array [, reverse = .true.|.false.])
! 
! DESCRIPTION
!    This function returns the indices of a sorted array.
!
! INPUTS
!    ...     :: array(:)  Array to be sorted.
!    logical :: reverse   If true, do an descending sort (default is to
!                           sort the array with ascending values).
!
! OUTPUT
!    int     :: idx(:)    Indices of array after sorting.
!
! NOTES
!    The routine is based on the quick_sort algorithm; the default sort order
!    is ascending. The returned index array has the same length as the array
!    to be sorted.
!
!    This function supports and single and double precison arguments.
!
! SEE ALSO
!    sorted
!    quick_sort
!
! REFERENCES
!    The quick sort routine originates from:
!
!       Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) Programmers
!          Guide to Fortran 90, McGraw-Hill  ISBN 0-07-000248-7, pages
!          149-150.
!
!    and was modified by Alan Miller to include an associated integer array
!    which gives the positions of the elements in the original order.
!
! AUTHOR
!    C. Marquardt, West Hill, UK     <christian@marquardt.fsnet.co.uk>
!
!**** 

!****f* Arrays/sorted *
!
! NAME
!    sorted - Sort an array. 
!
! SYNOPSIS
!    array = sort(array [, reverse = .true.|.false.])
! 
! DESCRIPTION
!    This function returns a sorted array.
!
! INPUTS
!    ...     :: array(:)  Array to be sorted.
!    logical :: reverse   If true, do an descending sort (default is to
!                           sort the array with ascending values).
!
! OUTPUT
!    ...     :: array(:)  Sorted array.
!
! NOTES
!    The routine is based on the quick_sort algorithm; the default sort order
!    is ascending. The returned array has the same length as the array to be
!    to be sorted.
!
!    This function supports and single and double precison arguments.
!
! SEE ALSO
!    sort
!    quick_sort
!
! REFERENCES
!    The quick sort routine originates from:
!
!       Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) Programmers
!          Guide to Fortran 90, McGraw-Hill  ISBN 0-07-000248-7, pages
!          149-150.
!
!    and was modified by Alan Miller to include an associated integer array
!    which gives the positions of the elements in the original order.
!
! AUTHOR
!    C. Marquardt, West Hill, UK     <christian@marquardt.fsnet.co.uk>
!
!****


!****s* Arrays/quick_sort *
!
! NAME
!    quick_sort - Sort an array. 
!
! SYNOPSIS
!    call quick_sort(array, idx)
! 
! DESCRIPTION
!    This function sorts an array.
!
! INPUTS
!    ...     :: array(:)  Array to be sorted; will be overwritten.
!
! OUTPUT
!    ...     :: array(:)  Sorted array.
!    int     :: idx(:)    
!
! NOTES
!    The routine implements the quick_sort algorithm; the sort order is
!    ascending. array and idx have the same length.
!
!    This function supports and single and double precison arguments.
!
! SEE ALSO
!    sort
!    sorted
!
! REFERENCES
!    The quick sort routine originates from:
!
!       Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) Programmers
!          Guide to Fortran 90, McGraw-Hill  ISBN 0-07-000248-7, pages
!          149-150.
!
!    and was modified by Alan Miller to include an associated integer array
!    which gives the positions of the elements in the original order.
!
! AUTHOR
!    C. Marquardt, West Hill, UK     <christian@marquardt.fsnet.co.uk>
!
!****

!-------------------------------------------------------------------------------
! 1. Single precision, sort
!-------------------------------------------------------------------------------

function sorts(list, reverse) result (indices)

! Declarations
! ------------

  use typesizes, only: wp => FourByteReal
  use arrays,    only: quick_sort

  implicit none

  real(wp), dimension (:),        intent(in) :: list
  real(wp), dimension(size(list))            :: values
  integer,  dimension(size(list))            :: indices
  integer,                        optional   :: reverse

! Sorting...
! ----------

  values = list
  call quick_sort(values, indices)

! ...reversing
! ------------

  if (present(reverse)) then
     indices   = indices(size(list):1:-1)
  endif

end function sorts


!-------------------------------------------------------------------------------
! 2. Single precision, sorted
!-------------------------------------------------------------------------------

function sorteds(list, reverse) result (values)

! Declarations
! ------------

  use typesizes, only: wp => FourByteReal
  use arrays,    only: quick_sort

  implicit none

  real(wp), dimension (:),        intent(in) :: list
  real(wp), dimension(size(list))            :: values
  integer,  dimension(size(list))            :: indices
  integer,                        optional   :: reverse

! Sorting...
! ----------

  values = list
  call quick_sort(values, indices)

! ...reversing
! ------------

  if (present(reverse)) then
     values = values(size(list):1:-1)
  endif

end function sorteds


!-------------------------------------------------------------------------------
! 3. Single precision, quick_sort
!-------------------------------------------------------------------------------

subroutine quick_sorts(list, order)

! Declarations
! ------------

  use typesizes, only: wp => FourByteReal

  implicit none

  real(wp), dimension (:), intent(inout) :: list
  integer,  dimension (:), intent  (out) :: order

  integer                                :: i

! Prepare index array
! -------------------

  do i = 1, size(list)
     order(i) = i
  end do

! Do the real work
! ----------------

  call quick_sort_1s(1, size(list))


contains


  ! Utility routines doing the real work
  ! ------------------------------------

  recursive subroutine quick_sort_1s(left_end, right_end)

    integer, intent(in) :: left_end, right_end

    integer             :: i, j, itemp
    real                :: reference, temp
    integer, parameter  :: max_simple_sort_size = 6

    if (right_end < left_end + max_simple_sort_size) then
       ! use interchange sort for small lists
       call interchange_sorts(left_end, right_end)
    else
       ! use partition ("quick") sort
       reference = list((left_end + right_end)/2)
       i = left_end - 1; j = right_end + 1

       do
          ! scan list from left end until element >= reference is found
          do
             i = i + 1
             if (list(i) >= reference) exit
          end do
          ! scan list from right end until element <= reference is found
          do
             j = j - 1
             if (list(j) <= reference) exit
          end do

          if (i < j) then
             ! swap two out-of-order elements
             temp = list(i); list(i) = list(j); list(j) = temp
             itemp = order(i); order(i) = order(j); order(j) = itemp
          else if (i == j) then
             i = i + 1
             exit
          else
             exit
          end if
       end do

       if (left_end < j) call quick_sort_1s(left_end, j)
       if (i < right_end) call quick_sort_1s(i, right_end)
    end if

  end subroutine quick_sort_1s

  subroutine interchange_sorts(left_end, right_end)

    integer, intent(in) :: left_end, right_end

    integer             :: i, j, itemp
    real(wp)            :: temp

    do i = left_end, right_end - 1
       do j = i+1, right_end
          if (list(i) > list(j)) then
             temp = list(i); list(i) = list(j); list(j) = temp
             itemp = order(i); order(i) = order(j); order(j) = itemp
          end if
       end do
    end do

  end subroutine interchange_sorts

end subroutine quick_sorts


!-------------------------------------------------------------------------------
! 4. Double precision, sort
!-------------------------------------------------------------------------------

function sortd(list, reverse) result (indices)

! Declarations
! ------------

  use typesizes, only: wp => EightByteReal
  use arrays,    only: quick_sort

  implicit none

  real(wp), dimension (:),        intent(in) :: list
  real(wp), dimension(size(list))            :: values
  integer,  dimension(size(list))            :: indices
  integer,                        optional   :: reverse

! Sorting...
! ----------

  values = list
  call quick_sort(values, indices)

! ...reversing
! ------------

  if (present(reverse)) then
     indices   = indices(size(list):1:-1)
  endif

end function sortd


!-------------------------------------------------------------------------------
! 5. Double precision, sorted
!-------------------------------------------------------------------------------

function sortedd(list, reverse) result (values)

! Declarations
! ------------

  use typesizes, only: wp => EightByteReal
  use arrays,    only: quick_sort

  implicit none

  real(wp), dimension (:),        intent(in) :: list
  real(wp), dimension(size(list))            :: values
  integer,  dimension(size(list))            :: indices
  integer,                        optional   :: reverse

! Sorting...
! ----------

  values = list
  call quick_sort(values, indices)

! ...reversing
! ------------

  if (present(reverse)) then
     values = values(size(list):1:-1)
  endif

end function sortedd


!-------------------------------------------------------------------------------
! 6. Double precision, quick_sort
!-------------------------------------------------------------------------------

subroutine quick_sortd(list, order)

! Declarations
! ------------

  use typesizes, only: wp => EightByteReal

  implicit none

  real(wp), dimension (:), intent(inout) :: list
  integer,  dimension (:), intent  (out) :: order

  integer                                :: i

! Prepare index array
! -------------------

  do i = 1, size(list)
     order(i) = i
  end do

! Do the real work
! ----------------

  call quick_sort_1d(1, size(list))


contains


  ! Utility routines doing the real work
  ! ------------------------------------

  recursive subroutine quick_sort_1d(left_end, right_end)

    integer, intent(in) :: left_end, right_end

    integer             :: i, j, itemp
    real(wp)            :: reference, temp
    integer, parameter  :: max_simple_sort_size = 6

    if (right_end < left_end + max_simple_sort_size) then
       ! use interchange sort for small lists
       call interchange_sortd(left_end, right_end)
    else
       ! use partition ("quick") sort
       reference = list((left_end + right_end)/2)
       i = left_end - 1; j = right_end + 1

       do
          ! scan list from left end until element >= reference is found
          do
             i = i + 1
             if (list(i) >= reference) exit
          end do
          ! scan list from right end until element <= reference is found
          do
             j = j - 1
             if (list(j) <= reference) exit
          end do

          if (i < j) then
             ! swap two out-of-order elements
             temp = list(i); list(i) = list(j); list(j) = temp
             itemp = order(i); order(i) = order(j); order(j) = itemp
          else if (i == j) then
             i = i + 1
             exit
          else
             exit
          end if
       end do

       if (left_end < j) call quick_sort_1d(left_end, j)
       if (i < right_end) call quick_sort_1d(i, right_end)
    end if

  end subroutine quick_sort_1d

  subroutine interchange_sortd(left_end, right_end)

    integer, intent(in) :: left_end, right_end

    integer             :: i, j, itemp
    real(wp)            :: temp

    do i = left_end, right_end - 1
       do j = i+1, right_end
          if (list(i) > list(j)) then
             temp = list(i); list(i) = list(j); list(j) = temp
             itemp = order(i); order(i) = order(j); order(j) = itemp
          end if
       end do
    end do

  end subroutine interchange_sortd

end subroutine quick_sortd
