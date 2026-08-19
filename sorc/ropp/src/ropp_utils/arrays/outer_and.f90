! $Id$

!****f* Arrays/outer_and *
!
! NAME
!    outer_product - Calculate the outer logical and of two vectors.
!
! SYNOPSIS
!    z = outer_and(x, y)
! 
! DESCRIPTION
!    This function returns the outer (dyadic) and between two
!    logical 1D vectors.
!
! INPUTS
!    logical, dimension(:)   :: x, y
!
! OUTPUT
!    logical, dimension(:,:) :: z
!
! NOTES
!    The result of an outer and of two 1d vectors is matrix!
!
! SEE ALSO
!      outer_product
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

function outer_and(x, y) result(z)

! Declarations
! ------------

  implicit none

  logical, dimension(:), intent(in)    :: x, y
  logical, dimension(size(x), size(y)) :: z

  integer                              :: i, j

! Calculate the outer and
! -----------------------

  do i = 1, size(x)
     do j = 1, size(y)
        z(i,j) = (x(i) .and. y(j))
     enddo
  enddo

end function outer_and
