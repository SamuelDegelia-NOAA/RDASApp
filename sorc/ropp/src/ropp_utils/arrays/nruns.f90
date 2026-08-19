! $Id$

function nruns(iarray) result (n_runs)

!****f* Arrays/nruns *
!
! NAME
!    nruns - Gives # of runs of consecutive integers in an array.
!
! SYNOPSIS
!    n_runs = nruns(iarray)
! 
! DESCRIPTION
!    This function returns the number of runs of consecutive integers
!    in a given (integer) array. This routine is meant to be used with
!    arrays obtained from the where_indices function.
!
! INPUTS
!    int :: iarray(:)
!
! OUTPUT
!    int :: n_runs
!
! SEE ALSO
!    getrun
!
! REFERENCES
!    This function is reimplemented in Fortran 90 from the original
!    IDL function nruns.pro written by R. Sterner; this function is
!    is part of the John Hopkins University / Applied Physics
!    Laboratory (JHU/APL) IDL library.
!
! AUTHOR
!    C. Marquardt, West Hill, UK    <christian@marquardt.fsnet.co.uk>
!
!****

!--------------------------------------------------------------------------
! 1. Declarations
!--------------------------------------------------------------------------

  use arrays, not_this => nruns

  implicit none

  integer, dimension(:), intent(in)  :: iarray
  integer                            :: n_runs

  integer, dimension(size(iarray))   :: dist
  integer, dimension(:), pointer     :: loc

!--------------------------------------------------------------------------
! 2. Calculate run lengths
!--------------------------------------------------------------------------

  n_runs = 0
  dist = iarray - cshift(iarray, -1)
  loc => where_indices(dist /= 1, n_runs)

  deallocate(loc)

end function nruns
