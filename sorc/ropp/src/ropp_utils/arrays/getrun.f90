! $Id$

function getrun(iarray, n, m, longest, last) result (run)

!****f* Arrays/getrun *
!
! NAME
!    getrun - Gives N'th run of consecutive integers from a given array.
!
! SYNOPSIS
!    run => getrun(iarray, n, m, longest)
! 
! DESCRIPTION
!    This function returns the N'th run of consecutive integers in a 
!    given (integer) array. This function is meant to be used with
!    arrays obtained from the where_indices function.
!
! INPUTS
!    int     iarray(:)    Integer array to process.
!    int     n            Run number to get (first = 1 = def).
!    int     m            Optional last run number to get.
!    logical last         If .true., n is offset from last run.
!    logical longest      Returns the longest run.
!
! OUTPUT
!    run
!
! NOTES
!    getrun is meant to be used on the output from where_indices. If m > n,
!    run will be a set of runs from run n to run m.  If no m is given,
!    run will be a single run. If n<0, the routine returns runs starting
!    at run abs(n) to end of iarray. If n is out of range, a -1 is
!    returned.
!
!    last = .true. means that n is an offset from last run. So n = -1
!    gives the last run, n = -2 gives next to last, ... If n = -3 and
!    m = -1, the last 3 runs are returned.
!
!    longest = .true. returns the longest run. n and m are both ignored.
!
! SEE ALSO
!    nruns
!
! REFERENCES
!    This function is reimplemented in Fortran 90 from the original
!    IDL function getrun.pro written by R. Sterner; this function is
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

  use arrays, not_this => getrun

  implicit none

  integer, dimension(:), intent(in)  :: iarray
  integer,               optional    :: n
  integer,               optional    :: m
  logical,               optional    :: longest
  logical,               optional    :: last
  integer, dimension(:), pointer     :: run

  integer, dimension(size(iarray))   :: dist
  integer, dimension(:), pointer     :: loc, idx
  integer, dimension(:), allocatable :: loc2, lenr

  integer                            :: ll, mth, nth, nn
  integer                            :: im, in
  integer                            :: n_runs = 0
  logical                            :: longest_run, last_run

!--------------------------------------------------------------------------
! 2. Default parameters
!--------------------------------------------------------------------------

  if (present(n)) then
     nth = n
  else
     if (present(last)) then
        if (last) then
           nth = -1
        else
           nth = 1
        endif
     else
        nth = 1
     endif
  endif

  if (present(m)) then
     mth = m
  else
     mth = nth
  endif

  if (present(longest)) then
     longest_run = longest
  else
     longest_run = .false.
  endif

  if (present(last)) then
     last_run = last
  else
     last_run = .false.
  endif

!--------------------------------------------------------------------------
! 3. Calculate number of runs and run lengths
!--------------------------------------------------------------------------

  dist = iarray - cshift(iarray, -1)
  loc => where_indices(dist /= 1, n_runs)

  allocate(loc2(size(loc)+1))
  allocate(lenr(size(loc)))

  loc2 = (/ loc, size(iarray)+1 /)
  lenr = loc2(2:) - loc2(1:size(loc2)-1)

!--------------------------------------------------------------------------
! 4. Extract longest run
!--------------------------------------------------------------------------

  if (longest_run) then
     idx => where_indices(lenr == maxval(lenr))
     nth = idx(1)
     ll  = loc(nth)

     allocate(run(lenr(nth)))
     run = iarray(ll:ll + lenr(nth) - 1)

     deallocate(idx, loc, loc2, lenr)
     return
  endif

!--------------------------------------------------------------------------
! 5. Extract runs with offset from last run
!--------------------------------------------------------------------------

  if (last_run) then
     in = n_runs + nth + 1
     im = n_runs + mth + 1
     if (in < 1 .and. im < 1) then
        allocate(run(1))
        run(1) = -1
        deallocate(loc, loc2, lenr)
        return
     endif
     in = max(in, 1)
     im = max(im, 1)
     if (in > n_runs .and. im > n_runs) then
        allocate(run(1))
        run(1) = -1
        deallocate(loc, loc2, lenr)
        return
     endif
     in = min(in, n_runs)
     im = min(im, n_runs)
     ll = loc(in)
     allocate(run(size(iarray(ll:loc(im) + lenr(im) - 1))))
     run = iarray(ll:loc(im) + lenr(im) - 1)
     deallocate(loc, loc2, lenr)
     return
  endif

!--------------------------------------------------------------------------
! 6. Extract remaining run(s)
!--------------------------------------------------------------------------

  nn = abs(n)

  if (nn > n_runs) then
     allocate(run(1))
     run(1) = -1
     deallocate(loc, loc2, lenr)
     return
  endif

  ll = loc(nn)

  if (nth < 0) then
     allocate(run(size(iarray)-ll+1))
     run = iarray(ll:)
     deallocate(loc, loc2, lenr)
  else
     if (mth > n_runs) mth = n_runs
     
     allocate(run(size(iarray(ll:loc(mth) + lenr(mth) - 1))))
     run = iarray(ll:loc(mth) + lenr(mth) -1 )
     deallocate(loc, loc2, lenr)
  endif

end function getrun
