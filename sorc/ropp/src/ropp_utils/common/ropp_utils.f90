! $Id$

module ropp_utils

!****m* Modules/ropp_utils *
!
! NAME
!    ropp_utils - The ROPP utils library
!
! SYNOPSIS
!    use ropp_utils
!
! DESCRIPTION
!    This Fortran module provides interfaces required for the use of the
!    ROPP utils library
!
! SEE ALSO
!    arrays
!    geodesy
!    coordinates
!    datetimeprogs
!    messages
!    unitconvert
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

!-------------------------------------------------------------------------------
! 1. Include all ropp_utils modules
!-------------------------------------------------------------------------------

  use typesizes, only: wp => EightByteReal

  use arrays
  use geodesy
  use coordinates
  use datetimeprogs
  use messages
  use earthmod
  use unitconvert

!-------------------------------------------------------------------------------
! 2. ROPP missing value definitions
!-------------------------------------------------------------------------------

!****ip* Initialisation/ropp_mdfv
!
! NAME
!    ropp_MDFV - Internal global 'Missing Data Flag/Test Value(s)'
!
! NOTES
!    These parameters are used to indicate/test a 'missing' (invalid)
!    data value.
!    ropp_MDFV should be used to set invalid data for most
!      ROPP parameters, but a single universal value is not suitable for
!      all; some - e.g. (X,Y,Z) coordinate vectors - are set to zero.
!      For others, parameter-specific values may be more appropriate.
!    ropp_ZERO can be used to set parameters to zero.
!    ropp_MDTV can used for testing for invalid parameter values;
!      anything less than this value can be assumed to be set 'missing',
!      though again, some parameters may have specific values to test for.
!    ropp_ZDTV can be used to test for (almost) zero, e.g.
!      if ( abs(value) < ropp_ZDTV ) then ...
!         ! value can be considered to be zero
!    ropp_MIFV and ropp_MITV are integer equivalents of
!      ropp_MDFV and ropp_MDTV respectively.
!
! SOURCE
!
  REAL(wp), PARAMETER              :: ropp_MDFV = -99999000.0_wp
  REAL(wp), PARAMETER              :: ropp_ZERO =     0.0_wp
  INTEGER,  PARAMETER              :: ropp_MIFV =    -999

  REAL(wp), PARAMETER              :: ropp_MDTV = -9999.0_wp
  REAL(wp), PARAMETER              :: ropp_ZDTV =   1e-10_wp

  INTEGER,  PARAMETER              :: ropp_MITV =     -99
!
!****

!-------------------------------------------------------------------------------
! 3. Misc interfaces
!-------------------------------------------------------------------------------

  interface ropp_utils_version
    function ropp_utils_version() result(version)
      character (len=20) :: version
    end function ropp_utils_version
  end interface ropp_utils_version

  interface
     subroutine To_Lower (string)
       implicit none
       character (Len=*), intent(inout) :: string
     end subroutine To_Lower
  end interface

  interface
     subroutine To_Upper (string)
       implicit none
       character (Len=*), intent(inout) :: string
     end subroutine To_Upper
  end interface

  interface
     subroutine File_Delete(file, ierr)
       implicit none
       character(len = *), intent(in)  :: file
       integer,            intent(out) :: ierr
     end subroutine File_Delete
  end interface

  interface
     function get_io_unit() result(unit)
       implicit none
       integer :: unit
     end function get_io_unit
  end interface

end module ropp_utils
