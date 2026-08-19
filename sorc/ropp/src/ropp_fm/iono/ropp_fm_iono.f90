! $Id$

!****m* Modules/ropp_fm_iono *
!
! NAME
!    ropp_fm_iono - Interface module for the ropp_fm direct_ion feature.
!
! SYNOPSIS
!    USE ropp_fm_iono
! 
! DESCRIPTION
!    This module provides interfaces for some "ionospheric" routines contained
!    in the ROPP FM library.
!
! NOTES
!
! SEE ALSO
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

MODULE ropp_fm_iono

!-------------------------------------------------------------------------------
! 1. Defaulting routines
!-------------------------------------------------------------------------------

  INTERFACE

    SUBROUTINE ropp_fm_iono_set_default(ro_data)
      USE ropp_io_types
      TYPE(ROprof),      INTENT(inout)      :: ro_data
    END SUBROUTINE ropp_fm_iono_set_default

  END INTERFACE

!-------------------------------------------------------------------------------
! 2. Unpacking routines
!-------------------------------------------------------------------------------

  INTERFACE ropp_fm_iono_unpack

    SUBROUTINE ropp_fm_iono_unpack_bangle(res_data)
      USE ropp_io_types
      TYPE(ROprof), INTENT(inout)           :: res_data
    END SUBROUTINE ropp_fm_iono_unpack_bangle

  END INTERFACE

END MODULE ropp_fm_iono
