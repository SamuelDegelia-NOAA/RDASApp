! $Id: ropp_math.f90 6655 2021-04-08 14:49:35Z idculv $

!****m* Modules/ropp_math *
!
! NAME
!    ropp_math - Interface module for the loe level ROPP maths routines.
!
! SYNOPSIS
!    USE math
!
! DESCRIPTION
!    This module provides interfaces for all 'maths' routines contained
!    in the ROPP UTILS library.
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

MODULE math

!-------------------------------------------------------------------------------
! 1. Gamma functions
!-------------------------------------------------------------------------------

  INTERFACE lngamma
    RECURSIVE FUNCTION lngamma(x) RESULT (lg)
      USE typesizes, ONLY: wp => EightByteReal
      REAL(wp), INTENT(in)                :: x
      REAL(wp)                            :: lg
    END FUNCTION lngamma
  END INTERFACE

END MODULE math
