! $Id$

FUNCTION ropp_fm_version() RESULT (version)

!****f* Common/ropp_fm_version *
!
! NAME
!   ropp_fm_version
!
! SYNOPSIS
!   Return ROPP_FM version ID string
!
!   USE ropp_fm
!   version = ropp_fm_version()
!
! DESCRIPTION
!   This function returns the (common) version string for the ROPP_FM
!   module. By default, this function should be called by all ROPP_FM
!   tools to display a version ID when the '-v' command-line switch is
!   used.
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

  CHARACTER (LEN=40) :: version

! Edit this string when ROPP_FM module version is updated

  version = "v11.3 30-Nov-2023"

END FUNCTION ropp_fm_version
