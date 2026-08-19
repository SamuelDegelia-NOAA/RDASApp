! $Id: ropp_fm_compress_tl.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_tl&
(x, x_tl, z_geop, z_geop_tl,zcomp_dry_inv, zcomp_dry_inv_tl, zcomp_wet_inv, zcomp_wet_inv_tl)

!****s* Compressibility/ropp_fm_compress_tl *
!
! NAME
!    ropp_fm_compress_tl - Tangent linear of ropp_fm_compress.
!
! SYNOPSIS
!    call ropp_fm_compress_tl(x, x_tl, z_geop, z_geop_tl, zcomp_dry_inv, zcomp_dry_inv_tl,&
!                             zcomp_wet_inv, zcomp_wet_inv_tl)
!
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_compress.
!
! INPUTS
!    TYPE(State1dFM)   :: x              ! State vector
!    TYPE(State1dFM)   :: x_tl           ! Perturbation in x
!
! OUTPUT
!    REAL              :: z_geop            ! Adjusted geop height
!    REAL(wp)          :: z_geop_tl         ! Perturbation in adjusted geop height
!    REAL(wp)          :: zcomp_dry_inv     ! Inverse of dry comp
!    REAL(wp)          :: zcomp_dry_inv_tl  ! Perturbation in inverse of dry comp
!    REAL(wp)          :: zcomp_wet_inv     ! Inverse of wet comp
!    REAL(wp)          :: zcomp_wet_inv_tl  ! Perturbation in inverse of wet comp
!
! NOTES
!    Line-by-line differentiation of ropp_fm_compress.
!
! SEE ALSO
!    ropp_fm_compress
!    ropp_fm_compress_ad
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
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_fm, ONLY: ropp_fm_compress_single_tl
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),              INTENT(in)    :: x              ! State vector
  TYPE(State1dFM),              INTENT(in)    :: x_tl              ! State vector
  REAL(wp), DIMENSION(x%n_lev), INTENT(out)   :: z_geop         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev), INTENT(out)   :: z_geop_tl         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev), INTENT(out)   :: zcomp_dry_inv  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev), INTENT(out)   :: zcomp_dry_inv_tl  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev), INTENT(out)   :: zcomp_wet_inv  ! inverse of wet comp
  REAL(wp), DIMENSION(x%n_lev), INTENT(out)   :: zcomp_wet_inv_tl  ! inverse of wet comp

! local variables
  INTEGER :: i
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix,zcomp1,zcomp2,zcomp3
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix_tl,zcomp1_tl,zcomp2_tl,zcomp3_tl

!-----------------------------------------------------------------
! 1. Calulate the compressibilty on the model levels
!-----------------------------------------------------------------

  DO i = 1,x%n_lev

! Calculate tangent linear of compressibilities on ith model level
   CALL ropp_fm_compress_single_tl(x%temp(i), x%pres(i), x%shum(i),&
     x_tl%temp(i), x_tl%pres(i), x_tl%shum(i), zcomp_dry_inv(i), zcomp_dry_inv_tl(i),&
     zcomp_wet_inv(i), zcomp_wet_inv_tl(i), zcomp1=zcomp1(i), zcomp1_tl=zcomp1_tl(i),&
     zcomp2=zcomp2(i), zcomp2_tl=zcomp2_tl(i), zcomp3=zcomp3(i), zcomp3_tl=zcomp3_tl(i))

  ENDDO

!------------------------------------------------------------------
! 2. adjust the geopotential heights and invert the wet/dry compress.
!------------------------------------------------------------------

  z_geop(:) = 0.0_wp

  z_geop_tl(:) = 0.0_wp

  DO i = 1, x%n_lev

    IF (i == 1) THEN

      zcomp_mix(i) = zcomp1(i)

      zcomp_mix_tl(i) = zcomp1_tl(i)

      z_geop(i) = x%geop(i)*zcomp_mix(i)

      z_geop_tl(i) = x%geop(i)*zcomp_mix_tl(i) + &
                     zcomp_mix(i)*x_tl%geop(i)

    ELSE

! use the mean value of the i and (i-1) levels

      zcomp_mix(i) = 0.5_wp*(zcomp1(i)+zcomp1(i-1))

      zcomp_mix_tl(i) = 0.5_wp*(zcomp1_tl(i) + zcomp1_tl(i-1))

      z_geop(i) = z_geop(i-1)+zcomp_mix(i)*(x%geop(i)-x%geop(i-1))

      z_geop_tl(i) = z_geop_tl(i-1) + &
                     zcomp_mix_tl(i)*(x%geop(i)-x%geop(i-1)) + &
                     zcomp_mix(i)*(x_tl%geop(i)-x_tl%geop(i-1))

    ENDIF

  ENDDO


END SUBROUTINE ropp_fm_compress_tl


