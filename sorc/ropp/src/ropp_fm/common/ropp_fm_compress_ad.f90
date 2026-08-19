! $Id: ropp_fm_compress_ad.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_ad&
(x, x_ad, z_geop_ad, zcomp_dry_inv_ad, zcomp_wet_inv_ad)

!****s* Compressibility/ropp_fm_compress_ad *
!
! NAME
!    ropp_fm_compress_ad - Adjoint of ropp_fm_compress.
!
! SYNOPSIS
!    call ropp_fm_compress_ad(x, x_ad, z_geop_ad, zcomp_dry_inv_ad, zcomp_wet_inv_ad)
!
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_compress.
!
! INPUTS
!    TYPE(State1dFM)   :: x                   ! State vector
!    TYPE(State1dFM)   :: x_ad                ! Adjoint of x
!    REAL(wp)          :: z_geop_ad           ! Adjoint of geopotential
!    REAL(wp)          :: zcomp_dry_inv_ad    ! Adjoint of inverse dry compressibility
!    REAL(wp)          :: zcomp_wet_inv_ad    ! Adjoint of inverse wet compressibility
!
! OUTPUT
!    TYPE(State1dFM)   :: x_ad                ! Adjoint of x
!    REAL(wp)          :: z_geop_ad           ! Adjoint of geopotential
!    REAL(wp)          :: zcomp_dry_inv_ad    ! Adjoint of inverse dry compressibility
!    REAL(wp)          :: zcomp_wet_inv_ad    ! Adjoint of inverse wet compressibility
!
! NOTES
!    Line-by-line differentiation of ropp_fm_compress.
!
! SEE ALSO
!    ropp_fm_compress
!    ropp_fm_compress_tl
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
  USE ropp_fm, ONLY: ropp_fm_compress_single, ropp_fm_compress_single_ad
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),              INTENT(in)      :: x              ! State vector
  TYPE(State1dFM),              INTENT(inout)   :: x_ad              ! State vector
  REAL(wp), DIMENSION(x%n_lev), INTENT(inout)   :: z_geop_ad         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev), INTENT(inout)   :: zcomp_dry_inv_ad  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev), INTENT(inout)   :: zcomp_wet_inv_ad  ! inverse of wet comp

! local variables
  INTEGER :: i
  REAL(wp), DIMENSION(x%n_lev) :: z_geop         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_dry_inv  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_wet_inv  ! inverse of wet comp
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix,zcomp1,zcomp2,zcomp3
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix_ad,zcomp1_ad,zcomp2_ad,zcomp3_ad

!-----------------------------------------------------------------
! 1. Calulate the compressibilty on the model levels
!-----------------------------------------------------------------

  zcomp_mix_ad(:) = 0.0_wp
  zcomp1_ad(:) = 0.0_wp
  zcomp2_ad(:) = 0.0_wp
  zcomp3_ad(:) = 0.0_wp


  DO i = 1,x%n_lev

! Calculate compressibility factors on ith level
    CALL ropp_fm_compress_single(x%temp(i),x%pres(i),x%shum(i), zcomp_dry_inv(i),&
      zcomp_wet_inv(i), zcomp1=zcomp1(i), zcomp2=zcomp2(i), zcomp3=zcomp3(i))

  ENDDO

!------------------------------------------------------------------
! 2. adjust the geopotential heights and invert the wet/dry compress
!------------------------------------------------------------------

  z_geop(:) = 0.0_wp

  DO i = 1, x%n_lev

    IF (i == 1) THEN

      zcomp_mix(i) = zcomp1(i)

      z_geop(i) = x%geop(i)*zcomp_mix(i)

    ELSE

! use the mean value of the i and (i-1) levels

      zcomp_mix(i) = 0.5_wp*(zcomp1(i)+zcomp1(i-1))

      z_geop(i) = z_geop(i-1)+zcomp_mix(i)*(x%geop(i)-x%geop(i-1))

    ENDIF

  ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Adjoint!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! adjoint bit

  DO i = x%n_lev, 1, -1

    IF (i == 1) THEN

      zcomp_mix_ad(i)=zcomp_mix_ad(i)+x%geop(i)*z_geop_ad(i)

      x_ad%geop(i) = x_ad%geop(i) +zcomp_mix(i)*z_geop_ad(i) 

      z_geop_ad(i) = 0.0_wp


      zcomp1_ad(i) = zcomp1_ad(i) + zcomp_mix_ad(i)

      zcomp_mix_ad(i) = 0.0_wp

    ELSE

! ad

      z_geop_ad(i-1) = z_geop_ad(i-1) + z_geop_ad(i)

      zcomp_mix_ad(i) = zcomp_mix_ad(i) + (x%geop(i)-x%geop(i-1))*z_geop_ad(i)

      x_ad%geop(i) = x_ad%geop(i) + zcomp_mix(i)*z_geop_ad(i)

      x_ad%geop(i-1) = x_ad%geop(i-1) - zcomp_mix(i)*z_geop_ad(i)

      z_geop_ad(i) = 0.0_wp


      zcomp1_ad(i) = zcomp1_ad(i) + 0.5_wp*zcomp_mix_ad(i)

      zcomp1_ad(i-1) = zcomp1_ad(i-1) + 0.5_wp*zcomp_mix_ad(i)

      zcomp_mix_ad(i) = 0.0_wp

    ENDIF

  ENDDO

  DO i = x%n_lev,1,-1

! call adjoint of ropp_fm_compress_single_tl
    CALL ropp_fm_compress_single_ad(x%temp(i),x_ad%temp(i),x%pres(i),&
      x_ad%pres(i),x%shum(i),x_ad%shum(i),zcomp_dry_inv_ad(i),zcomp_wet_inv_ad(i),&
      zcomp1_opt_ad=zcomp1_ad(i),zcomp2_opt_ad=zcomp2_ad(i),zcomp3_opt_ad=zcomp3_ad(i))

  ENDDO


END SUBROUTINE ropp_fm_compress_ad


