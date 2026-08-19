! $Id: ropp_fm_compress.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_single(temp,pres,shum, &
  zcomp_dry_inv, zcomp_wet_inv, zcomp1, zcomp2, zcomp3)

!****s* Compressibility/ropp_fm_compress_single *
!
! NAME
!    ropp_fm_compress_single - Calculate dry and wet compressibility
!                                    factors for a single set of model
!                                    variables
!
! SYNOPSIS
!    call ropp_fm_compresss_single(temp,pres,shum, &
!      zcomp_dry_inv, zcomp_wet_inv, zcomp1, zcomp2, zcomp3)
!
! DESCRIPTION
!    This routine calculates the inverses of the wet and dry compressibilities
!    and optionally returns the intermediate compressibility factors.
!
! INPUTS
!    REAL(wp)               :: temp            ! Model temperature
!    REAL(wp)               :: pres            ! Model pressure
!    REAL(wp)               :: shum            ! Model specific humidity
!
! OUTPUT
!    REAL(wp)               :: zcomp_dry_inv   ! Inverse of dry compressibility.
!    REAL(wp)               :: zcomp_wet_inv   ! Inverse of wet compressibility.
!    REAL(wp),OPTIONAL      :: zcomp1          ! Intermediate comp. factor 1.
!    REAL(wp),OPTIONAL      :: zcomp2          ! Intermediate comp. factor 2.
!    REAL(wp),OPTIONAL      :: zcomp3          ! Intermediate comp. factor 3.
!
! NOTES
!    Wet and dry compressibilities on a single model level calculated using 
!    known constants.
!    Wet and dry compressibilities returned and optionally the intermediate
!    factors for use in geopotential height adjustment calculations.
!
! SEE ALSO
!    ropp_fm_compress_single_ad
!    ropp_fm_compress_single_tl
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
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  REAL(wp), INTENT(in)  :: temp            ! Temperature
  REAL(wp), INTENT(in)  :: pres            ! Pressure
  REAL(wp), INTENT(in)  :: shum            ! Specific humidity
  REAL(wp), INTENT(out) :: zcomp_dry_inv   ! inverse of dry comp
  REAL(wp), INTENT(out) :: zcomp_wet_inv   ! inverse of wet comp
  REAL(wp),OPTIONAL, INTENT(out) :: zcomp1 ! Intermediate comp factor
  REAL(wp),OPTIONAL, INTENT(out) :: zcomp2 ! Intermediate comp factor
  REAL(wp),OPTIONAL, INTENT(out) :: zcomp3 ! Intermediate comp factor

! local variables

  REAL(wp) :: zpd,zpwet,ztc,zx,zpot
  REAL(wp) :: zterm1,zterm2,zterm3
  REAL(wp) :: zcomp1_dummy, zcomp2_dummy, zcomp3_dummy

! parameters from the Picard et al., 2008, Metrologia, vol 45. 149-155, Append A-2

  REAL(wp), PARAMETER :: za0=1.58123E-6_wp
  REAL(wp), PARAMETER :: za1=-2.91331E-8_wp
  REAL(wp), PARAMETER :: za2=1.1043E-10_wp
  REAL(wp), PARAMETER :: zb0=5.707E-6_wp
  REAL(wp), PARAMETER :: zb1=-2.051E-8_wp
  REAL(wp), PARAMETER :: zc0=1.9898E-4_wp
  REAL(wp), PARAMETER :: zc1=-2.376E-6_wp
  REAL(wp), PARAMETER :: zd=1.83E-11_wp
  REAL(wp), PARAMETER :: ze=-0.765E-8_wp

!-----------------------------------------------------------------
! 1. Calculate the compressibility on the model levels
!-----------------------------------------------------------------

! Temp in celsius

  ztc = temp - 273.15_wp

! calculate the water vapour and dry pressure

  zpwet = pres * shum / (epsilon_water + (1.0_wp - epsilon_water)*shum)

! dry pressure

  zpd = pres - zpwet

  zterm1 = za0+ztc*(za1+ztc*za2)

  zterm2 = zb0+zb1*ztc

  zterm3 = zc0+zc1*ztc

! compressibility of moist air

  zpot = pres/temp

  zx = zpwet/pres

  zcomp1_dummy = 1.0_wp - zpot*(zterm1 + zx*(zterm2 + zx*zterm3))

  zcomp1_dummy = zcomp1_dummy + zpot**2*(zd + zx**2*ze)

! compressibility for dry air

  zpot = zpd/temp

  zcomp2_dummy = 1.0_wp - zpot*(zterm1 - zpot*zd)

! compressibility of water vapour

  zpot = zpwet/temp

  zcomp3_dummy = 1.0_wp - zpot*(zterm1 + zterm2 + zterm3 - zpot*(zd+ze))

! compressibility factors

  zcomp_dry_inv = 1.0_wp/zcomp2_dummy

  zcomp_wet_inv = 1.0_wp/zcomp3_dummy

! optionally return intermediate factors
  IF (PRESENT(zcomp1) .AND. PRESENT(zcomp2) .AND. PRESENT (zcomp3)) THEN
    zcomp1=zcomp1_dummy
    zcomp2=zcomp2_dummy
    zcomp3=zcomp3_dummy
  END IF

END SUBROUTINE ropp_fm_compress_single


