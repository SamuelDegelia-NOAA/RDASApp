! $Id: ropp_fm_compress_single_ad.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_single_ad(temp,temp_ad,pres,pres_ad,shum,shum_ad,&
  zcomp_dry_inv_ad,zcomp_wet_inv_ad,zcomp1_opt_ad,zcomp2_opt_ad,zcomp3_opt_ad)

!****s* Compressibility/ropp_fm_compress_single_ad *
!
! NAME
!    ropp_fm_compress_single_ad - Adjoint of ropp_fm_compress_single.
!
! SYNOPSIS
!    call ropp_fm_compress_single_ad(temp,temp_ad,pres,pres_ad,shum,shum_ad,&
!         zcomp_dry_inv_ad,zcomp_wet_inv_ad,zcomp1_opt_ad,zcomp2_opt_ad,zcomp3_opt_ad)
!
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_compress_single.
!
! INPUTS
!    REAL(wp)          :: temp              ! Temperature
!    REAL(wp)          :: pres              ! Pressure
!    REAL(wp)          :: shum              ! Specific humidity
!    REAL(wp)          :: temp_ad           ! Temperature AD
!    REAL(wp)          :: pres_ad           ! Pressure AD
!    REAL(wp)          :: shum_ad           ! Specific humidity AD
!    REAL(wp)          :: zcomp_dry_inv_ad  ! Inverse of dry comp AD
!    REAL(wp)          :: zcomp_wet_inv_ad  ! Inverse of wet comp AD
!    REAL(wp),OPTIONAL :: zcomp1_opt_ad     ! Intermediate comp. factor 1 AD
!    REAL(wp),OPTIONAL :: zcomp2_opt_ad     ! Intermediate comp. factor 2 AD
!    REAL(wp),OPTIONAL :: zcomp3_opt_ad     ! Intermediate comp. factor 3 AD
!
! OUTPUT
!    REAL(wp)          :: temp_ad           ! Temperature AD
!    REAL(wp)          :: pres_ad           ! Pressure AD
!    REAL(wp)          :: shum_ad           ! Specific humidity AD
!    REAL(wp)          :: zcomp_dry_inv_ad  ! Inverse of dry comp AD
!    REAL(wp)          :: zcomp_wet_inv_ad  ! Inverse of wet comp AD
!    REAL(wp),OPTIONAL :: zcomp1_opt_ad     ! Intermediate comp. factor 1 AD
!    REAL(wp),OPTIONAL :: zcomp2_opt_ad     ! Intermediate comp. factor 2 AD
!    REAL(wp),OPTIONAL :: zcomp3_opt_ad     ! Intermediate comp. factor 3 AD
!
! NOTES
!    Line-by-line differentiation of ropp_fm_compress_single.
!
! SEE ALSO
!    ropp_fm_compress_single
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

  REAL(wp), INTENT(in)    :: temp               ! Temperature
  REAL(wp), INTENT(in)    :: pres               ! Pressure
  REAL(wp), INTENT(in)    :: shum               ! Specific humidity
  REAL(wp), INTENT(inout) :: temp_ad            ! Temperature AD
  REAL(wp), INTENT(inout) :: pres_ad            ! Pressure AD
  REAL(wp), INTENT(inout) :: shum_ad            ! Specific humidity AD
  REAL(wp), INTENT(inout)  :: zcomp_dry_inv_ad  ! inverse of dry comp
  REAL(wp), INTENT(inout)  :: zcomp_wet_inv_ad  ! inverse of wet comp
  REAL(wp), OPTIONAL, INTENT(inout) :: zcomp1_opt_ad !  {Intermediate
  REAL(wp), OPTIONAL, INTENT(inout) :: zcomp2_opt_ad !  compressibility
  REAL(wp), OPTIONAL, INTENT(inout) :: zcomp3_opt_ad !  factors AD}


  REAL(wp) :: zcomp1,zcomp2,zcomp3
  REAL(wp) :: zcomp1_ad,zcomp2_ad,zcomp3_ad
  REAL(wp) :: zpd,zpwet,ztc,zx,zpot
  REAL(wp) :: zpd_ad,zpwet_ad,ztc_ad,zx_ad,zpot_ad
  REAL(wp) :: zterm1,zterm2,zterm3
  REAL(wp) :: zterm1_ad,zterm2_ad,zterm3_ad

! parameters from the Davis paper

  REAL(wp), PARAMETER :: za0=1.58123E-6_wp
  REAL(wp), PARAMETER :: za1=-2.91331E-8_wp
  REAL(wp), PARAMETER :: za2=1.1043E-10_wp
  REAL(wp), PARAMETER :: zb0=5.707E-6_wp
  REAL(wp), PARAMETER :: zb1=-2.051E-8_wp
  REAL(wp), PARAMETER :: zc0=1.9898E-4_wp
  REAL(wp), PARAMETER :: zc1=-2.376E-6_wp
  REAL(wp), PARAMETER :: zd=1.83E-11_wp
  REAL(wp), PARAMETER :: ze=-0.765E-8_wp

! Handle optional arguments:
  IF (PRESENT(zcomp1_opt_ad) .AND. PRESENT(zcomp2_opt_ad) .AND. PRESENT(zcomp3_opt_ad)) THEN
    zcomp1_ad = zcomp1_opt_ad
    zcomp2_ad = zcomp2_opt_ad
    zcomp3_ad = zcomp3_opt_ad
  ELSE
    zcomp1_ad = 0.0_wp
    zcomp2_ad = 0.0_wp
    zcomp3_ad = 0.0_wp
  END IF

! Initialise adjoint variables
  zpd_ad   = 0.0_wp
  zpwet_ad = 0.0_wp
  ztc_ad   = 0.0_wp
  zx_ad    = 0.0_wp
  zpot_ad  = 0.0_wp
  zterm1_ad= 0.0_wp
  zterm2_ad= 0.0_wp
  zterm3_ad= 0.0_wp

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

  zcomp1 = 1.0_wp - zpot*(zterm1 + zx*(zterm2 + zx*zterm3)) 

  zcomp1 = zcomp1 + zpot**2*(zd + zx**2*ze)

! compressibility for dry air

  zpot = zpd/temp

  zcomp2 = 1.0_wp - zpot*(zterm1 - zpot*zd)

! compressibility of water vapour

  zpot = zpwet/temp

  zcomp3 = 1.0_wp - zpot*(zterm1 + zterm2 + zterm3 - zpot*(zd+ze))

  zcomp3_ad = zcomp3_ad -1.0_wp/zcomp3**2*zcomp_wet_inv_ad

  zcomp_wet_inv_ad = 0.0_wp

  zcomp2_ad = zcomp2_ad - 1.0_wp/zcomp2**2*zcomp_dry_inv_ad

  zcomp_dry_inv_ad = 0.0_wp

  zpot_ad = zpot_ad - (zterm1 + zterm2 + zterm3 - 2.0_wp*zpot*(zd+ze))*zcomp3_ad

  zterm1_ad = zterm1_ad -zpot*zcomp3_ad

  zterm2_ad = zterm2_ad -zpot*zcomp3_ad

  zterm3_ad = zterm3_ad -zpot*zcomp3_ad

  zcomp3_ad = 0.0_wp

  zpwet_ad = zpwet_ad + zpot_ad/temp

  temp_ad =  temp_ad - zpot/temp*zpot_ad

  zpot_ad = 0.0_wp

! dry air

  zpot = zpd/temp

! zcomp2_tl = - (zterm1 - 2.0_wp*zpot*zd)*zpot_tl  &
!               & - zpot*zterm1_tl

  zpot_ad = zpot_ad -(zterm1 - 2.0_wp*zpot*zd)*zcomp2_ad
  zterm1_ad = zterm1_ad - zpot*zcomp2_ad
  zcomp2_ad = 0.0_wp

!  zpot_tl = zpot/zpd*zpd_tl - zpot/temp*temp_tl

  zpd_ad = zpd_ad + zpot/zpd*zpot_ad
  temp_ad = temp_ad - zpot/temp*zpot_ad
  zpot_ad = 0.0_wp

! compressibility of moist air

  zpot = pres/temp

! zcomp1_tl = zcomp1_tl + 2.0_wp*zpot*(zd + zx**2*ze)*zpot_tl + &
!               & 2.0_wp*zpot**2*zx*ze*zx_tl

  zpot_ad = zpot_ad + 2.0_wp*zpot*(zd + zx**2*ze)*zcomp1_ad
  zx_ad = zx_ad + 2.0_wp*zpot**2*zx*ze*zcomp1_ad
  zcomp1_ad = zcomp1_ad

! zcomp1_tl = -(zterm1 + zx*(zterm2 + zx*zterm3))*zpot_tl  &
!            &   -zpot*(zterm1_tl + (zterm2 + 2.0_wp*zx*zterm3)*zx_tl  &
!            &   +zx*(zterm2_tl + zx*zterm3_tl))

  zpot_ad = zpot_ad - (zterm1 + zx*(zterm2 + zx*zterm3))* zcomp1_ad
  zterm1_ad = zterm1_ad - zpot*zcomp1_ad
  zx_ad = zx_ad - zpot*(zterm2 + 2.0_wp*zx*zterm3)*zcomp1_ad
  zterm2_ad = zterm2_ad - zpot*zx*zcomp1_ad
  zterm3_ad = zterm3_ad - zpot*zx**2*zcomp1_ad
  zcomp1_ad= 0.0_wp

! zpot = pres/temp

! zpot_tl = zpot/pres*pres_tl - zpot/temp*temp_tl

! zx = zpwet/pres

! zx_tl = 1.0_wp/pres*zpwet_tl - zx/pres*pres_tl

  zpwet_ad = zpwet_ad + 1.0_wp/pres*zx_ad
  pres_ad = pres_ad - zx/pres*zx_ad
  zx_ad = 0.0_wp

  pres_ad = pres_ad + zpot/pres*zpot_ad
  temp_ad = temp_ad - zpot/temp*zpot_ad
  zpot_ad = 0.0_wp

! zterm3_tl = zc1*ztc_tl

  ztc_ad = ztc_ad + zc1*zterm3_ad
  zterm3_ad = 0.0_wp

! zterm2_tl = zb1*ztc_tl

  ztc_ad = ztc_ad + zb1*zterm2_ad
  zterm2_ad = 0.0_wp

! zterm1_tl = (za1+2.0_wp*ztc*za2)*ztc_tl

  ztc = temp - 273.15_wp
  ztc_ad = ztc_ad + (za1+2.0_wp*ztc*za2)*zterm1_ad
  zterm1_ad = 0.0_wp

! zpd_tl = pres_tl - zpwet_tl

  pres_ad = pres_ad + zpd_ad
  zpwet_ad = zpwet_ad - zpd_ad
  zpd_ad = 0.0_wp

! zpwet_tl = zpwet/pres*pres_tl &
! & + pres*epsilon_water/(epsilon_water + (1.0_wp - epsilon_water)*shum)**2*shum_tl

  pres_ad = pres_ad + zpwet/pres*zpwet_ad
  shum_ad = shum_ad +  &
  pres*epsilon_water/( epsilon_water+ (1.0_wp - epsilon_water)*shum)**2*zpwet_ad
  zpwet_ad = 0.0_wp

!ztc_tl = temp_tl

  temp_ad = temp_ad + ztc_ad
  ztc_ad = 0.0_wp


END SUBROUTINE ropp_fm_compress_single_ad
