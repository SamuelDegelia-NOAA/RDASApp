! $Id: ropp_fm_compress_tl.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_single_tl(temp, pres, shum, temp_tl, pres_tl,&
  shum_tl, zcomp_dry_inv, zcomp_dry_inv_tl, zcomp_wet_inv, zcomp_wet_inv_tl,&
  zcomp1, zcomp1_tl, zcomp2, zcomp2_tl, zcomp3, zcomp3_tl)

!****s* Compressibility/ropp_fm_compress_single_tl *
!
! NAME
!    ropp_fm_compress_single_tl - Tangent linear of
!                                 ropp_fm_compress_single.
!
! SYNOPSIS
!    call ropp_fm_compress_single_tl(temp, pres, shum, temp_tl, pres_tl,&
!     shum_tl, zcomp_dry_inv, zcomp_dry_inv_tl, zcomp_wet_inv, zcomp_wet_inv_tl,&
!      zcomp1, zcomp1_tl, zcomp2, zcomp2_tl, zcomp3, zcomp3_tl)
!
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_compress_single.
!
! INPUTS
!    REAL(wp)          :: temp              ! Temperature
!    REAL(wp)          :: pres              ! Pressure
!    REAL(wp)          :: shum              ! Specific humidity
!    REAL(wp)          :: temp_tl           ! Temperature TL
!    REAL(wp)          :: pres_tl           ! Pressure TL
!    REAL(wp)          :: shum_tl           ! Specific humidity TL
!
! OUTPUT
!    REAL(wp)           :: zcomp_dry_inv     ! inverse of dry comp
!    REAL(wp)           :: zcomp_dry_inv_tl  ! inverse of dry comp TL
!    REAL(wp)           :: zcomp_wet_inv     ! inverse of wet comp
!    REAL(wp)           :: zcomp_wet_inv_tl  ! inverse of wet comp TL
!    REAL(wp),OPTIONAL  :: zcomp1            ! comp. factor 1
!    REAL(wp),OPTIONAL  :: zcomp1_tl         ! comp. factor 1 TL
!    REAL(wp),OPTIONAL  :: zcomp2            ! comp. factor 2
!    REAL(wp),OPTIONAL  :: zcomp2_tl         ! comp. factor 2 TL
!    REAL(wp),OPTIONAL  :: zcomp3            ! comp. factor 3
!    REAL(wp),OPTIONAL  :: zcomp3_tl         ! comp. factor 3 TL
!
! NOTES
!    Line-by-line differentiation of ropp_fm_compress_single.
!
! SEE ALSO
!    ropp_fm_compress_single
!    ropp_fm_compress_single_ad
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
  REAL(wp), INTENT(in)    :: temp_tl            ! Temperature TL
  REAL(wp), INTENT(in)    :: pres_tl            ! Pressure TL
  REAL(wp), INTENT(in)    :: shum_tl            ! Specific humidity TL
  REAL(wp), INTENT(out)   :: zcomp_dry_inv      ! inverse of dry comp
  REAL(wp), INTENT(out)   :: zcomp_dry_inv_tl   ! inverse of dry comp TL
  REAL(wp), INTENT(out)   :: zcomp_wet_inv      ! inverse of wet comp
  REAL(wp), INTENT(out)   :: zcomp_wet_inv_tl   ! inverse of wet comp TL
  REAL(wp),OPTIONAL, INTENT(out)   :: zcomp1    ! comp. factor 1
  REAL(wp),OPTIONAL, INTENT(out)   :: zcomp1_tl ! comp. factor 1 TL
  REAL(wp),OPTIONAL, INTENT(out)   :: zcomp2    ! comp. factor 2
  REAL(wp),OPTIONAL, INTENT(out)   :: zcomp2_tl ! comp. factor 2 TL
  REAL(wp),OPTIONAL, INTENT(out)   :: zcomp3    ! comp. factor 3
  REAL(wp),OPTIONAL, INTENT(out)   :: zcomp3_tl ! comp. factor 3 TL

! local variables

  REAL(wp) :: zpd,zpwet,ztc,zx,zpot
  REAL(wp) :: zpd_tl,zpwet_tl,ztc_tl,zx_tl,zpot_tl
  REAL(wp) :: zterm1,zterm2,zterm3
  REAL(wp) :: zterm1_tl,zterm2_tl,zterm3_tl

  REAL(wp) :: zcomp1_dummy, zcomp1_dummy_tl
  REAL(wp) :: zcomp2_dummy, zcomp2_dummy_tl
  REAL(wp) :: zcomp3_dummy, zcomp3_dummy_tl

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

!-----------------------------------------------------------------
! 1. Calulate the compressibilty on the model levels
!-----------------------------------------------------------------


! Temp in celsius

  ztc = temp - 273.15_wp

  ztc_tl = temp_tl

! calculate the water vapour and dry pressure

  zpwet = pres * shum / (epsilon_water + (1.0_wp - epsilon_water)*shum)

  zpwet_tl = zpwet/pres*pres_tl &
    & + pres*epsilon_water/(epsilon_water + (1.0_wp - epsilon_water)*shum)**2*shum_tl

! dry pressure

  zpd = pres - zpwet

  zpd_tl = pres_tl - zpwet_tl

  zterm1 = za0+ztc*(za1+ztc*za2)

  zterm1_tl = (za1+2.0_wp*ztc*za2)*ztc_tl

  zterm2 = zb0+zb1*ztc

  zterm2_tl = zb1*ztc_tl

  zterm3 = zc0+zc1*ztc

  zterm3_tl = zc1*ztc_tl


! compressibility of moist air

  zpot = pres/temp

  zpot_tl = zpot/pres*pres_tl - zpot/temp*temp_tl

  zx = zpwet/pres

  zx_tl = 1.0_wp/pres*zpwet_tl - zx/pres*pres_tl

  zcomp1_dummy = 1.0_wp - zpot*(zterm1 + zx*(zterm2 + zx*zterm3)) 

  zcomp1_dummy_tl = -(zterm1 + zx*(zterm2 + zx*zterm3))*zpot_tl  &
             &   -zpot*(zterm1_tl + (zterm2 + 2.0_wp*zx*zterm3)*zx_tl  &
             &   +zx*(zterm2_tl + zx*zterm3_tl))

  zcomp1_dummy = zcomp1_dummy + zpot**2*(zd + zx**2*ze)

  zcomp1_dummy_tl = zcomp1_dummy_tl + 2.0_wp*zpot*(zd + zx**2*ze)*zpot_tl + &
                & 2.0_wp*zpot**2*zx*ze*zx_tl


! compressibility for dry air

  zpot = zpd/temp

  zpot_tl = zpot/zpd*zpd_tl - zpot/temp*temp_tl

  zcomp2_dummy = 1.0_wp - zpot*(zterm1 - zpot*zd)

  zcomp2_dummy_tl = - (zterm1 - 2.0_wp*zpot*zd)*zpot_tl  &
                & - zpot*zterm1_tl

! compressibility of water vapour

  zpot = zpwet/temp

  zpot_tl = zpwet_tl/temp - zpot/temp*temp_tl

  zcomp3_dummy = 1.0_wp - zpot*(zterm1 + zterm2 + zterm3 - zpot*(zd+ze))

  zcomp3_dummy_tl = -(zterm1 + zterm2 + zterm3 - 2.0_wp*zpot*(zd+ze))*zpot_tl &
                & -zpot*(zterm1_tl + zterm2_tl + zterm3_tl)


! return the inverse of the compressibility for dry air + vapour 
! used to calculate the refractivity.  

  zcomp_dry_inv = 1.0_wp/zcomp2_dummy

  zcomp_dry_inv_tl = -1.0_wp/zcomp2_dummy**2*zcomp2_dummy_tl

  zcomp_wet_inv = 1.0_wp/zcomp3_dummy

  zcomp_wet_inv_tl = -1.0_wp/zcomp3_dummy**2*zcomp3_dummy_tl

  IF (PRESENT(zcomp1) .AND. PRESENT(zcomp1_tl) .AND. PRESENT(zcomp2) .AND. &
      PRESENT(zcomp2_tl) .AND. PRESENT(zcomp3) .AND. PRESENT(zcomp3_tl)) THEN
    zcomp1=zcomp1_dummy
    zcomp1_tl=zcomp1_dummy_tl
    zcomp2=zcomp2_dummy
    zcomp2_tl=zcomp2_dummy_tl
    zcomp3=zcomp3_dummy
    zcomp3_tl=zcomp3_dummy_tl
  END IF


END SUBROUTINE ropp_fm_compress_single_tl


