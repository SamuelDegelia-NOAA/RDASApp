! $Id: ropp_fm_compress_2d_tl.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_2d_tl&
(x, x_tl, z_geop, z_geop_tl, zcomp_dry_inv, zcomp_dry_inv_tl, zcomp_wet_inv, zcomp_wet_inv_tl)

!****s* Compressibility/ropp_fm_compress_2d_tl *
!
! NAME
!    ropp_fm_compress_2d_tl - Tangent linear of ropp_fm_compress_2d.
!
! SYNOPSIS
!    call ropp_fm_compress_2d_tl(x, x_tl, z_geop, z_geop_tl, zcomp_dry_inv, zcomp_dry_inv_tl, zcomp_wet_inv, zcomp_wet_inv_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_compress_2d.
!
! INPUTS
!    TYPE(State1dFM)   :: x                 ! State vector
!    TYPE(State1dFM)   :: x_tl              ! Perturbation in x
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
!    Line-by-line differentiation of ropp_fm_compress_2d.
!
! SEE ALSO
!    ropp_fm_compress_2d
!    ropp_fm_compress_2d_ad
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

  TYPE(State2dFM),              INTENT(in)    :: x              ! State vector
  TYPE(State2dFM),              INTENT(in)    :: x_tl           ! State vector
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(out)   :: z_geop         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(out)   :: z_geop_tl         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(out)   :: zcomp_dry_inv  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(out)   :: zcomp_dry_inv_tl  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(out)   :: zcomp_wet_inv  ! inverse of wet comp
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(out)   :: zcomp_wet_inv_tl  ! inverse of wet comp


! local variables

  INTEGER :: i,j
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix,zcomp1,zcomp2,zcomp3
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix_tl,zcomp1_tl,zcomp2_tl,zcomp3_tl
  REAL(wp) :: zpd,zpwet,ztc,zx,zpot
  REAL(wp) :: zpd_tl,zpwet_tl,ztc_tl,zx_tl,zpot_tl
  REAL(wp) :: zterm1,zterm2,zterm3
  REAL(wp) :: zterm1_tl,zterm2_tl,zterm3_tl

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

  DO j = 1,x%n_horiz
  
  DO i = 1,x%n_lev

! Temp in celcius
   
     ztc = x%temp(i,j) - 273.15_wp

     ztc_tl = x_tl%temp(i,j) 

! calculate the water vapour and dry pressure

     zpwet = x%pres(i,j) * x%shum(i,j) / (epsilon_water + (1.0_wp - epsilon_water)*x%shum(i,j))

     zpwet_tl = zpwet/x%pres(i,j)*x_tl%pres(i,j) &
     & + x%pres(i,j)*epsilon_water/(epsilon_water + (1.0_wp - epsilon_water)*x%shum(i,j))**2*x_tl%shum(i,j)   
	     
! dry pressure

     zpd = x%pres(i,j) - zpwet

     zpd_tl = x_tl%pres(i,j) - zpwet_tl

     zterm1 = za0+ztc*(za1+ztc*za2)
     
     zterm1_tl = (za1+2.0_wp*ztc*za2)*ztc_tl
     
     zterm2 = zb0+zb1*ztc
     
     zterm2_tl = zb1*ztc_tl
       
     zterm3 = zc0+zc1*ztc
     
     zterm3_tl = zc1*ztc_tl

   
! compressibility of moist air
  
     zpot = x%pres(i,j)/x%temp(i,j)
  
     zpot_tl = zpot/x%pres(i,j)*x_tl%pres(i,j) - zpot/x%temp(i,j)*x_tl%temp(i,j)
  
     zx = zpwet/x%pres(i,j)
     
     zx_tl = 1.0_wp/x%pres(i,j)*zpwet_tl - zx/x%pres(i,j)*x_tl%pres(i,j) 
     
     zcomp1(i) = 1.0_wp - zpot*(zterm1 + zx*(zterm2 + zx*zterm3)) 
   
     zcomp1_tl(i) = -(zterm1 + zx*(zterm2 + zx*zterm3))*zpot_tl  &
                &   -zpot*(zterm1_tl + (zterm2 + 2.0_wp*zx*zterm3)*zx_tl  &
                &   +zx*(zterm2_tl + zx*zterm3_tl))		   
  
     zcomp1(i) = zcomp1(i) + zpot**2*(zd + zx**2*ze)
        
     
     zcomp1_tl(i) = zcomp1_tl(i) + 2.0_wp*zpot*(zd + zx**2*ze)*zpot_tl + &
                   & 2.0_wp*zpot**2*zx*ze*zx_tl   

! compressibility for dry air
   
     zpot = zpd/x%temp(i,j)
     
     zpot_tl = zpot/zpd*zpd_tl - zpot/x%temp(i,j)*x_tl%temp(i,j)
       
     zcomp2(i) = 1.0_wp - zpot*(zterm1 - zpot*zd)

     zcomp2_tl(i) = - (zterm1 - 2.0_wp*zpot*zd)*zpot_tl  & 
                   & - zpot*zterm1_tl     
   
! compressibility of water vapour

     zpot = zpwet/x%temp(i,j)
     
     zpot_tl = zpwet_tl/x%temp(i,j) - zpot/x%temp(i,j)*x_tl%temp(i,j)
   
     zcomp3(i) = 1.0_wp - zpot*(zterm1 + zterm2 + zterm3 - zpot*(zd+ze))
     
     zcomp3_tl(i) = -(zterm1 + zterm2 + zterm3 - 2.0_wp*zpot*(zd+ze))*zpot_tl &
                   & -zpot*(zterm1_tl + zterm2_tl + zterm3_tl)     


!     zcomp1(i)=1.0_wp
!     zcomp2(i)=1.0_wp
!     zcomp3(i)=1.0_wp

!     zcomp1_tl(i)=0.0_wp
!     zcomp2_tl(i)=0.0_wp
!     zcomp3_tl(i)=0.0_wp

   
  ENDDO   

!------------------------------------------------------------------
! 2. adjust the geopotential heights and invert the wet/dry compress.
!------------------------------------------------------------------
  
  z_geop(:,j) = 0.0_wp
  
  z_geop_tl(:,j) = 0.0_wp
    
  DO i = 1, x%n_lev

    IF (i == 1) THEN
   
      zcomp_mix(i) = zcomp1(i)
   
      zcomp_mix_tl(i) = zcomp1_tl(i)
   
      z_geop(i,j) = x%geop(i,j)*zcomp_mix(i)
      
      z_geop_tl(i,j) = x%geop(i,j)*zcomp_mix_tl(i) + &
                     zcomp_mix(i)*x_tl%geop(i,j)
   
    ELSE
  
! use the mean value of the i and (i-1) levels
   
      zcomp_mix(i) = 0.5_wp*(zcomp1(i)+zcomp1(i-1))

      zcomp_mix_tl(i) = 0.5_wp*(zcomp1_tl(i) + zcomp1_tl(i-1))
      
      z_geop(i,j) = z_geop(i-1,j)+zcomp_mix(i)*(x%geop(i,j)-x%geop(i-1,j))

      z_geop_tl(i,j) = z_geop_tl(i-1,j) + &
                     zcomp_mix_tl(i)*(x%geop(i,j)-x%geop(i-1,j)) + &
		     zcomp_mix(i)*(x_tl%geop(i,j)-x_tl%geop(i-1,j))

    ENDIF    
               
! return the inverse of the compressibility for dry air + vapour 
! used to calculate the refractivity.  
   
    zcomp_dry_inv(i,j) = 1.0_wp/zcomp2(i)
    
    zcomp_dry_inv_tl(i,j) = -1.0_wp/zcomp2(i)**2*zcomp2_tl(i)
    
    zcomp_wet_inv(i,j) = 1.0_wp/zcomp3(i)
    
    zcomp_wet_inv_tl(i,j) = -1.0_wp/zcomp3(i)**2*zcomp3_tl(i)
   
  ENDDO  
  
  
  ENDDO ! j

END SUBROUTINE ropp_fm_compress_2d_tl


