! $Id: ropp_fm_compress_2d_ad.f90 2704 2011-02-23 14:21:33Z idculv $

SUBROUTINE ropp_fm_compress_2d_ad&
(x, x_ad, z_geop_ad,zcomp_dry_inv_ad,zcomp_wet_inv_ad)

!****s* Compressibility/ropp_fm_compress_2d_ad *
!
! NAME
!    ropp_fm_compress - .
!
! SYNOPSIS
!    call ropp_fm_compresss_2d_ad(x, x_ad, z_geop_ad, zcomp_dry_inv_ad, zcomp_wet_inv_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_compress_2d.
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
!    Line-by-line differentiation of ropp_fm_compress_2d.
!
! SEE ALSO
!    ropp_fm_compress_2d
!    ropp_fm_compress_2d_tl
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

  TYPE(State2dFM),              INTENT(in)      :: x              ! State vector
  TYPE(State2dFM),              INTENT(inout)   :: x_ad              ! State vector
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(inout)   :: z_geop_ad         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(inout)   :: zcomp_dry_inv_ad  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz), INTENT(inout)   :: zcomp_wet_inv_ad  ! inverse of wet comp


! local variables

  INTEGER :: i,j
  REAL(wp), DIMENSION(x%n_lev,x%n_horiz) :: z_geop         ! adjusted geop height
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_dry_inv  ! inverse of dry comp
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_wet_inv  ! inverse of wet comp
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix,zcomp1,zcomp2,zcomp3
  REAL(wp), DIMENSION(x%n_lev) :: zcomp_mix_ad,zcomp1_ad,zcomp2_ad,zcomp3_ad
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

!-----------------------------------------------------------------
! 1. Calulate the compressibilty on the model levels
!-----------------------------------------------------------------

zcomp_mix_ad(:) = 0.0_wp
zcomp1_ad(:) = 0.0_wp
zcomp2_ad(:) = 0.0_wp
zcomp3_ad(:) = 0.0_wp
zpd_ad   = 0.0_wp
zpwet_ad = 0.0_wp
ztc_ad   = 0.0_wp
zx_ad    = 0.0_wp
zpot_ad  = 0.0_wp
zterm1_ad= 0.0_wp
zterm2_ad= 0.0_wp
zterm3_ad= 0.0_wp

  DO j = 1,x%n_horiz

  DO i = 1,x%n_lev

! Temp in celcius
   
     ztc = x%temp(i,j) - 273.15_wp

! calculate the water vapour and dry pressure

     zpwet = x%pres(i,j) * x%shum(i,j) / (epsilon_water + (1.0_wp - epsilon_water)*x%shum(i,j))

! dry pressure

     zpd = x%pres(i,j) - zpwet

     zterm1 = za0+ztc*(za1+ztc*za2)

     zterm2 = zb0+zb1*ztc
   
     zterm3 = zc0+zc1*ztc
   
! compressibility of moist air
  
     zpot = x%pres(i,j)/x%temp(i,j)
  
     zx = zpwet/x%pres(i,j)
 
     zcomp1(i) = 1.0_wp - zpot*(zterm1 + zx*(zterm2 + zx*zterm3)) 
   
     zcomp1(i) = zcomp1(i) + zpot**2*(zd + zx**2*ze)
        
! compressibility for dry air
   
     zpot = zpd/x%temp(i,j)
   
     zcomp2(i) = 1.0_wp - zpot*(zterm1 - zpot*zd)
   
! compressibility of water vapour

     zpot = zpwet/x%temp(i,j)
   
     zcomp3(i) = 1.0_wp - zpot*(zterm1 + zterm2 + zterm3 - zpot*(zd+ze))
   
  ENDDO   

!------------------------------------------------------------------
! 2. adjust the geopotential heights and invert the wet/dry compress.
!------------------------------------------------------------------
  
  z_geop(:,j) = 0.0_wp
    
  DO i = 1, x%n_lev

    IF (i == 1) THEN
   
      zcomp_mix(i) = zcomp1(i)
   
      z_geop(i,j) = x%geop(i,j)*zcomp_mix(i)
   
    ELSE
  
! use the mean value of the i and (i-1) levels
   
      zcomp_mix(i) = 0.5_wp*(zcomp1(i)+zcomp1(i-1))
      
      z_geop(i,j) = z_geop(i-1,j)+zcomp_mix(i)*(x%geop(i,j)-x%geop(i-1,j))
            
    ENDIF    
               
! return the inverse of the compressibility for dry air + vapour 
! used to calculate the refractivity.  
   
    zcomp_dry_inv(i) = 1.0_wp/zcomp2(i)
   
    zcomp_wet_inv(i) = 1.0_wp/zcomp3(i)
   
  ENDDO   

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Ajoint!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! adjoint bit

  DO i = x%n_lev, 1, -1
    
    zcomp3_ad(i) = zcomp3_ad(i) -1.0_wp/zcomp3(i)**2*zcomp_wet_inv_ad(i,j)
  
    zcomp_wet_inv_ad(i,j) = 0.0_wp
  
    zcomp2_ad(i) = zcomp2_ad(i) - 1.0_wp/zcomp2(i)**2*zcomp_dry_inv_ad(i,j)
  
    zcomp_dry_inv_ad(i,j) = 0.0_wp

    IF (i == 1) THEN
     
        
      zcomp_mix_ad(i)=zcomp_mix_ad(i)+x%geop(i,j)*z_geop_ad(i,j)

      x_ad%geop(i,j) = x_ad%geop(i,j) +zcomp_mix(i)*z_geop_ad(i,j) 
           
      z_geop_ad(i,j) = 0.0_wp
      
      zcomp1_ad(i) = zcomp1_ad(i) + zcomp_mix_ad(i)
      
      zcomp_mix_ad(i) = 0.0_wp
        
    ELSE
  

! ad

      z_geop_ad(i-1,j) = z_geop_ad(i-1,j) + z_geop_ad(i,j)
      
      zcomp_mix_ad(i) = zcomp_mix_ad(i) + (x%geop(i,j)-x%geop(i-1,j))*z_geop_ad(i,j)
      
      x_ad%geop(i,j) = x_ad%geop(i,j) + zcomp_mix(i)*z_geop_ad(i,j)
      
      x_ad%geop(i-1,j) = x_ad%geop(i-1,j) - zcomp_mix(i)*z_geop_ad(i,j)
      
      z_geop_ad(i,j) = 0.0_wp
      
      
      zcomp1_ad(i) = zcomp1_ad(i) + 0.5_wp*zcomp_mix_ad(i)

      zcomp1_ad(i-1) = zcomp1_ad(i-1) + 0.5_wp*zcomp_mix_ad(i)

      zcomp_mix_ad(i) = 0.0_wp
      
           
    ENDIF    
     
  ENDDO
  
   DO i = x%n_lev,1,-1

   
!     zcomp1_ad(i)=0.0_wp
!     zcomp2_ad(i)=0.0_wp
!     zcomp3_ad(i)=0.0_wp


! Temp in celcius
   
     ztc = x%temp(i,j) - 273.15_wp


! calculate the water vapour and dry pressure

     zpwet = x%pres(i,j) * x%shum(i,j) / (epsilon_water + (1.0_wp - epsilon_water)*x%shum(i,j))

	     
! dry pressure

     zpd = x%pres(i,j) - zpwet

     zterm1 = za0+ztc*(za1+ztc*za2)     
     
     zterm2 = zb0+zb1*ztc
             
     zterm3 = zc0+zc1*ztc
        
! compressibility of water vapour

     zpot = zpwet/x%temp(i,j)
 
     zx = zpwet/x%pres(i,j)
          
     zpot_ad = zpot_ad - (zterm1 + zterm2 + zterm3 - 2.0_wp*zpot*(zd+ze))*zcomp3_ad(i) 

     zterm1_ad = zterm1_ad -zpot*zcomp3_ad(i) 
 
     zterm2_ad = zterm2_ad -zpot*zcomp3_ad(i) 
 
     zterm3_ad = zterm3_ad -zpot*zcomp3_ad(i) 

     zcomp3_ad(i) = 0.0_wp
     
     zpwet_ad = zpwet_ad + zpot_ad/x%temp(i,j)
     
     x_ad%temp(i,j) =  x_ad%temp(i,j) - zpot/x%temp(i,j)*zpot_ad
     
     zpot_ad = 0.0_wp

! dry air
     
     zpot = zpd/x%temp(i,j)
    
!!     zcomp2_tl(i) = - (zterm1 - 2.0_wp*zpot*zd)*zpot_tl  & 
!!                   & - zpot*zterm1_tl     
   
    
    
     zpot_ad = zpot_ad -(zterm1 - 2.0_wp*zpot*zd)*zcomp2_ad(i)
     zterm1_ad = zterm1_ad - zpot*zcomp2_ad(i)
     zcomp2_ad(i) = 0.0_wp
     
!!     zpot_tl = zpot/zpd*zpd_tl - zpot/x%temp(i)*x_tl%temp(i)
     
     zpd_ad = zpd_ad + zpot/zpd*zpot_ad          
     x_ad%temp(i,j) = x_ad%temp(i,j) - zpot/x%temp(i,j)*zpot_ad
     zpot_ad = 0.0_wp
    
! compressibility of moist air
  
     zpot = x%pres(i,j)/x%temp(i,j)
          
!     zcomp1_tl(i) = zcomp1_tl(i) + 2.0_wp*zpot*(zd + zx**2*ze)*zpot_tl + &
!                   & 2.0_wp*zpot**2*zx*ze*zx_tl   
   
      zpot_ad = zpot_ad + 2.0_wp*zpot*(zd + zx**2*ze)*zcomp1_ad(i)
      zx_ad = zx_ad + 2.0_wp*zpot**2*zx*ze*zcomp1_ad(i)
      zcomp1_ad(i) = zcomp1_ad(i)  
     
!     zcomp1_tl(i) = -(zterm1 + zx*(zterm2 + zx*zterm3))*zpot_tl  &
!                &   -zpot*(zterm1_tl + (zterm2 + 2.0_wp*zx*zterm3)*zx_tl  &
!                &   +zx*(zterm2_tl + zx*zterm3_tl))		   
      
      zpot_ad = zpot_ad - (zterm1 + zx*(zterm2 + zx*zterm3))* zcomp1_ad(i)
      zterm1_ad = zterm1_ad - zpot*zcomp1_ad(i)
      zx_ad = zx_ad - zpot*(zterm2 + 2.0_wp*zx*zterm3)*zcomp1_ad(i)
      zterm2_ad = zterm2_ad - zpot*zx*zcomp1_ad(i)
      zterm3_ad = zterm3_ad - zpot*zx**2*zcomp1_ad(i)
      zcomp1_ad(i)= 0.0_wp
      
      
!!      zpot = x%pres(i)/x%temp(i)
  
!!     zpot_tl = zpot/x%pres(i,j)*x_tl%pres(i,j) - zpot/x%temp(i,j)*x_tl%temp(i,j)
  
!!     zx = zpwet/x%pres(i,j)
     
!!     zx_tl = 1.0_wp/x%pres(i)*zpwet_tl - zx/x%pres(i)*x_tl%pres(i) 
     
     zpwet_ad = zpwet_ad + 1.0_wp/x%pres(i,j)*zx_ad 
     x_ad%pres(i,j) = x_ad%pres(i,j) - zx/x%pres(i,j)*zx_ad
     zx_ad = 0.0       
   
     x_ad%pres(i,j) = x_ad%pres(i,j) + zpot/x%pres(i,j)*zpot_ad 
     x_ad%temp(i,j) = x_ad%temp(i,j) - zpot/x%temp(i,j)*zpot_ad
     zpot_ad = 0.0_wp
   
   
!!   zterm3_prime = zc1*ztc_prime
   
     ztc_ad = ztc_ad + zc1*zterm3_ad
     zterm3_ad = 0.0_wp
   
!!   zterm2_prime = zb1*ztc_prime

   ztc_ad = ztc_ad + zb1*zterm2_ad 
   zterm2_ad = 0.0_wp
   
!!   zterm1_prime = (za1+2.0_wp*ztc*za2)*ztc_prime
  
   ztc = x%temp(i,j) - 273.15_wp
   ztc_ad = ztc_ad + (za1+2.0_wp*ztc*za2)*zterm1_ad
   zterm1_ad = 0.0_wp       
   
!!   zpd_prime = PRES_PRIME(i) - zpwet_prime 

   x_ad%pres(i,j) = x_ad%pres(i,j) + zpd_ad
   zpwet_ad = zpwet_ad - zpd_ad
   zpd_ad = 0.0_wp   
   
   
!!   zpwet_prime = zpwet/PRES(i)*PRES_PRIME(i) &
!!             & + PRES(i)*z_eps/(z_eps + (1.0_wp - z_eps)*PQ(i))**2*PQ_PRIME(i)   

   x_ad%pres(i,j) = x_ad%pres(i,j) + zpwet/x%pres(i,j)*zpwet_ad
   x_ad%shum(i,j) = x_ad%shum(i,j) +  &
   x%pres(i,j)*epsilon_water/( epsilon_water+ (1.0_wp - epsilon_water)*x%shum(i,j))**2*zpwet_ad
   zpwet_ad = 0.0_wp
   
   
!!   ztc_prime = PTEMP_PRIME(i)
     
   x_ad%temp(i,j) = x_ad%temp(i,j) + ztc_ad
   ztc_ad = 0.0_wp
   
   
  ENDDO   
 
  ENDDO ! j
 

END SUBROUTINE ropp_fm_compress_2d_ad


