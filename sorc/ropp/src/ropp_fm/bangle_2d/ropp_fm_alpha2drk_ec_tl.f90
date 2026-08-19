! $id$

!****s* bendingangle2d/ropp_fm_alpha2drk_ec_tl *
!
! name
!    ropp_fm_alpha2drk - forward model calculating a bending
!                        angle profile from planar information.
!
! synopsis
!    call ropp_fm_alpha2drk_ec_tl(kobs, klev, ...)
! 
! description
!    this routine is a forward model calculating the bending angle profile
!    from planar refractivity information.  
!
! inputs
!
!           kobs   =  number of observed bending angles 
!           klev   =  number of vertical levels
!           khoriz =  number of horizontal locations
!           ksplit =  splitting of model levels
!           pdsep  =  angular spacing
!           pa     =  impact PARAMETERs
!           proc   =  radius of curvature 
!           pz_2d  =  2d impact height (do a 1d calculation above pz_2d)
!           prefrac=  refractivity values
!           pradius=  radius values
!           pnr    =  nr product 
!
! output
! 
!           pa_path = impact PARAMETER at end points of ray path
!           palpha  = bending angle values 
!           pa_path_prime = tl of impact PARAMETER at end points of ray path
!           palpha_prime  = tl of bending angle values 
!
! notes
!    the forward model calculate the bending angle as a function of
!    impact PARAMETER. below "pz_2d" a runge-kutta solver is used
!    calculate the ray-path and bending angle. above "pz_2d" we use
!    the 1d method, based on the error function solution of the 
!    bending angle integral.  
!
! see also
!    ropp_fm_types
!
! author
!   ecmwf, uk.
!   any comments on this software should be given via the rom saf
!   helpdesk at http://www.romsaf.org
!
! copyright
!   (c) eumetsat. all rights reserved.
!   for further details please refer to the file copyright
!   which you should have received as part of this distribution.
!
!****
!**************************************************************

SUBROUTINE ropp_fm_alpha2drk_ec_tl(kobs,    & ! no.of observations
                             & klev,    & ! no. of vertical levels
                             & khoriz,  & ! no. of horizontal layers  odd
                             & ksplit,  &
                             & pdsep,   & ! the angular spacing 
                             & pa,      & ! impact PARAMETER values
                             & prefrac, & ! refractivity
                             & prefrac_prime, &
                             & pradius, & ! radius values
                             & pradius_prime, &
                             & pnr,     &
                             & pnr_prime, &
                             & proc, &
                             & pz_2d, &
		             & pa_path, &
                             & pa_path_prime, &
		             & palpha, &
                             & palpha_prime) ! partial path length along rays



USE typesizes, ONLY: wp => EightByteREAL
USE ropp_utils, ONLY: ropp_MDFV
USE ropp_fm_constants, ONLY : pi
USE ropp_fm, not_this => ropp_fm_alpha2drk_ec_tl


IMPLICIT NONE

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

INTEGER, INTENT(IN)  :: kobs           ! size of ob. vector
INTEGER, INTENT(IN)  :: klev           ! no. of refractivity levels
INTEGER, INTENT(IN)  :: khoriz         ! no. of horizontal locations
INTEGER, INTENT(IN)  :: ksplit
REAL(wp),    INTENT(IN)  :: pdsep           ! angular spacing of grid
REAL(wp),    INTENT(IN)  :: pa(kobs)        ! impact PARAMETER - now assumed to be on pressure levels
REAL(wp),    INTENT(IN)  :: prefrac(klev,khoriz)   ! refractivity values on levels
REAL(wp),    INTENT(IN)  :: prefrac_prime(klev,khoriz)   ! refractivity values on levels
REAL(wp),    INTENT(IN)  :: pradius(klev,khoriz)   ! radius values
REAL(wp),    INTENT(IN)  :: pradius_prime(klev,khoriz)   ! radius values
REAL(wp),    INTENT(IN)  :: pnr(klev,khoriz)
REAL(wp),    INTENT(IN)  :: pnr_prime(klev,khoriz)
REAL(wp),    INTENT(IN)  :: proc
REAL(wp),    INTENT(IN)  :: pz_2d
REAL(wp),    INTENT(OUT) :: pa_path_prime(kobs,2)        
REAL(wp),    INTENT(OUT) :: palpha_prime(kobs)   ! path length
REAL(wp),    INTENT(OUT) :: palpha(kobs)   ! path length
REAL(wp),    INTENT(OUT) :: pa_path(kobs,2)        
                       

! local variables


INTEGER :: i,j,in,ibot,jbot,ikbot,iside,ik,ikp1,in_2d,jj,isplit,i_below
INTEGER :: ikcen
REAL(wp), PARAMETER :: zhmax = 5.0e4_wp
REAL(wp), PARAMETER :: zhmin = 1.0e2_wp
REAL(wp), PARAMETER :: dn_dx_max = 0.157_wp ! setting the maxiumin N gradients
REAL(wp) :: zrad,zdndr
REAL(wp) :: zrad_prime,zdndr_prime
REAL(wp) :: zhwt1,zhwt2
REAL(wp) :: zhwt1_prime,zhwt2_prime
REAL(wp) :: zamult
REAL(wp) :: zh,zh2,zhuse,zhnew,zh_up
REAL(wp) :: zh_prime,zh2_prime,zhuse_prime,zhnew_prime,zh_up_prime
REAL(wp) :: zy(4),zyt(4)
REAL(wp) :: zy_prime(4),zyt_prime(4)
REAL(wp) :: zdydh(4)
REAL(wp) :: zdydh_prime(4)
REAL(wp) :: ztheta_tan,ztheta_min,ztheta_max
REAL(wp) :: zdr_max,zdr_dtheta,zrtan,zdr
REAL(wp) :: zdr_max_prime,zdr_dtheta_prime,zrtan_prime,zdr_prime
REAL(wp) :: palpha_half(2)
REAL(wp) :: palpha_half_prime(2)
REAL(wp) :: zkval(klev-1,khoriz)
REAL(wp) :: zkval_prime(klev-1,khoriz)
REAL(wp) :: ztlow,ztup,zdalpha,zroot_halfpi
REAL(wp) :: ztlow_prime,ztup_prime,zdalpha_prime
REAL(wp) :: zerf_up,zerf_low,zt,zdiff_erf,znr_low,zref_low,zaval
REAL(wp) :: zerf_up_prime,zerf_low_prime,zt_prime,zdiff_erf_prime,znr_low_prime
REAL(wp) :: zref_low_prime,zaval_prime
REAL(wp) :: zrad_up,zrad_low,zref_up,zkval_theta,zdndr2,zdn_dx,zed
REAL(wp) :: zrad_up_prime,zrad_low_prime,zref_up_prime,zkval_theta_prime,zdndr2_prime,zdn_dx_prime,zed_prime
logical :: llfirst_1d,lleaving,llone_d_calc,ll_intercept         


!-------------------------------------------------------------------------------
! 2. Set up the central profile kcen 
!-------------------------------------------------------------------------------

ikcen = khoriz/2 + 1
ztheta_tan = REAL(ikcen-1)*pdsep 
ztheta_min = -ztheta_tan
ztheta_max =  ztheta_tan


! set the kvals used in the 1d calculation

zkval(:,:) = 1.5e-4_wp ! climatological value 
zkval_prime(:,:) = 0.0_wp


do i = 1,klev-1

   do j = 1, khoriz
     
     if (prefrac(i,j) > 0.0_wp .and. prefrac(i+1,j) > 0.0_wp) then 
   
       zkval(i,j) = LOG(prefrac(i,j)/prefrac(i+1,j))/MAX((pnr(i+1,j) - pnr(i,j)),1.0_wp)
      
       if (zkval(i,j) > 1.0e-6_wp) then
      
        zkval_prime(i,j) = ((zkval(i,j)*(pnr_prime(i,j)-pnr_prime(i+1,j))) + &
                  & (prefrac_prime(i,j)/prefrac(i,j)-              &
                  &  prefrac_prime(i+1,j)/prefrac(i+1,j)))/        &
                  &  MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j)))
                    
       else
      
        zkval(i,j)=1.0e-6_wp
        zkval_prime(i,j) = 0.0_wp
            
       endif        
      
! limit the maximum gradient      
      
       if (zkval(i,j) > (dn_dx_max/prefrac(i,j))) then
      
          zkval(i,j) = dn_dx_max/prefrac(i,j)
          zkval_prime(i,j) = - zkval(i,j)/prefrac(i,j)*prefrac_prime(i,j)
      
       endif 
      
    endif        
            
  enddo
   
enddo   



! set n_2d level. for levels below n_2d we do 2d ray bending calculation
! above n_2d we do the 1d calculation


in_2d = 0 

do while ((pnr(in_2d+1,ikcen)-proc < pz_2d) .and. (in_2d < klev - 1)) 

    in_2d = in_2d + 1
    
enddo    

jbot = 1

do

  if (prefrac(jbot,ikcen) > 0.0_wp .and. pnr(jbot,ikcen) > 0.0_wp) exit
  
  jbot = jbot + 1

enddo

ikbot = klev

do i=klev,jbot+1,-1

   if ((pnr(ikbot,ikcen) - pnr(ikbot-1,ikcen)) < 1.0_wp ) exit 

   ikbot = ikbot - 1

enddo
 
jbot = MAX(jbot,ikbot)



! set the outputs to missing


palpha(:)= ropp_MDFV
palpha_prime(:) =  0.0_wp !ropp_MDFV
pa_path(:,:)=  ropp_MDFV
pa_path_prime(:,:) = 0.0_wp


zroot_halfpi = SQRT(0.5_wp*pi)

obloop: do in=1,kobs
   
   if (pa(in) < pnr(jbot,ikcen) .or. pa(in) > pnr(klev-3,ikcen)) cycle  
      
   ibot = jbot

   do 

      if (pnr(ibot+1,ikcen) - pa(in) > 1.0_wp) exit   ! assuming "a" is on one of the pressure levels

      ibot=ibot+1

   enddo


! calculate the radius at tangent point   

   zrad = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))   
   zrad_prime = 0.5_wp*(pradius_prime(ibot,ikcen)+pradius_prime(ibot+1,ikcen))
   
   zdndr = 1.0e-6_wp*(prefrac(ibot+1,ikcen)-prefrac(ibot,ikcen))/ &
                & (pradius(ibot+1,ikcen)-pradius(ibot,ikcen)) 
  
   zdndr_prime = (1.0e-6_wp*(prefrac_prime(ibot+1,ikcen)-prefrac_prime(ibot,ikcen)) -  &
 & zdndr*(pradius_prime(ibot+1,ikcen)-pradius_prime(ibot,ikcen)))/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))


 
   if ( zrad*zdndr > -1.0_wp) then
  
       zrtan = pradius(ibot,ikcen) + &
           &  (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad*zdndr)


       zrtan_prime = pradius_prime(ibot,ikcen) - &
                 &   (pnr_prime(ibot,ikcen) + &
                 &   (pa(in)-pnr(ibot,ikcen))/ &
                 &    (1.0_wp + zrad*zdndr)*(zrad*zdndr_prime + zdndr*zrad_prime))/&  
                 &   (1.0_wp + zrad*zdndr)   

   else
   
       zrtan = zrad   ! probably in a super-refracting layer
       
       zrtan_prime = zrad_prime
       
       
   endif                

! if zrtan is within a 1 m of upper level set to upper level
   
   if ((zrtan - pradius(ibot+1,ikcen)) > -1.0_wp) then
   
       ibot = ibot + 1
   
       zrtan = pradius(ibot,ikcen)
       
       zrtan_prime = pradius_prime(ibot,ikcen) 
              
   endif     

! don't calculate ba's that may hit orography 
        
   i_below = 0
   
   do i = ikcen-1, ikcen+1
   
      if (prefrac(ibot,i) < 0.0_wp .or.prefrac(ibot,i) == ropp_MDFV) i_below = i_below + 1
      
   enddo
   
   if (i_below /= 0) cycle    
                       


!  set bending angle value  
   

   palpha_half(:) = 0.0_wp   
   palpha_half_prime(:) = 0.0_wp
   
   ll_intercept = .false.

   do iside = 1,2 
   
 
      pa_path(in,iside) = pa(in)
      pa_path_prime(in,iside) = 0.0_wp
      llfirst_1d = .true.
      llone_d_calc = .false.        
      zamult = 1.0_wp
      if (iside == 2) zamult  = -1.0_wp
      

! initialise vector


      zy(1) = 0.0_wp           ! height above tangent point           
      zy(2) = 0.0_wp           ! theta
      zy(3) = ASIN(1.0_wp)     ! thi
      zy(4) = 0.0_wp           ! bending angle 
      
      
      zy_prime(:) = 0.0_wp
      
        
      do i = ibot,klev-1
        
        
         if ( i < MIN(in_2d,klev-1)) then

! set the set the splitting
  
             isplit = ksplit

! smaller steps near tangent point.

                if (i - ibot < 2) isplit = 2*ksplit
        
             if (i == ibot) then
         
                ik = ikcen
                zdr_max = (pradius(i+1,ik)- zrtan)/REAL(isplit)                        
                zdr_max_prime = (pradius_prime(i+1,ik)-zrtan_prime)/REAL(isplit)
                zh = SQRT(2.0_wp*pradius(ibot,ik)*zdr_max)                    
                zh_prime = &
             &  (pradius(ibot,ik)*zdr_max_prime + zdr_max*pradius_prime(ibot,ik))/zh
                        
             else

                ik = int((zy(2) + ztheta_tan)/pdsep)+1
                ik = MIN(MAX(1,ik),khoriz)
                

                if ((prefrac(i,ik) < 0.0_wp)   .or.&
                &    prefrac(i,ik) == ropp_MDFV) then
       
                   ll_intercept = .true.
       
                else

    
                zdr_max = (pradius(i+1,ik)-pradius(i,ik))/REAL(isplit)
                zdr_max_prime = (pradius_prime(i+1,ik)-pradius_prime(i,ik))/REAL(isplit)
            
            
                zh_up = SQRT(2.0_wp*pradius(i,ik)*zdr_max)
                zh_up_prime = &
             &  (pradius(i,ik)*zdr_max_prime + zdr_max*pradius_prime(i,ik))/zh_up
                          
                zh = zdr_max/MAX(COS(zy(3)),1.0e-10_wp)
            

                if (COS(zy(3)) < 1.0e-10_wp) then
            
                   zh_prime = 1.0e10_wp*zdr_max_prime
                
                else 
            
                   zh_prime = zh/zdr_max*zdr_max_prime + zh*TAN(zy(3))*zy_prime(3)           
               
                endif 

! use zh_up when COS(phi) cose to 0.0
              
                if (zh > zh_up) then
              
                    zh = zh_up
                    zh_prime = zh_up_prime
                  
                endif  
               
                endif  

             endif


             if (ll_intercept) exit 


! limit the step-length

             if (zh > zhmax) then
         
               zh = zhmax
               zh_prime = 0.0_wp
            
             elseif (zh < zhmin) then
         
               zh = zhmin    
               zh_prime = 0.0_wp
         
             endif 


             zh2 = 0.5_wp*zh         
             zh2_prime = 0.5_wp*zh_prime
         

! now calculate the path-length with a runge-kutta


             lleaving=.false.
         
             do j = 1,isplit          
            
                do jj = 1, 2
    
                   zyt(:) = zy(:)            
                   zyt_prime(:) = zy_prime(:)  
                      
                   if (jj == 2) then

                      zyt(:) = zyt(:) + zdydh(:)*zh2
                      zyt_prime(:) = zyt_prime(:) + zdydh(:)*zh2_prime + zdydh_prime(:)*zh2
            
                   endif


! where are we in the plane    
                   
                   ik = int((zyt(2) + ztheta_tan)/pdsep)+1
                   ik = MIN(MAX(1,ik),khoriz-1)
                   ikp1 = ik+1


! intercepted the orography

                   if ((prefrac(i,ik) < 0.0_wp)   .or.&
                   &   (prefrac(i,ikp1) < 0.0_wp) .or. &
                   &    prefrac(i,ik) == ropp_MDFV       .or. &
                   &    prefrac(i,ikp1) == ropp_MDFV ) then 

                  ! intercepted the orography - exit

                      ll_intercept = .true.
   
                   endif 

 
                   if (ll_intercept) exit  ! jj loop 
         
! horizontal weighting factor               
            
                   if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
                       zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep  
                       zhwt1_prime = - zyt_prime(2)/pdsep
                
                       zhwt2 = 1.0_wp - zhwt1
                       zhwt2_prime = - zhwt1_prime            
                   
                   elseif (zyt(2) < ztheta_min) then
            
                       zhwt1 = 1.0_wp
                       zhwt2 = 0.0_wp
       
                       zhwt1_prime = 0.0_wp
                       zhwt2_prime = 0.0_wp
              
                   elseif (zyt(2) > ztheta_max) then
                       
                       zhwt1 = 0.0_wp
                       zhwt2 = 1.0_wp
       
                       zhwt1_prime = 0.0_wp
                       zhwt2_prime = 0.0_wp
              
                   endif    


!   
! calculate the gradients at that point
!

                   zdydh(1) = MAX(1.0e-10_wp,COS(zyt(3)))
   
                   if (COS(zyt(3)) > 1.0e-10_wp) then
   
                      zdydh_prime(1) = -SIN(zyt(3))*zyt_prime(3)
      
                   else
      
                      zdydh_prime(1) = 0.0_wp
             
                   endif        

                   zdydh(2) = zamult*SIN(zyt(3))/(zyt(1)+zrtan)

                   zdydh_prime(2) = (zamult*COS(zyt(3))*zyt_prime(3)-zdydh(2)* &
              &    (zyt_prime(1) + zrtan_prime))/(zyt(1)+zrtan)
   

!
! interpolate refractivity and radius to point in plane.
!
     
                   zref_up  = zhwt1*prefrac(i+1,ik)+zhwt2*prefrac(i+1,ikp1)

                   zref_up_prime = zhwt1*prefrac_prime(i+1,ik) + &
              &                    prefrac(i+1,ik)*zhwt1_prime + &
              &                    zhwt2*prefrac_prime(i+1,ikp1) + &
              &                    prefrac(i+1,ikp1)*zhwt2_prime 

                   zref_low = zhwt1*prefrac(i,ik) + zhwt2*prefrac(i,ikp1)


                   zref_low_prime = zhwt1*prefrac_prime(i,ik) + &
               &                    prefrac(i,ik)*zhwt1_prime + &
               &                    zhwt2*prefrac_prime(i,ikp1) + &
               &                    prefrac(i,ikp1)*zhwt2_prime 


                   zrad_up = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)

                   zrad_up_prime = zhwt1*pradius_prime(i+1,ik) + &
              &                    pradius(i+1,ik)*zhwt1_prime + &
              &                    zhwt2*pradius_prime(i+1,ikp1) + &
              &                    pradius(i+1,ikp1)*zhwt2_prime 



                   zrad_low = zhwt1*pradius(i,ik) + zhwt2*pradius(i,ikp1)

                   zrad_low_prime = zhwt1*pradius_prime(i,ik) + &
               &                    pradius(i,ik)*zhwt1_prime + &
               &                    zhwt2*pradius_prime(i,ikp1) + &
               &                    pradius(i,ikp1)*zhwt2_prime 




! estimate radial gradient
    
                   if ((zref_up - zref_low) > -1.0e-10_wp) then
      
                      zdndr2 = 1.0e-6_wp*(zref_up-zref_low)/(zrad_up-zrad_low)  ! +ve refrac gradient
       
                      zdndr2_prime = 1.0e-6_wp*(zref_up_prime-zref_low_prime) &
                    & /(zrad_up-zrad_low) - &
                    & zdndr2/(zrad_up-zrad_low)*(zrad_up_prime-zrad_low_prime)

                   else
    
                       zkval_theta = LOG(zref_low/zref_up)/(zrad_up-zrad_low)

                       zkval_theta_prime = (zref_low_prime/zref_low - zref_up_prime/zref_up + &
            &          zkval_theta*(zrad_low_prime - zrad_up_prime))/(zrad_up-zrad_low)

                       zed = MAX(0.0_wp,(zyt(1)+zrtan-zrad_low))

                       if ((zyt(1)+zrtan-zrad_low) >  0.0_wp) then

                           zed_prime = zyt_prime(1) + zrtan_prime - zrad_low_prime
   
                       else   

                           zed_prime = 0.0_wp

                       endif 
          
                       zdndr = - 1.0e-6_wp*zkval_theta*zref_low*EXP(-zkval_theta*zed)

                       zdndr_prime = zdndr*((1.0_wp/zkval_theta - zed)*zkval_theta_prime + &
                     & zref_low_prime/zref_low - &
                     & zkval_theta*zed_prime)

                       zdndr2 = MAX(-0.75e-7_wp,zdndr)

                        
                       if (zdndr > -0.75e-7_wp) then

                          zdndr2_prime = zdndr_prime

                       else

                          zdndr2_prime = 0.0_wp

                       endif

  
                   endif  


                   zdydh(3) = -SIN(zyt(3))*(1.0_wp/(zyt(1)+zrtan) + zdndr2)

                   zdydh_prime(3)  = - COS(zyt(3))*(1.0_wp/(zyt(1)+zrtan) + zdndr2)*zyt_prime(3) - &
                &  SIN(zyt(3))*(zdndr2_prime - 1.0_wp/(zyt(1)+zrtan)**2* &
                &  (zyt_prime(1) + zrtan_prime))
                   
   
                   zdydh(4) = - SIN(zyt(3))*zdndr2
                   zdydh_prime(4)  = - COS(zyt(3))*zdndr2*zyt_prime(3) - SIN(zyt(3))*zdndr2_prime               
       
              enddo  ! jj

              if (ll_intercept) exit 
  
! update with latest estimate of gradient            
    
              zyt(:) = zy(:) + zdydh(:)*zh

              zyt_prime(:) = zy_prime(:) +  zdydh_prime(:)*zh +  zdydh(:)*zh_prime 


! check the radius - have we exited the level
            
            
              ik = int((zyt(2) + ztheta_tan)/pdsep)+1
              ik = MIN(MAX(1,ik),khoriz-1)
              ikp1 = ik+1
      
      
! next step
       
              if ((prefrac(i,ik) < 0.0_wp)   .or.&
              &   (prefrac(i,ikp1) < 0.0_wp) .or. &
              &    prefrac(i,ik) == ropp_MDFV       .or. &
              &    prefrac(i,ikp1) == ropp_MDFV ) ll_intercept = .true.


! will next step hit orography
       
              if (ll_intercept) exit 
      
      
         
! horizontal weighting factor               
            
              if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
                 zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
                 zhwt2 = 1.0_wp - zhwt1
               
                 zhwt1_prime = -zyt_prime(2)/pdsep
                 zhwt2_prime = - zhwt1_prime 
            
              elseif (zyt(2) < ztheta_min) then            
            
                 zhwt1 = 1.0_wp
                 zhwt2 = 0.0_wp
               
                 zhwt1_prime = 0.0_wp
                 zhwt2_prime = 0.0_wp
               
              elseif (zyt(2) > ztheta_max) then
            
                 zhwt1 = 0.0_wp
                 zhwt2 = 1.0_wp
               
                 zhwt1_prime = 0.0_wp
                 zhwt2_prime = 0.0_wp
               
              endif    

         
! radius of pressure level
             
              zrad = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)
            
              zrad_prime = zhwt1*pradius_prime(i+1,ik)+zhwt2*pradius_prime(i+1,ikp1) + &
             & zhwt1_prime*pradius(i+1,ik)+zhwt2_prime*pradius(i+1,ikp1)
               
            
! if gone over the boundary scale h
            
              if ( j == isplit .or. (zyt(1)+zrtan - zrad) > 0.0_wp) then
            
            
                 lleaving = .true. 
                  
                 zdr_dtheta = 0.0_wp
                 zdr_dtheta_prime = 0.0_wp 
            
                 if (zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                
                    zdr_dtheta = (pradius(i+1,ikp1)-pradius(i+1,ik))/pdsep
               
                    zdr_dtheta_prime = &
                   & (pradius_prime(i+1,ikp1)-pradius_prime(i+1,ik))/pdsep
        
                 endif                
                            
                 zhuse = zh - (zyt(1)+zrtan-zrad)/(zdydh(1) - zdr_dtheta*zdydh(2))
                        
                 zhuse_prime = zh_prime - &
                & (zyt_prime(1) + zrtan_prime - zrad_prime)/(zdydh(1) - zdr_dtheta*zdydh(2))  &
                & + (zyt(1)+zrtan-zrad)/(zdydh(1) - zdr_dtheta*zdydh(2))**2* &
                & (zdydh_prime(1) - zdr_dtheta_prime*zdydh(2) - zdr_dtheta*zdydh_prime(2))              

! limit the step

               
               if (zhuse > zhmax) then
               
                   zhuse = zhmax
            
                   zhuse_prime = 0.0_wp
            
               elseif (zhuse < zhmin) then
               
                   zhuse = zhmin
                   
                   zhuse_prime = 0.0_wp
            
               endif

                
                
              else
                
                 zhuse = zh
                 zhuse_prime = zh_prime                             
                     
              endif 

! update the position vector

                     
              zy(:) = zy(:) + zdydh(:)*zhuse 
              zy_prime(:) = zy_prime(:) + zdydh(:)*zhuse_prime + zdydh_prime(:)*zhuse
      
              palpha_half(iside) = zy(4)
              palpha_half_prime(iside) = zy_prime(4)
      
         
              if (lleaving) exit
                             
! try to maintain roughly the same radial increment by adjusting h
        
              if (j < isplit) then
            
        
                 zdr = (zrad-zy(1)-zrtan)/REAL(MAX(isplit-j,1))
                 zdr_prime = (zrad_prime-zy_prime(1)-zrtan_prime)/ &
               &        REAL(isplit-j) 
                
                 zhnew = MIN(zh,zdr/MAX(1.0e-10_wp,COS(zy(3))))
               
            
                 if (zhnew < zh) then
            
                    zhnew_prime = zhnew/zdr*zdr_prime + zhnew*TAN(zy(3))*zy_prime(3)
               
                 else
            
                    zhnew_prime = zh_prime    

                 endif  
                
                 zh = zhnew
                 zh_prime = zhnew_prime


! set minimum step-length
            
                 if (zh > zhmax) then
               
                    zh = zhmax
            
                    zh_prime = 0.0_wp
            
                 elseif (zh < zhmin) then
               
                    zh = zhmin
                    
                    zh_prime = 0.0_wp
            
                 endif

            
                 zh2 = 0.5_wp*zh
                 zh2_prime = 0.5_wp*zh_prime

              endif 


           enddo  ! complete path thru ith layer

          
      else


! do 1d calculation


         if (llfirst_1d) then
         
         
            ik = NINT((zy(2) + ztheta_tan)/pdsep)+1
            ik = MIN(MAX(1,ik),khoriz-1)
            
            pa_path(in,iside) = (1.0_wp+1.0e-6_wp*prefrac(i,ik))*((zy(1)+zrtan)*SIN(zy(3)))

            pa_path_prime(in,iside) = pa_path(in,iside)* &
          &  (1.0e-6_wp/(1.0_wp+1.0e-6_wp*prefrac(i,ik))*prefrac_prime(i,ik) + &
          &  (zy_prime(1)+zrtan_prime)/(zy(1)+zrtan) + &
          &  COS(zy(3))/SIN(zy(3))*zy_prime(3) ) 
                        
            zaval = pa_path(in,iside)
            zaval_prime = pa_path_prime(in,iside)            

            zaval = pa(in)
            zaval_prime = 0.0_wp
                       
                        
            palpha_half(iside) = zy(4)
            palpha_half_prime(iside) = zy_prime(4)
            
            
            
            llfirst_1d = .false.
            
          endif     

! continue with 1d bending angle calculation

         
         if ( i == ibot) then 
      
! we are doing a 1d calc for entire ray path
            
            llone_d_calc = .true.             
            zref_low = prefrac(i,ik)*EXP(-zkval(i,ik)*(pa(in)-pnr(i,ik)))
            
            zref_low_prime = zref_low*               &
                 & (prefrac_prime(i,ik)/prefrac(i,ik) -  &
                 &  zkval_prime(i,ik)*(pa(in)-pnr(i,ik)) + &
                 &  zkval(i,ik)*pnr_prime(i,ik))  

            pa_path(in,iside) = pa(in)
            pa_path_prime(in,iside) = 0.0_wp

            zaval = pa(in)
            zaval_prime = 0.0_wp
                         
            znr_low = pa(in)
            znr_low_prime = 0.0_wp


         else 
      
            zref_low = prefrac(i,ik)
            zref_low_prime = prefrac_prime(i,ik)
             
            znr_low  = pnr(i,ik) 
            znr_low_prime = pnr_prime(i,ik)
                  
         endif


         if ((prefrac(i+1,ik)-prefrac(i,ik)) > -1.0e-10_wp) then  
!
! allow the refractivity to increase with height when calculating bending angle 
! occurs in ~ 8% of cases.

! assume a constant gradient 
    
             zdn_dx = (prefrac(i+1,ik)-prefrac(i,ik))/(pnr(i+1,ik)-pnr(i,ik))
     
             zdn_dx_prime = (prefrac_prime(i+1,ik)-prefrac_prime(i,ik))/(pnr(i+1,ik)-pnr(i,ik)) - &
           & zdn_dx/(pnr(i+1,ik)-pnr(i,ik))*(pnr_prime(i+1,ik)-pnr_prime(i,ik))
       
      
             ztup = SQRT( pnr(i+1,ik)-pa(in))       
             ztup_prime = 0.5_wp/SQRT( pnr(i+1,ik)-pa(in))*pnr_prime(i+1,ik)
          
             ztlow = 0.0_wp
             ztlow_prime = 0.0_wp
       
             if (i > ibot)  then
       
                ztlow = SQRT( pnr(i,ik)-pa(in))
                ztlow_prime = 0.5_wp/SQRT( pnr(i,ik)-pa(in))*pnr_prime(i,ik)

             endif
                
             zdalpha  = - 1.0e-6_wp*SQRT(2.0_wp*pa(in))* zdn_dx*(ztup-ztlow)
       
             zdalpha_prime  = - 1.0e-6_wp*SQRT(2.0_wp*pa(in))* &
       &     (zdn_dx_prime*(ztup-ztlow) + zdn_dx*(ztup_prime-ztlow_prime))
            
      
         else


             ztlow = 0.0_wp
             ztlow_prime = 0.0_wp 
       
       
             if (i > ibot) then
         
               ztlow = SQRT(MAX(zkval(i,ik)*(pnr(i,ik) - zaval),1.0e-10_wp))
            
               if (zkval(i,ik)*(pnr(i,ik) - zaval) > 1.0e-10_wp) then
            
                 ztlow_prime = 0.5_wp*(zkval_prime(i,ik)*(pnr(i,ik)-zaval) + &
             &   zkval(i,ik)*(pnr_prime(i,ik)-zaval_prime))/ztlow
               
               else
            
                  ztlow_prime = 0.0_wp                
                
               endif                  

             endif 
 
             ztup = SQRT(MAX(zkval(i,ik)*(pnr(i+1,ik)-zaval),1.0e-10_wp))
  
             if (zkval(i,ik)*(pnr(i+1,ik)-zaval) > 1.0e-10_wp) then 
 
               ztup_prime = 0.5_wp*(zkval_prime(i,ik)*(pnr(i+1,ik)-zaval) + &
         &     zkval(i,ik)*(pnr_prime(i+1,ik)-zaval_prime))/ztup

             else
            
               ztup_prime = 0.0_wp
            
             endif   


! calculate the error functions within this routine rather than an external function call.


            if (i == ibot) then
                                   
               zerf_low = 0.0_wp
            
               zerf_low_prime = 0.0_wp
                                      
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztup)
            
               zt_prime = - zt/(1.0_wp+0.47047_wp*ztup)*0.47047_wp*ztup_prime
                        
               zerf_up= &
            &1.0_wp-(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztup*ztup))
            
               zerf_up_prime = &
           &-(0.3480242_wp-(0.1917596_wp-2.2435668_wp*zt)*zt)*EXP(-(ztup*ztup))*zt_prime + &
           & (0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztup*ztup))&
           &*2.0_wp*ztup*ztup_prime
            
            elseif (i > ibot .and. i < klev-1) then
                    
! lower
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztlow)            
               zt_prime = - zt/(1.0_wp+0.47047_wp*ztlow)*0.47047_wp*ztlow_prime
            
               zerf_low = -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztlow*ztlow))
             
               zerf_low_prime = &
          &  -(0.3480242_wp-(0.1917596_wp-2.2435668_wp*zt)*zt)*EXP(-(ztlow*ztlow))*zt_prime + &
          &  (0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)* &
          &  zt*EXP(-(ztlow*ztlow))*2.0_wp*ztlow*ztlow_prime

! upper

               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztup)
               zt_prime = - zt/(1.0_wp+0.47047_wp*ztup)*0.47047_wp*ztup_prime

            
               zerf_up= -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztup*ztup))            
            
               zerf_up_prime = &
          &  -(0.3480242_wp-(0.1917596_wp-2.2435668_wp*zt)*zt)*&
          &   EXP(-(ztup*ztup))*zt_prime + &
          &  (0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztup*ztup))* &
          &   2.0_wp*ztup*ztup_prime
                        
            else
         
               zerf_up = 0.0_wp
               zerf_up_prime = 0.0_wp 
         
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztlow)            
               zt_prime = - zt/(1.0_wp+0.47047_wp*ztlow)*0.47047_wp*ztlow_prime
            
               zerf_low = -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztlow*ztlow))
             
               zerf_low_prime = &
           &-(0.3480242_wp-(0.1917596_wp-2.2435668_wp*zt)*zt)*&
           &EXP(-(ztlow*ztlow))*zt_prime + &
           &(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztlow*ztlow)) &
           &*2.0_wp*ztlow*ztlow_prime
                  
                                 
            endif          
         
          
            zdiff_erf = zerf_up - zerf_low 
            zdiff_erf_prime = zerf_up_prime - zerf_low_prime
         


! bending angle          
         
         
            zdalpha    =  &
        & + 1.0e-6_wp * zroot_halfpi* SQRT(zaval*zkval(i,ik)) & 
        & * zref_low*EXP(zkval(i,ik)*(znr_low-zaval))*zdiff_erf 
 
 
            zdalpha_prime = zdalpha*(  &
                      & zref_low_prime/MAX(1.0e-10_wp,zref_low)  + &
                      &        zdiff_erf_prime/MAX(1.0e-10_wp,zdiff_erf) + &
                      &        (0.5_wp/zaval - zkval(i,ik))*zaval_prime + &
                      &        (znr_low -zaval + 0.5_wp/zkval(i,ik))*zkval_prime(i,ik) + &
                      &        zkval(i,ik)*znr_low_prime ) 
        


           endif


          palpha_half(iside) = palpha_half(iside) + zdalpha 
          palpha_half_prime(iside) = palpha_half_prime(iside) + zdalpha_prime  


        endif 
      
      if (ll_intercept) exit   ! the model level loop                                
                  
      enddo  ! i the layers

      if (ll_intercept) exit   ! the iside loop              

! if we performed a 1d calculation don't evaluate iside = 2

     
      if (llone_d_calc) then
      
         palpha_half(2) = palpha_half(1)

         palpha_half_prime(2) = palpha_half_prime(1)
          
         exit  ! exiting the iside loop
     
     endif      


   enddo ! iside


! the total bending angle adding both sides

   if (ll_intercept) then
   
      palpha_prime(in) = ropp_MDFV   
   
   else
       
      palpha(in) = palpha_half(1) + palpha_half(2) 
   
      palpha_prime(in) = palpha_half_prime(1) + palpha_half_prime(2)
   
   endif 

   
enddo obloop


! we're not using the impact variation

pa_path_prime = 0.0_wp

END SUBROUTINE ropp_fm_alpha2drk_ec_tl
