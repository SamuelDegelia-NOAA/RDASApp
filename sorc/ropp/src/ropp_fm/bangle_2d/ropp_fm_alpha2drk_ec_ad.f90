! $id$

!****s* bendingangle2d/ropp_fm_alpha2drk_ec_ad *
!
! name
!    ropp_fm_alpha2drk_ad - forward model calculating a bending
!                        angle profile from planar information.
!
! synopsis
!    call ropp_fm_alpha2drk_ec_ad(kobs, klev, ...)
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
!           ksplit = splitting of model levels
!           pdsep  =  angular spacing
!           pa     =  impact parameters
!           proc   =  radius of curvature 
!           pz_2d  =  2d impact height (do a 1d calculation above pz_2d)
!           prefrac=  refractivity values
!           pradius=  radius values
!           pnr    =  nr product 
!
! output
! 
!           pa_path = impact parameter at end points of ray path
!           palpha  = bending angle values 
!
! notes
!    the forward model calculate the bending angle as a function of
!    impact parameter. below "pz_2d" a runge-kutta solver is used
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

subroutine ropp_fm_alpha2drk_ec_ad(kobs,   &  ! no.of observations
                             & klev,   &  ! no. of vertical levels
                             & khoriz, &  ! no. of horizontal layers  odd
                             & ksplit, &
                             & pdsep,  &  ! the angular spacing 
                             & pa,     &  ! impact parameter values
                             & prefrac,&  ! refractivity
                             & prefrac_hat, &
                             & pradius, & ! radius values
                             & pradius_hat, &
                             & pnr, &
                             & pnr_hat, &
                             & proc, &
                             & pz_2d, &
                             & pa_path_hat, &
                             & palpha_hat) ! partial path length along rays



use typesizes, only: wp => eightbytereal
use ropp_utils, only: ropp_mdfv
use ropp_fm_constants, only : pi
USE ropp_fm, not_this => ropp_fm_alpha2drk_ec_ad


implicit none


! subroutine args. 


integer, intent(in)  :: kobs           ! size of ob. vector
integer, intent(in)  :: klev           ! no. of refractivity levels
integer, intent(in)  :: khoriz         ! no. of horizontal locations
integer, intent(in)  :: ksplit 
REAL(wp),    intent(in)  :: pdsep           ! angular spacing of grid
REAL(wp),    intent(in)  :: pa(kobs)        ! impact parameter - now assumed to be on pressure levels
REAL(wp),    intent(in)  :: prefrac(klev,khoriz)   ! refractivity values on levels
REAL(wp),    intent(inout)  :: prefrac_hat(klev,khoriz)   ! refractivity values on levels
REAL(wp),    intent(in)  :: pradius(klev,khoriz)   ! radius values
REAL(wp),    intent(inout)  :: pradius_hat(klev,khoriz)   ! radius values
REAL(wp),    intent(in)  :: pnr(klev,khoriz)
REAL(wp),    intent(inout)  :: pnr_hat(klev,khoriz)
REAL(wp),    intent(in)  :: proc                   ! radius of curvature
REAL(wp),    intent(in)  :: pz_2d
REAL(wp),    intent(inout) :: pa_path_hat(kobs,2)        
REAL(wp),    intent(inout) :: palpha_hat(kobs)   ! path length
                       

! local variables


integer :: i,j,jj,in,ibot,jbot,ikbot,iside,ik,ikp1,ibot_old,jdum,isplit,i_below
integer :: ikcen
REAL(wp), parameter :: zhmax = 5.0e4_wp
REAL(wp), parameter :: zhmin = 1.0e2_wp
REAL(wp), PARAMETER :: dn_dx_max = 0.157_wp ! setting the maxiumin N gradients
REAL(wp) :: zrad(klev,2*ksplit),zdndr
REAL(wp) :: zrad_hat,zdndr_hat
REAL(wp) :: zhwt1,zhwt2
REAL(wp) :: zhwt1_hat,zhwt2_hat
REAL(wp) :: zamult
REAL(wp) :: zh(klev,2*ksplit),zh2,zhuse(klev,2*ksplit),zhnew,zh_up(klev)
REAL(wp) :: zh_hat,zh2_hat,zhuse_hat,zhnew_hat,zh_up_hat
REAL(wp) :: zy(4,klev,2*ksplit),zyt_save(4,klev,2*ksplit),zyt(4)
REAL(wp) :: zy_hat(4),zyt_hat(4)
REAL(wp) :: zdydh(4,klev,2*ksplit),zdydh_save(4,klev,2*ksplit),zdydh_tmp
REAL(wp) :: zdydh_hat(4)
REAL(wp) :: ztheta_tan,ztheta_min,ztheta_max
REAL(wp) :: zdr_max,zdr_dtheta(klev),zrtan,zdr(klev,2*ksplit)
REAL(wp) :: zdr_max_hat,zdr_dtheta_hat,zrtan_hat,zdr_hat
REAL(wp) :: zalpha_half(2)
REAL(wp) :: zalpha_half_hat(2)
REAL(wp) :: zkval(klev-1,khoriz),zkval_save(klev-1,khoriz)
REAL(wp) :: zkval_hat(klev-1,khoriz)
REAL(wp) :: ztlow(klev),ztup(klev),zdalpha(klev),zroot_halfpi
REAL(wp) :: ztlow_hat,ztup_hat,zdalpha_hat
REAL(wp) :: zerf_up(klev),zerf_low(klev),ztl(klev),ztu(klev),zdiff_erf(klev)
REAL(wp) :: znr_low(klev),zref_low_1d(klev),zaval
REAL(wp) :: zerf_up_hat,zerf_low_hat,zt_hat,zdiff_erf_hat,znr_low_hat,zaval_hat
REAL(wp) :: zrad_up,zrad_low,zref_up,zref_low,zkval_theta,zdndr2,zdn_dx(klev),zed
REAL(wp) :: zrad_up_hat,zrad_low_hat,zref_up_hat,zref_low_hat,zkval_theta_hat,zdndr2_hat,zdn_dx_hat,zed_hat


REAL(wp) :: zalpha(kobs),za_path(kobs,2)
integer :: in_2d
integer :: istep(klev)
logical :: llfirst_1d,lleaving,llone_d_calc,ll_intercept

REAL(wp) :: zhook_handle



!initialise local adjoint variables


zalpha_half_hat(:) = 0.0_wp 

! 1d

zdalpha_hat = 0.0_wp
zerf_up_hat = 0.0_wp
zerf_low_hat = 0.0_wp
zref_low_hat = 0.0_wp
zdiff_erf_hat = 0.0_wp
zt_hat = 0.0_wp
znr_low_hat = 0.0_wp
zaval_hat = 0.0_wp
zkval_hat = 0.0_wp
ztlow_hat = 0.0_wp
ztup_hat = 0.0_wp

! 2d

zrtan_hat = 0.0_wp
zy_hat(:) = 0.0_wp
zyt_hat(:) = 0.0_wp
zh2_hat = 0.0_wp
zh_hat = 0.0_wp
zhnew_hat = 0.0_wp
zhuse_hat = 0.0_wp
zhwt1_hat = 0.0_wp
zhwt2_hat = 0.0_wp
zrad_hat   = 0.0_wp

zdydh_hat(:) = 0.0_wp
zdr_max_hat = 0.0_wp
zdr_dtheta_hat = 0.0_wp
zrad_hat = 0.0_wp
zdndr_hat = 0.0_wp
zhwt1_hat = 0.0_wp
zhwt2_hat = 0.0_wp
zdr_hat = 0.0_wp
zh_up_hat = 0.0_wp

zrad_up_hat = 0.0_wp
zrad_low_hat = 0.0_wp
zref_up_hat = 0.0_wp
zkval_theta_hat = 0.0_wp
zdndr2_hat = 0.0_wp
zdn_dx_hat = 0.0_wp
zed_hat = 0.0_wp

! the central profile kcen


! we're not using the impact parameter variation.

pa_path_hat = 0.0_wp        

ikcen = khoriz/2 + 1
ztheta_tan = REAL(ikcen-1)*pdsep 
ztheta_min = -ztheta_tan
ztheta_max =  ztheta_tan


zkval(:,:) = 1.5e-4_wp ! climatological value 
zkval_save(:,:) = zkval(:,:)

do i = 1,klev-1

   do j = 1, khoriz
   
      if (prefrac(i,j) > 0.0_wp .and. prefrac(i+1,j) > 0.0_wp) then
   
        zkval(i,j) = LOG(prefrac(i,j)/prefrac(i+1,j))/MAX((pnr(i+1,j) - pnr(i,j)),1.0_wp)      

        zkval_save(i,j) = zkval(i,j) ! before limiting value
      
        zkval(i,j) = MAX(1.0e-6_wp,zkval(i,j))
        zkval(i,j) = MIN(zkval(i,j),(dn_dx_max/prefrac(i,j)))      
      
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

   if ((pnr(ikbot,ikcen) - pnr(ikbot-1,ikcen)) < 1.0_wp) exit 

   ikbot = ikbot - 1

enddo
 
jbot = MAX(jbot,ikbot)



! set the outputs to missing


zalpha(:)=ropp_MDFV
za_path(:,:)= ropp_MDFV


zroot_halfpi = SQRT(0.5_wp*pi)

obloop: do in=1,kobs
              
   if (pa(in) < pnr(jbot,ikcen) .or. pa(in) > pnr(klev-3,ikcen)) cycle  

! if the departure is missing cycle loop
        
   if (palpha_hat(in) == ropp_MDFV) cycle
   
! adjoint code
    
   zalpha_half_hat(1) = zalpha_half_hat(1) + palpha_hat(in)
   zalpha_half_hat(2) = zalpha_half_hat(2) + palpha_hat(in)
   palpha_hat(in) = 0.0_wp
      
   ibot = jbot

   do 
    if (pnr(ibot+1,ikcen) - pa(in) > 1.0_wp) exit   ! assuming "a" is on one of the pressure levels
    ibot=ibot+1
   enddo

! calculate the radius at tangent point   

   zrad(1,1) = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))   
   
   zdndr = 1.0e-6_wp*(prefrac(ibot+1,ikcen)-prefrac(ibot,ikcen))/ &
         & (pradius(ibot+1,ikcen)-pradius(ibot,ikcen)) 
  
                   
   if ( zrad(1,1)*zdndr > -1.0_wp) then
  
       zrtan = pradius(ibot,ikcen) + &
             & (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad(1,1)*zdndr)

   else
   
       zrtan = zrad(1,1)   ! probably in a super-refracting layer
              
   endif                

! if zrtan is within a 1 m of upper level set to upper level
   
   ibot_old = ibot
   
   if ((zrtan - pradius(ibot+1,ikcen)) > -1.0_wp) then
   
       ibot = ibot + 1
          
       zrtan = pradius(ibot,ikcen)
       
   endif     

! don't calculate ba's that may hit orography

   i_below = 0
   
   do i = ikcen-1, ikcen+1
   
      if (prefrac(ibot,i) < 0.0_wp .or.prefrac(ibot,i) == ropp_MDFV) i_below = i_below + 1
      
   enddo
   
   if (i_below /= 0) cycle    

!  set bending angle value     

   zalpha_half(:) = 0.0_wp   

   ll_intercept = .false. 

   do iside = 1,2 
   
      istep(:) = 0
   
      za_path(in,iside) = pa(in)  ! adjoint = ??
      llfirst_1d = .true.
      llone_d_calc = .false.  
      zamult = 1.0_wp
      if (iside == 2) zamult  = -1.0_wp

! initialise vector

      zy(1,ibot,1) = 0.0_wp           ! height above tangent point           
      zy(2,ibot,1) = 0.0_wp           ! theta
      zy(3,ibot,1) = aSIN(1.0_wp)     ! thi
      zy(4,ibot,1) = 0.0_wp           ! bending angle 
      
      
      do i = ibot,klev-1
          
  
         if ( i < MIN(in_2d,klev-1)) then
 
 ! set the set the splitting
  
            isplit = ksplit

! smaller steps near tangent point.

            if (i - ibot < 2) isplit = 2*ksplit

        
            if (i == ibot) then
         
               ik = ikcen
               zdr_max = (pradius(i+1,ik)- zrtan)/REAL(isplit)                        
               zh(i,1) = SQRT(2.0_wp*pradius(ibot,ik)*zdr_max)                    
            
            else

               ik = INT((zy(2,i,1) + ztheta_tan)/pdsep)+1
               ik = MIN(MAX(1,ik),khoriz)
               
               if ((prefrac(i,ik) < 0.0_wp)   .or.&
               &    prefrac(i,ik) == ropp_MDFV) then 
       
                  ll_intercept = .true.
       
               else
       
               zdr_max = (pradius(i+1,ik)-pradius(i,ik))/REAL(isplit)
            
               zh_up(i) = SQRT(2.0_wp*pradius(i,ik)*zdr_max)            
               zh(i,1) = zdr_max/MAX(COS(zy(3,i,1)),1.0e-10_wp)

! physical limit on step size
               
               zh(i,1) = MIN(zh_up(i),zh(i,1))
       
               endif

            endif

            if (ll_intercept) exit              

! limit the step-length
         
            if (zh(i,1) > zhmax) then
         
              zh(i,1) = zhmax
            
            elseif (zh(i,1) < zhmin) then
         
              zh(i,1) = zhmin    
                     
            endif 


            zh2 = 0.5_wp*zh(i,1)         
         

! now calculate the path-length with a runge-kutta

         
            lleaving = .false. 
           
            do j = 1,isplit          
           
               istep(i) = istep(i) + 1

               do jj = 1, 2 
                                   
                  zyt(:) = zy(:,i,j)           
   
                  if (jj == 2) then
  
                     zyt(:) = zyt(:) + zdydh_save(:,i,j)*zh2   ! use gradients from jj=1 loop
                     zyt_save(:,i,j) = zyt(:)  ! use this in adjoint
  
                  endif
                 
        
! where are we in the plane    
                   
                   ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
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

                   if (ll_intercept) exit  ! leave jj
         
! horizontal weighting factor               
            
                   if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
                       zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
                       zhwt2 = 1.0_wp - zhwt1
            
                   elseif (zyt(2) < ztheta_min) then            
            
                       zhwt1 = 1.0_wp
                       zhwt2 = 0.0_wp
               
                   elseif (zyt(2) > ztheta_max) then
                       
                       zhwt1 = 0.0_wp
                       zhwt2 = 1.0_wp
               
                   endif    
!   
! calculate the gradients at that point
!

                   zdydh(1,i,j) = MAX(1.0e-10_wp,COS(zyt(3)))       

                   zdydh(2,i,j) = zamult*SIN(zyt(3))/(zyt(1)+zrtan)

!
! interpolate refractivity and radius to point in plane.
!
     
                   zref_up  = zhwt1*prefrac(i+1,ik)+zhwt2*prefrac(i+1,ikp1)
                   zref_low = zhwt1*prefrac(i,ik) + zhwt2*prefrac(i,ikp1)

                   zrad_up = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)
                   zrad_low = zhwt1*pradius(i,ik) + zhwt2*pradius(i,ikp1)
    
! estimate radial gradient
    
                   if ((zref_up - zref_low) > -1.0e-10_wp) then
      
                      zdndr2 = 1.0e-6_wp*(zref_up-zref_low)/(zrad_up-zrad_low)  ! +ve refrac gradient
    
                   else
    
                       zkval_theta = LOG(zref_low/zref_up)/(zrad_up-zrad_low)

                       zed = MAX(0.0_wp,(zyt(1)+zrtan-zrad_low))
          
                       zdndr = - 1.0e-6_wp*zkval_theta*zref_low*EXP(-zkval_theta*zed)

                       zdndr2 = MAX(-0.75e-7_wp,zdndr)
  
                   endif  
  
                   zdydh(3,i,j) = -SIN(zyt(3))*(1.0_wp/(zyt(1)+zrtan) + zdndr2)

                   zdydh(4,i,j) = - SIN(zyt(3))*zdndr2

! need to save this for the adjoint   
   
                   if (jj == 1) zdydh_save(:,i,j) = zdydh(:,i,j)
  
       
               enddo  ! jj
       
               if (ll_intercept) exit    ! exit isplit
       
! update with latest estimate of gradient            
    
               zyt(:) = zy(:,i,j) + zdydh(:,i,j)*zh(i,j)
            
! check the radius - have we exited the level
                        
               ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
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
            
               elseif (zyt(2) < ztheta_min) then            
            
                  zhwt1 = 1.0_wp
                  zhwt2 = 0.0_wp
               
               elseif (zyt(2) > ztheta_max) then
            
                  zhwt1 = 0.0_wp
                  zhwt2 = 1.0_wp
               
               endif    
                              
                    
! radius of pressure level             

               zrad(i,j) = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)   
       
            
! if gone over the boundary scale h
            
               if ( j == isplit .or. (zyt(1)+zrtan - zrad(i,j)) > 0.0_wp ) then
            
                  lleaving = .true.
            
                  zdr_dtheta(i) = 0.0_wp
            
                  if (zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) &            
                 & zdr_dtheta(i) = (pradius(i+1,ikp1)-pradius(i+1,ik))/pdsep
        
                  zhuse(i,j) = zh(i,j) - (zyt(1)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))

                  zhuse(i,j) = MAX(MIN(zhuse(i,j),zhmax),zhmin)
                                             
               else 
            
                  zhuse(i,j) = zh(i,j)  
                           
               endif 
          
                        
! update the position vector

              if ( j == isplit .or. lleaving) then
                        
                 zy(:,i+1,1) = zy(:,i,j) + zdydh(:,i,j)*zhuse(i,j)
            
              else
            
                 zy(:,i,j+1) = zy(:,i,j) + zdydh(:,i,j)*zhuse(i,j)                

              endif   
                    
               if (lleaving) exit  

! try to maintain roughly the same radial increment by adjusting h
               if (j < isplit) then

                 zdr(i,j) = (zrad(i,j)-zy(1,i,j+1)-zrtan)/REAL(isplit-j)
                 zhnew = MIN(zh(i,j),zdr(i,j)/MAX(1.0e-10_wp,COS(zy(3,i,j+1))))            
                 
                 zh(i,j+1) = MAX(MIN(zhmax,zhnew),zhmin)

                 zh2 = 0.5_wp*zh(i,j+1)
               
               endif   

            enddo  ! complete path thru ith layer


      else


! do 1d calculation


         if (llfirst_1d) then
         
         
            ik = NINT((zy(2,i,1) + ztheta_tan)/pdsep)+1
            ik = MIN(MAX(1,ik),khoriz-1)
            
            za_path(in,iside) = &
           & (1.0_wp+1.0e-6_wp*prefrac(i,ik))*((zy(1,i,1)+zrtan)*SIN(zy(3,i,1)))
                        
             zaval = za_path(in,iside)

! testing
     
            zaval = pa(in) 
     
            zalpha_half(iside) = zy(4,i,1)
            
            llfirst_1d = .false.
            
          endif     

         ! continue with 1d bending angle calculation

         if ( i == ibot) then 
 
 
 ! we are doing a 1d calc for entire ray path
            
            llone_d_calc = .true.             
      
            zref_low_1d(i) = prefrac(ibot,ik)*EXP(-zkval(ibot,ik)*(pa(in)-pnr(ibot,ik)))
            
            zaval = pa(in) 
            znr_low(i) = pa(in)
        
         else 
      
            zref_low_1d(i) = prefrac(i,ik) 
            znr_low(i)  = pnr(i,ik) 
         
         endif
         
 
         if ((prefrac(i+1,ik)-prefrac(i,ik)) > -1.0e-10_wp) then  
!
! allow the refractivity to increase with height when calculating bending angle 
! occurs in ~ 8% of cases.

! assume a constant gradient 
    
             zdn_dx(i) = (prefrac(i+1,ik)-prefrac(i,ik))/(pnr(i+1,ik)-pnr(i,ik))
       
             ztup(i) = SQRT( pnr(i+1,ik)-pa(in))       
      
             ztlow(i) = 0.0_wp
       
             if (i > ibot)  ztlow(i) = SQRT( pnr(i,ik)-pa(in))
            
             zdalpha(i)  = - 1.0e-6_wp*SQRT(2.0_wp*pa(in))* zdn_dx(i)*(ztup(i)-ztlow(i))
      
         else

            ztlow(i) = 0.0_wp
            if (i > ibot) ztlow(i) = SQRT(MAX(zkval(i,ik)*(pnr(i,ik) - zaval),1.0e-10_wp))

            ztup(i) = SQRT(MAX(zkval(i,ik)*(pnr(i+1,ik)-zaval),1.0e-10_wp))


! calculate the error functions within this routine rather than an external function call.


            if (i == ibot) then
                                   
               zerf_low(i) = 0.0_wp         
                 
               ztu(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztup(i))
            
               zerf_up(i)= &
         &   1.0_wp-(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztu(i))*ztu(i))*ztu(i)*EXP(-(ztup(i)*ztup(i)))            
         
            elseif (i > ibot .and. i < klev-1) then
                    
! lower
               ztl(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztlow(i)) 
            
              zerf_low(i) = &
          & -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztl(i))*ztl(i))*ztl(i)*EXP(-(ztlow(i)*ztlow(i))) 

! upper

               ztu(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztup(i))
              
               zerf_up(i)= &
             & -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztu(i))*ztu(i))*ztu(i)*EXP(-(ztup(i)*ztup(i)))            
                        
            else
         
               zerf_up(i) = 0.0_wp 
         
               ztl(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztlow(i)) 
            
               zerf_low(i) = &
             & -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztl(i))*ztl(i))*ztl(i)*EXP(-(ztlow(i)*ztlow(i))) 
            
            endif     
          
          
            zdiff_erf(i) = zerf_up(i) - zerf_low(i) 
         

! bending angle          
         
            zdalpha(i)    =  &
            & + 1.0e-6_wp * zroot_halfpi* SQRT(zaval*zkval(i,ik)) & 
            & * zref_low_1d(i)*EXP(zkval(i,ik)*(znr_low(i)-zaval))*zdiff_erf(i) 
 
 
         endif  
 
         zalpha_half(iside) = zalpha_half(iside) + zdalpha(i) 

      endif 

      if (ll_intercept) exit
                  
      enddo  ! i the layers

      if (ll_intercept) exit

! now start the adjoint

      if (llone_d_calc) then
      
         zalpha_half_hat(1) = zalpha_half_hat(1) + zalpha_half_hat(2)
         zalpha_half_hat(2) = 0.0_wp
 
      endif 
 
 
! loop through levels
      
      do i = klev-1,ibot,-1                      
         

! set the set the splitting
  
         isplit = ksplit

! smaller steps near tangent point.

         if (i - ibot < 2) isplit = 2*ksplit

      
         if (i >= MIN(in_2d,klev-1)) then


! 1d calculation         
                  
            zdalpha_hat = zdalpha_hat + zalpha_half_hat(iside)
            zalpha_half_hat(iside) = zalpha_half_hat(iside)  
    
    
            if ((prefrac(i+1,ik)-prefrac(i,ik)) > -1.0e-10_wp) then  
            
! adjoint bit     
  
             zdn_dx_hat = zdn_dx_hat - 1.0e-6_wp*SQRT(2.0_wp*pa(in))*(ztup(i)-ztlow(i))*zdalpha_hat
             ztup_hat = ztup_hat - 1.0e-6_wp*SQRT(2.0_wp*pa(in))*zdn_dx(i)*zdalpha_hat  
             ztlow_hat = ztlow_hat + 1.0e-6_wp*SQRT(2.0_wp*pa(in))*zdn_dx(i)*zdalpha_hat
             zdalpha_hat = 0.0_wp
       
             if (i > ibot)  then

                pnr_hat(i,ik) = pnr_hat(i,ik) + 0.5_wp/SQRT( pnr(i,ik)-pa(in))*ztlow_hat
                ztlow_hat = 0.0_wp

             endif
    
             ztlow_hat = 0.0_wp
     
             pnr_hat(i+1,ik) = pnr_hat(i+1,ik) + 0.5_wp/SQRT( pnr(i+1,ik)-pa(in))*ztup_hat
             ztup_hat = 0.0_wp
     
             prefrac_hat(i+1,ik) = prefrac_hat(i+1,ik) + zdn_dx_hat/(pnr(i+1,ik)-pnr(i,ik))
             prefrac_hat(i,ik) =   prefrac_hat(i,ik)   - zdn_dx_hat/(pnr(i+1,ik)-pnr(i,ik))
     
             pnr_hat(i+1,ik) = pnr_hat(i+1,ik) - zdn_dx(i)/(pnr(i+1,ik)-pnr(i,ik))*zdn_dx_hat
             pnr_hat(i,ik)   = pnr_hat(i,ik)   + zdn_dx(i)/(pnr(i+1,ik)-pnr(i,ik))*zdn_dx_hat
             zdn_dx_hat = 0.0_wp
     
      
          else
    
                    
            zref_low_hat = zref_low_hat + &
         &   zdalpha(i)/MAX(1.0e-10_wp,zref_low_1d(i))*zdalpha_hat
         
            zdiff_erf_hat = zdiff_erf_hat + &
         &  zdalpha(i)/MAX(1.0e-10_wp,zdiff_erf(i))*zdalpha_hat
         
            zaval_hat = zaval_hat + &
         &  zdalpha(i)*(0.5_wp/zaval - zkval(i,ik))*zdalpha_hat
         
            zkval_hat(i,ik) = zkval_hat(i,ik) + &
         &  zdalpha(i)*(znr_low(i) -zaval + 0.5_wp/zkval(i,ik))*zdalpha_hat 
            
            znr_low_hat = znr_low_hat + &
         &  zdalpha(i)*zkval(i,ik)*zdalpha_hat
            
            zdalpha_hat = 0.0_wp

! diff erf
      
            zerf_up_hat = zerf_up_hat + zdiff_erf_hat
            zerf_low_hat = zerf_low_hat - zdiff_erf_hat
            zdiff_erf_hat = 0.0_wp
      
         if (i == ibot) then
                                               
            ztup_hat = ztup_hat + &
          & (0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztu(i))*ztu(i))*ztu(i)*  &
          & EXP(-(ztup(i)*ztup(i)))*2.0_wp*ztup(i)*zerf_up_hat
            
            zt_hat = zt_hat - &
         &  (0.3480242_wp-(0.1917596_wp-2.2435668_wp*ztu(i))*ztu(i))*   &
         &   EXP(-(ztup(i)*ztup(i)))*zerf_up_hat
            
            zerf_up_hat = 0.0_wp
            
            ztup_hat = ztup_hat - ztu(i)/(1.0_wp+0.47047_wp*ztup(i))*0.47047_wp*zt_hat
            zt_hat = 0.0_wp
            
            zerf_low_hat = 0.0_wp 
            
            
            
         elseif (i > ibot .and. i < klev-1) then
                    
! upper            
                                               
            ztup_hat = ztup_hat + &
         &   (0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztu(i))*ztu(i))*ztu(i)*  &
         &   EXP(-(ztup(i)*ztup(i)))*2.0_wp*ztup(i)*zerf_up_hat
            
            zt_hat = zt_hat - &
         &  (0.3480242_wp-(0.1917596_wp-2.2435668_wp*ztu(i))*ztu(i))*   &
         &   EXP(-(ztup(i)*ztup(i)))*zerf_up_hat
            
            zerf_up_hat = 0.0_wp
            
            ztup_hat = ztup_hat - ztu(i)/(1.0_wp+0.47047_wp*ztup(i))*0.47047_wp*zt_hat
            zt_hat = 0.0_wp
           
! lower           
           
            ztlow_hat = ztlow_hat + &
        &    (0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztl(i))*ztl(i))*ztl(i)*  &
        &    EXP(-(ztlow(i)*ztlow(i)))*2.0_wp*ztlow(i)*zerf_low_hat
            
            zt_hat = zt_hat - &
        &    (0.3480242_wp-(0.1917596_wp-2.2435668_wp*ztl(i))*ztl(i))*  &
        &    EXP(-(ztlow(i)*ztlow(i)))*zerf_low_hat
            
            zerf_low_hat = 0.0_wp
            
            
            ztlow_hat = ztlow_hat - ztl(i)/(1.0_wp+0.47047_wp*ztlow(i))*0.47047_wp*zt_hat
            zt_hat = 0.0_wp
                
           
                        
        else
         
! lower           
           
            ztlow_hat = ztlow_hat + &
          &  (0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztl(i))*ztl(i))*ztl(i)*  &
          &  EXP(-(ztlow(i)*ztlow(i)))*2.0_wp*ztlow(i)*zerf_low_hat
            
            zt_hat = zt_hat - &
          &  (0.3480242_wp-(0.1917596_wp-2.2435668_wp*ztl(i))*ztl(i))*  &
          &  EXP(-(ztlow(i)*ztlow(i)))*zerf_low_hat
            
            zerf_low_hat = 0.0_wp
            
            
            ztlow_hat = ztlow_hat - ztl(i)/(1.0_wp+0.47047_wp*ztlow(i))*0.47047_wp*zt_hat
            zt_hat = 0.0_wp
         
         
            zerf_up_hat = 0.0_wp 
                  
                                 
         endif

! tup
!!!!         tup_hat = 0.0

      
         if (zkval(i,ik)*(pnr(i+1,ik)-zaval) > 1.0e-10_wp) then 
 
            zkval_hat(i,ik) = zkval_hat(i,ik) + &
         &   0.5_wp*(pnr(i+1,ik)-zaval)/ztup(i)*ztup_hat
            
            pnr_hat(i+1,ik) = pnr_hat(i+1,ik) + 0.5_wp*zkval(i,ik)/ztup(i)*ztup_hat
            zaval_hat = zaval_hat - 0.5_wp*zkval(i,ik)/ztup(i)*ztup_hat
            
            ztup_hat = 0.0_wp            

         else
            
            ztup_hat = 0.0_wp
            
         endif   

! tlow

!!!         tlow_hat = 0.0 

         if (i > ibot) then
         
            
            if (zkval(i,ik)*(pnr(i,ik) - zaval) > 1.0e-10) then
            
               zkval_hat(i,ik) = zkval_hat(i,ik) + &
            &  0.5_wp*(pnr(i,ik)-zaval)/ztlow(i)*ztlow_hat
               
               pnr_hat(i,ik) = pnr_hat(i,ik) + &
            &  0.5_wp*zkval(i,ik)/ztlow(i)*ztlow_hat
               
               zaval_hat = zaval_hat - 0.5_wp*zkval(i,ik)/ztlow(i)*ztlow_hat
               
               ztlow_hat = 0.0_wp
               
               
            else
            
               ztlow_hat = 0.0_wp                
                
            endif                  


         endif 
         
         
         ztlow_hat = 0.0_wp
         
       endif
 
 
         if ( i == ibot) then 
      
            znr_low_hat = 0.0_wp
            zaval_hat = 0.0_wp
            
            pa_path_hat(in,iside) = 0.0_wp
            
            prefrac_hat(i,ik) = prefrac_hat(i,ik) + &
         &  zref_low_1d(i)/prefrac(i,ik)*zref_low_hat
            
            zkval_hat(i,ik) = zkval_hat(i,ik) - &
         &  zref_low_1d(i)*(pa(in)-pnr(i,ik))*zref_low_hat
            
            pnr_hat(i,ik)=pnr_hat(i,ik) + zref_low_1d(i)*zkval(i,ik)*zref_low_hat
            zref_low_hat = 0.0_wp
            

         else 
      
            prefrac_hat(i,ik) = prefrac_hat(i,ik) + zref_low_hat
            zref_low_hat = 0.0_wp
                         
            pnr_hat(i,ik) = pnr_hat(i,ik) + znr_low_hat
            znr_low_hat = 0.0_wp 
        
                  
         endif
         
         if (i == MIN(in_2d,klev-1)) then

! impact parameter variation
         

            zy_hat(4) = zy_hat(4) + zalpha_half_hat(iside) 
            zalpha_half_hat(iside) = 0.0_wp

            zaval_hat = 0.0_wp

            pa_path_hat(in,iside) = pa_path_hat(in,iside) + zaval_hat
            zaval_hat = 0.0_wp  
            
            prefrac_hat(i,ik) = prefrac_hat(i,ik) + &
          &  1.0e-6_wp*za_path(in,iside)/(1.0_wp+1.0e-6_wp*prefrac(i,ik))*pa_path_hat(in,iside)
            
            zy_hat(1) = zy_hat(1) + za_path(in,iside)/(zy(1,i,1)+zrtan)*pa_path_hat(in,iside)
            zrtan_hat = zrtan_hat + za_path(in,iside)/(zy(1,i,1)+zrtan)*pa_path_hat(in,iside)
            
            zy_hat(3) = zy_hat(3) + &
          &  za_path(in,iside)*COS(zy(3,i,1))/SIN(zy(3,i,1))*pa_path_hat(in,iside)
            
            pa_path_hat(in,iside) = 0.0_wp
            
 
         endif 
      
      
         else 

! 2d bit         
               
           do j = istep(i),1,-1
                    
                    
              if (j < istep(i)) then
      
                 zh_hat = zh_hat + 0.5_wp*zh2_hat
                 zh2_hat = 0.0_wp

! limiting the size of the step
               
                  zhnew = zh(i,j+1) ! just for clarity
               
                 if (ABS(zhnew-zhmax)  < 1.0e-3_wp*SPACING(zhnew)) zh_hat = 0.0_wp  
                 if (ABS(zhnew-zhmin)  < 1.0e-3_wp*SPACING(zhnew)) zh_hat = 0.0_wp  
         
                 zhnew_hat = zhnew_hat + zh_hat
                 zh_hat = 0.0_wp
            
                 zhnew = zh(i,j+1) ! just for clarity
               
               if (zhnew < zh(i,j)) then
            
                  
                  zdr_hat = zdr_hat + zhnew/zdr(i,j)*zhnew_hat
                  zy_hat(3) = zy_hat(3) + zhnew*TAN(zy(3,i,j+1))*zhnew_hat
                  zhnew_hat = 0.0_wp
               
               else
            
                  zh_hat = zh_hat + zhnew_hat
                  zhnew_hat = 0.0_wp   
                    
               endif 
            
               zrad_hat = zrad_hat + zdr_hat/REAL(isplit-j)
               zy_hat(1) = zy_hat(1) - zdr_hat/REAL(isplit-j)
               zrtan_hat = zrtan_hat - zdr_hat/REAL(isplit-j)
               zdr_hat = 0.0_wp
           
            endif 
      
! updating the vector. 
          
           zy_hat(4) = zy_hat(4) + zalpha_half_hat(iside)
           zalpha_half_hat(iside) = 0.0_wp    
                      
!!            y_prime(:) = y_prime(:) + dydh(:)*huse_prime + dydh_prime(:)*huse

            zdydh_hat(:) = zdydh_hat(:) + zy_hat(:)*zhuse(i,j)            
            do jj = 1,4
               zhuse_hat = zhuse_hat + zdydh(jj,i,j)*zy_hat(jj)
            enddo   
            zy_hat(:) = zy_hat(:)


! rececalculate position before step adjustment

            zyt(:) = zy(:,i,j) + zdydh(:,i,j)*zh(i,j)            

            ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
            ik = MIN(MAX(1,ik),khoriz-1)
            ikp1 = ik+1
         
! horizontal weighting factor               
            
            if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
               zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
               zhwt2 = 1.0_wp - zhwt1
               
            
            elseif (zyt(2) < ztheta_min) then            
            
               zhwt1 = 1.0_wp
               zhwt2 = 0.0_wp
               
            elseif (zyt(2) > ztheta_max) then
            
               zhwt1 = 0.0_wp
               zhwt2 = 1.0_wp
               
            endif    


            if ( j == istep(i)) then
            
               if (ABS(zhuse(i,j)-zhmax)  < 1.0e-3_wp*SPACING(zhmin)) zhuse_hat = 0.0_wp  
               if (ABS(zhuse(i,j)-zhmin)  < 1.0e-3_wp*SPACING(zhmin)) zhuse_hat = 0.0_wp  
                        
               zh_hat = zh_hat + zhuse_hat
                              
               zyt_hat(1) = zyt_hat(1) - zhuse_hat/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))
               
               zrtan_hat = zrtan_hat - zhuse_hat/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))
               
               zrad_hat = zrad_hat + zhuse_hat/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))
               
               zdydh_hat(1) = zdydh_hat(1) + zhuse_hat* &
           &   (zyt(1)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))**2
               
               zdr_dtheta_hat = zdr_dtheta_hat - zhuse_hat*zdydh(2,i,j)* &
           &   (zyt(1)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))**2
               
               zdydh_hat(2) = zdydh_hat(2) - zhuse_hat*zdr_dtheta(i)* &
           &   (zyt(1)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))**2        
               zhuse_hat = 0.0_wp
                
               
               if (zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                               
!                  dr_dtheta_prime = &
!                &  (radius_prime(i+1,kp1)-radius_prime(i+1,k))/pdsep
        
                  pradius_hat(i+1,ikp1) = pradius_hat(i+1,ikp1) + zdr_dtheta_hat/pdsep
                  pradius_hat(i+1,ik) = pradius_hat(i+1,ik) - zdr_dtheta_hat/pdsep
                  zdr_dtheta_hat = 0.0_wp
                  
               endif
               
               
               zdr_dtheta_hat = 0.0_wp               
                       
                
            else
               
               zh_hat = zh_hat + zhuse_hat
               zhuse_hat = 0.0_wp
                                   
            endif 
            


! radius at current position

      
!            rad_prime = hwt1*radius_prime(i+1,k)+hwt2*radius_prime(i+1,kp1) + &
!          &  hwt1_prime*radius(i+1,k)+hwt2_prime*radius(i+1,kp1)
            
            pradius_hat(i+1,ik) = pradius_hat(i+1,ik) + zhwt1*zrad_hat
            pradius_hat(i+1,ikp1) = pradius_hat(i+1,ikp1) + zhwt2*zrad_hat
            zhwt1_hat = zhwt1_hat + pradius(i+1,ik)*zrad_hat
            zhwt2_hat = zhwt2_hat + pradius(i+1,ikp1)*zrad_hat
            zrad_hat = 0.0_wp

            
             if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
!               hwt1_prime = -yt_prime(2)/pdsep
!               hwt2_prime = - hwt1_prime 
               
               zhwt1_hat = zhwt1_hat - zhwt2_hat
               zhwt2_hat = 0.0_wp
               
               zyt_hat(2) = zyt_hat(2) - zhwt1_hat/pdsep
               zhwt1_hat = 0.0_wp
               
            
            elseif (zyt(2) < ztheta_min) then            
                           
               zhwt1_hat = 0.0_wp
               zhwt2_hat = 0.0_wp
               
            elseif (zyt(2) > ztheta_max) then
            
               zhwt1_hat = 0.0_wp
               zhwt2_hat = 0.0_wp
               
            endif    

!            yt_prime(:) = y_prime(:) + dydh(:)*h_prime + dydh_prime(:)*h
 
!            yt_hat = 0.0
 
 
            zy_hat(:) = zy_hat(:) +  zyt_hat(:)
            zdydh_hat(:) = zdydh_hat(:) + zyt_hat(:)*zh(i,j)
            do jj = 1,4
               zh_hat = zh_hat + zdydh(jj,i,j)*zyt_hat(jj)
            enddo
            zyt_hat(:) = 0.0_wp   
    


            do jj = 2,1,-1 !  stuff 


               zyt(:) = zy(:,i,j)
               if (jj == 2) zyt(:) = zyt_save(:,i,j)
       
        
! where are we in the plane    
                   
                ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
                ik = MIN(MAX(1,ik),khoriz-1)
                ikp1 = ik+1
         
! horizontal weighting factor               
            
                if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
                    zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
                    zhwt2 = 1.0_wp - zhwt1
            
                 elseif (zyt(2) < ztheta_min) then            
             
                    zhwt1 = 1.0_wp
                    zhwt2 = 0.0_wp
               
                 elseif (zyt(2) > ztheta_max) then
                       
                    zhwt1 = 0.0_wp
                    zhwt2 = 1.0_wp
               
                 endif    

!
! interpolate refractivity and radius to point in plane.
!
     
                 zref_up  = zhwt1*prefrac(i+1,ik)+zhwt2*prefrac(i+1,ikp1)
                 zref_low = zhwt1*prefrac(i,ik) + zhwt2*prefrac(i,ikp1)

                 zrad_up = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)
                 zrad_low = zhwt1*pradius(i,ik) + zhwt2*pradius(i,ikp1)
    
! estimate radial gradient
    
                 if ((zref_up - zref_low) > -1.0e-10_wp) then
      
                    zdndr2 = 1.0e-6_wp*(zref_up-zref_low)/(zrad_up-zrad_low)  ! +ve refrac gradient
    
                 else
    
                     zkval_theta = LOG(zref_low/zref_up)/(zrad_up-zrad_low)

                     zed = MAX(0.0_wp,(zyt(1)+zrtan-zrad_low))
          
                     zdndr = - 1.0e-6_wp*zkval_theta*zref_low*EXP(-zkval_theta*zed)

                     zdndr2 = MAX(-0.75e-7_wp,zdndr)
  
                 endif

! adjoint bit

                 
                 zyt_hat(3) = zyt_hat(3) - COS(zyt(3))*zdndr2*zdydh_hat(4)
                 zdndr2_hat = zdndr2_hat - SIN(zyt(3))*zdydh_hat(4)              
                 zdydh_hat(4) = 0.0_wp
 
                 zyt_hat(3) = zyt_hat(3) - COS(zyt(3))*(1.0_wp/(zyt(1)+zrtan) + zdndr2)*zdydh_hat(3)
                 zdndr2_hat = zdndr2_hat -  SIN(zyt(3))*zdydh_hat(3)
                 zyt_hat(1) = zyt_hat(1) + SIN(zyt(3))/ (zyt(1)+zrtan)**2*zdydh_hat(3)
                 zrtan_hat  = zrtan_hat + SIN(zyt(3))/ (zyt(1)+zrtan)**2*zdydh_hat(3)
                 zdydh_hat(3) = 0.0_wp
 
 
                 if ((zref_up - zref_low) > -1.0e-10) then

!
!!       zdndr2_prime = 1.0e-6_wp*(zref_up_prime-zref_low_prime)/(zrad_up-zrad_low) - &
!!       &            zdndr2/(zrad_up-zrad_low)*(zrad_up_prime-zrad_low_prime)
!                  
     
                     zref_up_hat = zref_up_hat + 1.0e-6_wp*zdndr2_hat/(zrad_up-zrad_low)
                     zref_low_hat = zref_low_hat - 1.0e-6_wp*zdndr2_hat/(zrad_up-zrad_low)
                     zrad_up_hat = zrad_up_hat - zdndr2/(zrad_up-zrad_low)*zdndr2_hat
                     zrad_low_hat = zrad_low_hat + zdndr2/(zrad_up-zrad_low)*zdndr2_hat
                     zdndr2_hat = 0.0_wp

                 else
  
  
                     if (zdndr > -0.75e-7_wp) then


                         zdndr_hat = zdndr_hat + zdndr2_hat
                         zdndr2_hat = 0.0_wp
 
                     else
     
                         zdndr2_hat = 0.0_wp

                     endif

! gradient
                      
                     zkval_theta_hat = zkval_theta_hat + zdndr*(1.0_wp/zkval_theta - zed)*zdndr_hat
                     zref_low_hat = zref_low_hat + zdndr/zref_low*zdndr_hat
                     zed_hat = zed_hat - zdndr*zkval_theta*zdndr_hat
                     zdndr_hat = 0.0_wp
     
                     if ((zyt(1)+zrtan-zrad_low) >  0.0_wp) then

!!!!                         zed_prime = zyt_prime(1) + zrtan_prime - zrad_low_prime
 
                         zyt_hat(1) = zyt_hat(1) + zed_hat
                         zrtan_hat = zrtan_hat + zed_hat
                         zrad_low_hat = zrad_low_hat - zed_hat
                         zed_hat = 0.0_wp 
   
                     else   

                         zed_hat = 0.0_wp

                     endif 
! kval

!!                       zkval_theta_prime = (zref_low_prime/zref_low - zref_up_prime/zref_up + &
!!            &          zkval_theta*(zrad_low_prime - zrad_up_prime))/(zrad_up-zrad_low)

                       
                     zref_low_hat = zref_low_hat + zkval_theta_hat/(zref_low*(zrad_up-zrad_low))
                     zref_up_hat  = zref_up_hat  - zkval_theta_hat/(zref_up*(zrad_up-zrad_low))
                     zrad_low_hat = zrad_low_hat + zkval_theta/(zrad_up-zrad_low)*zkval_theta_hat
                     zrad_up_hat  = zrad_up_hat -  zkval_theta/(zrad_up-zrad_low)*zkval_theta_hat
                     zkval_theta_hat = 0.0_wp
     
     
                 endif
               

                 pradius_hat(i,ik)=pradius_hat(i,ik)+zhwt1*zrad_low_hat
                 zhwt1_hat = zhwt1_hat + pradius(i,ik)* zrad_low_hat
                 pradius_hat(i,ikp1)=pradius_hat(i,ikp1)+zhwt2*zrad_low_hat
                 zhwt2_hat = zhwt2_hat + pradius(i,ikp1)* zrad_low_hat  
                 zrad_low_hat = 0.0_wp
   
                 pradius_hat(i+1,ik)=pradius_hat(i+1,ik)+zhwt1*zrad_up_hat
                 zhwt1_hat = zhwt1_hat + pradius(i+1,ik)* zrad_up_hat
                 pradius_hat(i+1,ikp1)=pradius_hat(i+1,ikp1)+zhwt2*zrad_up_hat
                 zhwt2_hat = zhwt2_hat + pradius(i+1,ikp1)* zrad_up_hat
                 zrad_up_hat = 0.0_wp

                 prefrac_hat(i,ik) = prefrac_hat(i,ik) + zhwt1*zref_low_hat
                 zhwt1_hat = zhwt1_hat + prefrac(i,ik)*zref_low_hat
                 prefrac_hat(i,ikp1) = prefrac_hat(i,ikp1) + zhwt2*zref_low_hat
                 zhwt2_hat = zhwt2_hat + prefrac(i,ikp1)*zref_low_hat
                 zref_low_hat = 0.0_wp
 
                 prefrac_hat(i+1,ik) = prefrac_hat(i+1,ik) + zhwt1*zref_up_hat
                 zhwt1_hat = zhwt1_hat + prefrac(i+1,ik)*zref_up_hat
                 prefrac_hat(i+1,ikp1) = prefrac_hat(i+1,ikp1) + zhwt2*zref_up_hat
                 zhwt2_hat = zhwt2_hat + prefrac(i+1,ikp1)*zref_up_hat
                 zref_up_hat = 0.0_wp
 
 
                 zdydh_tmp = zamult*SIN(zyt(3))/(zyt(1)+zrtan)
                 
                 zyt_hat(3)=zyt_hat(3)+zamult*COS(zyt(3))/(zyt(1)+zrtan)*zdydh_hat(2)
                 zyt_hat(1)=zyt_hat(1)-zdydh_tmp/(zyt(1)+zrtan)*zdydh_hat(2)
                 zrtan_hat = zrtan_hat-zdydh_tmp/(zyt(1)+zrtan)*zdydh_hat(2)
                 zdydh_hat(2)=0.0_wp
 
    
                 if (COS(zyt(3)) > 1.0e-10_wp) then
   
                      zyt_hat(3) =zyt_hat(3) - SIN(zyt(3))*zdydh_hat(1)
                      zdydh_hat(1)=0.0_wp

                 else
      
                      zdydh_hat(1) = 0.0_wp
             
                 endif

           
                 if ( zyt(2) < ztheta_max .and. zyt(2) > ztheta_min) then
                           
                       zhwt1_hat = zhwt1_hat - zhwt2_hat
                       zhwt2_hat = 0.0_wp
       
                       zyt_hat(2) = zyt_hat(2) - zhwt1_hat/pdsep
                       zhwt1_hat = 0.0_wp
 
   
                 elseif (zyt(2) < ztheta_min) then
       
                       zhwt1_hat = 0.0_wp
                       zhwt2_hat = 0.0_wp
              
                 elseif (zyt(2) > ztheta_max) then
                       
                       zhwt1_hat = 0.0_wp
                       zhwt2_hat= 0.0_wp
              
                 endif    


                 if (jj == 2) then
     
                     zyt_hat(:)=zyt_hat(:)
     
                     zh2 = 0.5_wp*zh(i,j)
     
                     zdydh_hat(:) = zdydh_hat(:)+zh2*zyt_hat(:)
     
                     do jdum=1,4
                        zh2_hat = zh2_hat+zdydh_save(jdum,i,j)*zyt_hat(jdum)
                     enddo
     
                 endif 

                 zy_hat(:) = zy_hat(:) + zyt_hat(:)
                 zyt_hat(:) = 0.0_wp
 
           
             enddo ! jj
                      
           
         enddo ! j
      
      
         zh_hat = zh_hat + 0.5_wp*zh2_hat
         zh2_hat = 0.0_wp

         
! had problems with this line of code         
         

         if (ABS(zh(i,1) - zhmax) < 1.0e3_wp*SPACING(zh(i,1))) zh_hat = 0.0_wp
         if (ABS(zh(i,1) - zhmin) < 1.0e3_wp*SPACING(zh(i,1))) zh_hat = 0.0_wp
                           
         if (i == ibot) then

            ik = ikcen
            zdr_max = (pradius(i+1,ik)- zrtan)/REAL(isplit)                        
         
            zdr_max_hat = zdr_max_hat + pradius(ibot,ik)/zh(i,1)*zh_hat
            pradius_hat(ibot,ik) = pradius_hat(ibot,ik) + zdr_max/zh(i,1)*zh_hat            
            zh_hat = 0.0_wp
                        
            pradius_hat(i+1,ik) = pradius_hat(i+1,ik) + zdr_max_hat/REAL(isplit)
            zrtan_hat = zrtan_hat - zdr_max_hat/REAL(isplit)
            zdr_max_hat = 0.0_wp 
            
            
         else

            ik = INT((zy(2,i,1) + ztheta_tan)/pdsep)+1
            ik = MIN(MAX(1,ik),khoriz)
                 zdr_max = (pradius(i+1,ik)-pradius(i,ik))/REAL(isplit)

! physical limit of step
            
            if (ABS(zh(i,1) - zh_up(i)) < 1.0e3_wp*SPACING(zh(i,1))) then
            
               zh_up_hat = zh_up_hat + zh_hat
               zh_hat = 0.0_wp
               
            endif   
                           
           
            if (COS(zy(3,i,1)) < 1.0e-10_wp) then
            
!!                h_prime = 1.0e10*dr_max_prime
                
                zdr_max_hat = zdr_max_hat + 1.0e10_wp*zh_hat
                zh_hat = 0.0_wp
                
            else 
            
!!               h_prime = h/dr_max*dr_max_prime + h*TAN(y(3))*y_prime(3)                          
               
               zdr_max_hat = zdr_max_hat + zh(i,1)/zdr_max*zh_hat
               zy_hat(3) = zy_hat(3) + zh(i,1)*TAN(zy(3,i,1))*zh_hat
               zh_hat = 0.0_wp
                                             
            endif 

! new limit on step size 

            zdr_max_hat = zdr_max_hat + pradius(i,ik)/zh_up(i)*zh_up_hat
            pradius_hat(i,ik) = pradius_hat(i,ik) + zdr_max/zh_up(i)*zh_up_hat            
            zh_up_hat = 0.0_wp
           
            pradius_hat(i+1,ik) = pradius_hat(i+1,ik) + zdr_max_hat/REAL(isplit)
            pradius_hat(i,ik) = pradius_hat(i,ik) - zdr_max_hat/REAL(isplit)
            zdr_max_hat = 0.0_wp 
             

         endif
         
         
         endif ! 1d or 2d
           
            
      enddo ! i
      
      zy_hat(:) = 0.0_wp   

      pa_path_hat(in,iside) = 0.0_wp  


! don't do iside = 2 if its a 1d calculation

      if (llone_d_calc) exit

   enddo ! iside

   zalpha_half_hat(:) = 0.0_wp
 
! might have been over-written

   zrad(1,1) = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))

! if zrtan was close to upper model level

   if (ibot_old /= ibot) then
   
      pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + zrtan_hat
      
      zrtan_hat = 0.0_wp
   
   endif 
    
   if ( zrad(1,1)*zdndr > -1.0_wp) then
  
!
! need to recalculate zdndr
!
       
       zdndr = 1.0e-6_wp*(prefrac(ibot+1,ikcen)-prefrac(ibot,ikcen))/ &
         & (pradius(ibot+1,ikcen)-pradius(ibot,ikcen)) 
   
    
  
!       rtan_prime = radius_prime(ibot,kcen) - &
!                  &  (nr_prime(ibot,kcen) + &
!                  &  (a(n)-nr(ibot,kcen))/(1.0 + rad(1,1)*dndr)*(rad(1,1)*dndr_prime + dndr*rad_prime))/&  
!                  &  (1.0 + rad(1,1)*dndr)   

       pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + zrtan_hat 
       pnr_hat(ibot,ikcen) = pnr_hat(ibot,ikcen) - zrtan_hat/(1.0_wp + zrad(1,1)*zdndr)
       
       zdndr_hat = zdndr_hat - &
    &  (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad(1,1)*zdndr)**2*zrad(1,1)*zrtan_hat        
       zrad_hat = zrad_hat - &
    &  (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad(1,1)*zdndr)**2*zdndr*zrtan_hat
       zrtan_hat = 0.0_wp
       
       
   else
       
       zrad_hat = zrad_hat + zrtan_hat
       zrtan_hat = 0.0_wp       
       
   endif                
  
  
!!  dndr_hat = 0.0
  
!   dndr_prime = (1.0e-6*(refrac_prime(ibot+1,kcen)-refrac_prime(ibot,kcen)) -  &
!               &  dndr*(radius_prime(ibot+1,kcen)-radius_prime(ibot,kcen))) &                 
!               &  /(radius(ibot+1,kcen)-radius(ibot,kcen))
 
   prefrac_hat(ibot+1,ikcen) = prefrac_hat(ibot+1,ikcen) + &
 &  1.0e-6_wp*zdndr_hat/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))

   prefrac_hat(ibot,ikcen) = prefrac_hat(ibot,ikcen) - &
 &  1.0e-6_wp*zdndr_hat/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))

   pradius_hat(ibot+1,ikcen) = pradius_hat(ibot+1,ikcen) - &
 &  zdndr/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))*zdndr_hat

   pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + &
 &  zdndr/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))*zdndr_hat
   zdndr_hat = 0.0_wp

   pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + 0.5_wp*zrad_hat
   pradius_hat(ibot+1,ikcen) = pradius_hat(ibot+1,ikcen) + 0.5_wp*zrad_hat
   zrad_hat = 0.0_wp
   

enddo obloop ! all the observations

palpha_hat(:) = 0.0_wp
pa_path_hat(:,:) = 0.0_wp



! adjoint of the kvals


!!!kval_hat = 0.0

do i = 1,klev-1

   do j = 1,khoriz
      
    if (prefrac(i,j) > 0.0_wp .and. prefrac(i+1,j) > 0.0_wp) then
      
      if (zkval_save(i,j) >= dn_dx_max/prefrac(i,j)) then
      
          prefrac_hat(i,j)=prefrac_hat(i,j)-dn_dx_max/prefrac(i,j)**2*zkval_hat(i,j)
          zkval_hat(i,j)=0.0_wp

      endif
      
         
      if (zkval_save(i,j) > 1.0e-6_wp) then
      
        pnr_hat(i,j) = pnr_hat(i,j) + &
      &        zkval(i,j)/MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j)))*zkval_hat(i,j)
        pnr_hat(i+1,j) = pnr_hat(i+1,j) - &
      &        zkval(i,j)/MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j)))*zkval_hat(i,j)
        prefrac_hat(i,j) = prefrac_hat(i,j) + &
      &        zkval_hat(i,j)/(prefrac(i,j)*MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j))))
        prefrac_hat(i+1,j) = prefrac_hat(i+1,j) - &
      &        zkval_hat(i,j)/(prefrac(i+1,j)*MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j))))        

        
        zkval_hat(i,j) = 0.0_wp                                
                    
      else
      
        zkval_hat(i,j) = 0.0_wp
            
      endif        
      
    endif        
      
   enddo
   
enddo   

zkval_hat(:,:) = 0.0_wp

end subroutine ropp_fm_alpha2drk_ec_ad
