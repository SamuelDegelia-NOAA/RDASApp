! $Id$

!****s* BendingAngle2d/ropp_fm_alpha2drk_tl *
!
! NAME
!    ropp_fm_alpha2drk - Forward model calculating a bending
!                        angle profile from planar information.
!
! SYNOPSIS
!    call ropp_fm_alpha2drk_tl(kobs, klev, ...)
! 
! DESCRIPTION
!    This routine is a forward model calculating the bending angle profile
!    from planar refractivity information.  
!
! INPUTS
!
!           kobs   =  number of observed bending angles 
!           klev   =  number of vertical levels
!           khoriz =  number of horizontal locations
!           ksplit =  splitting of model levels
!           pdsep  =  angular spacing
!           pa     =  impact parameters
!           proc   =  radius of curvature 
!           pz_2d  =  2D impact height (do a 1D calculation above pz_2d)
!           prefrac=  refractivity values
!           pradius=  radius values
!           pnr    =  nr product 
!
! OUTPUT
! 
!           pa_path = impact parameter at end points of ray path
!           palpha  = bending angle values 
!           pa_path_prime = TL of impact parameter at end points of ray path
!           palpha_prime  = TL of bending angle values 
!
! NOTES
!    The forward model calculate the bending angle as a function of
!    impact parameter. Below "pz_2d" a Runge-Kutta solver is used
!    calculate the ray-path and bending angle. Above "pz_2d" we use
!    the 1D method, based on the error function solution of the 
!    bending angle integral.  
!
! SEE ALSO
!    ropp_fm_types
!
! AUTHOR
!   ECMWF, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****


SUBROUTINE ropp_fm_alpha2drk_tl(kobs,   & ! no.of observations
                       klev,   & ! no. of vertical levels
		       khoriz, & ! no. of horizontal layers  ODD
		       ksplit, &
		       pdsep,   & ! the angular spacing 
                       pa,      & ! impact parameter values
   		       prefrac, & ! refractivity
		       prefrac_prime, &
		       pradius, & ! radius values
		       pradius_prime, &
		       pnr,     &
		       pnr_prime, &
		       proc, &
		       pz_2d, &
		       pa_path, &
		       pa_path_prime, &
		       palpha, &
		       palpha_prime)       ! partial path length along rays



USE typesizes, ONLY: wp => EightByteReal
USE ropp_utils, ONLY: ropp_MDFV
USE ropp_fm_constants, ONLY : pi



IMPLICIT NONE

!
! subroutine args. 
!

INTEGER, INTENT(IN)  :: kobs           ! size of ob. vector
INTEGER, INTENT(IN)  :: klev           ! no. of refractivity levels
INTEGER, INTENT(IN)  :: khoriz         ! no. of horizontal locations
INTEGER, INTENT(IN)  :: ksplit
REAL(KIND=wp),    INTENT(IN)  :: pdsep           ! angular spacing of grid
REAL(KIND=wp),    INTENT(IN)  :: pa(kobs)        ! impact parameter - NOW assumed to be on pressure levels
REAL(KIND=wp),    INTENT(IN)  :: prefrac(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(IN)  :: prefrac_prime(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(IN)  :: pradius(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(IN)  :: pradius_prime(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(IN)  :: pnr(klev,khoriz)
REAL(KIND=wp),    INTENT(IN)  :: pnr_prime(klev,khoriz)
REAL(KIND=wp),    INTENT(IN)  :: proc
REAL(KIND=wp),    INTENT(IN)  :: pz_2d
REAL(KIND=wp),    INTENT(OUT) :: pa_path_prime(kobs,2)        
REAL(KIND=wp),    INTENT(OUT) :: palpha_prime(kobs)   ! path length
REAL(KIND=wp),    INTENT(OUT) :: pa_path(kobs,2)        
REAL(KIND=wp),    INTENT(OUT) :: palpha(kobs)   ! path length

		       
!
! local variables
!

INTEGER :: i,j,in,ibot,jbot,ikbot,iside,ik,ikp1,in_2d
INTEGER :: ikcen
REAL(KIND=wp), PARAMETER :: zhmax = 3.0E4_wp
REAL(KIND=wp), PARAMETER :: zhmin = 1.0E2_wp
REAL(KIND=wp) :: zrad,zdndr
REAL(KIND=wp) :: zrad_prime,zdndr_prime
REAL(KIND=wp) :: zhwt1,zhwt2
REAL(KIND=wp) :: zhwt1_prime,zhwt2_prime
REAL(KIND=wp) :: zamult
REAL(KIND=wp) :: zh,zh2,zhuse,zhnew,zh_up
REAL(KIND=wp) :: zh_prime,zh2_prime,zhuse_prime,zhnew_prime,zh_up_prime
REAL(KIND=wp) :: zy(4),zyt(4)
REAL(KIND=wp) :: zy_prime(4),zyt_prime(4)
REAL(KIND=wp) :: zdydh(4),zdydht(4,4)
REAL(KIND=wp) :: zdydh_prime(4),zdydht_prime(4,4)
REAL(KIND=wp) :: ztheta_tan,ztheta_min,ztheta_max
REAL(KIND=wp) :: zdr_max,zdr_dtheta,zrtan,zdr
REAL(KIND=wp) :: zdr_max_prime,zdr_dtheta_prime,zrtan_prime,zdr_prime
REAL(KIND=wp) :: zalpha_half(2)
REAL(KIND=wp) :: zalpha_half_prime(2)
REAL(KIND=wp) :: zkval(klev-1,khoriz)
REAL(KIND=wp) :: zkval_prime(klev-1,khoriz)
REAL(KIND=wp) :: ztlow,ztup,zdalpha,zRoot_halfPI
REAL(KIND=wp) :: ztlow_prime,ztup_prime,zdalpha_prime
REAL(KIND=wp) :: zerf_up,zerf_low,zt,zdiff_erf,znr_low,zref_low,zaval
REAL(KIND=wp) :: zerf_up_prime,zerf_low_prime,zt_prime,zdiff_erf_prime,znr_low_prime, &
      &  zref_low_prime,zaval_prime
LOGICAL :: lfirst_1d,LEAVING,lone_d_calc

!
! the central profile kcen
!

ikcen = khoriz/2 + 1
ztheta_tan = REAL(ikcen-1)*pdsep 
ztheta_min = -ztheta_tan
ztheta_max =  ztheta_tan

DO i = 1,klev-1

   DO j = 1, khoriz
   
      zkval(i,j) = LOG(prefrac(i,j)/prefrac(i+1,j))/MAX((pnr(i+1,j) - pnr(i,j)),1.0_wp)
      
      IF (zkval(i,j) > 1.0E-6_wp) THEN
      
        zkval_prime(i,j) = ((zkval(i,j)*(pnr_prime(i,j)-pnr_prime(i+1,j))) + &
                  & (prefrac_prime(i,j)/prefrac(i,j)-              &
		  &  prefrac_prime(i+1,j)/prefrac(i+1,j)))/        &
                  &  MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j)))
		    
      ELSE
      
        zkval(i,j)=1.0E-6_wp
        zkval_prime(i,j) = 0.0_wp
            
      ENDIF        
      
  ENDDO
   
ENDDO   

!
! set n_2d level. For levels below n_2d we do 2D ray bending calculation
! above n_2d we do the 1D calculation
!

in_2d = 0 

DO WHILE ((pnr(in_2d+1,ikcen)-proc < pz_2d) .AND. (in_2d < klev - 1)) 

    in_2d = in_2d + 1
    
ENDDO    

jbot = 1

DO

  IF (prefrac(jbot,ikcen) > 0.0_wp .AND. pnr(jbot,ikcen) > 0.0_wp) EXIT
  
  jbot = jbot + 1

ENDDO

ikbot = klev

DO i=klev,jbot+1,-1

   IF (pnr(ikbot,ikcen) < pnr(ikbot-1,ikcen)) EXIT 

   ikbot = ikbot - 1

ENDDO
 
jbot = MAX(jbot,ikbot)

!
! set the outputs to missing
!

palpha(:)=ropp_MDFV
palpha_prime(:) = 0.0_wp
pa_path(:,:)=ropp_MDFV
pa_path_prime(:,:) = 0.0_wp

zRoot_halfPI = SQRT(0.5_wp*pi)

OBLOOP: DO in=1,kobs
   
   IF (pa(in) < pnr(jbot,ikcen) .OR. pa(in) >= pnr(klev,ikcen)) CYCLE  
      
   ibot = jbot

   DO 

      IF (pnr(ibot+1,ikcen) - pa(in) > 1.0_wp) EXIT   ! assuming "a" is on one of the pressure levels

      ibot=ibot+1

   ENDDO

!
! calculate the radius at tangent point   
!   
   zrad = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))   
   zrad_prime = 0.5_wp*(pradius_prime(ibot,ikcen)+pradius_prime(ibot+1,ikcen))
   
   zdndr = 1.0E-6_wp*(prefrac(ibot+1,ikcen)-prefrac(ibot,ikcen))/ &
                & (pradius(ibot+1,ikcen)-pradius(ibot,ikcen)) 
  
   zdndr_prime = (1.0E-6_wp*(prefrac_prime(ibot+1,ikcen)-prefrac_prime(ibot,ikcen)) -  &
 & zdndr*(pradius_prime(ibot+1,ikcen)-pradius_prime(ibot,ikcen)))/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))


 
   IF ( zrad*zdndr > -1.0_wp) THEN
  
       zrtan = pradius(ibot,ikcen) + &
           &  (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad*zdndr)


       zrtan_prime = pradius_prime(ibot,ikcen) - &
                 &   (pnr_prime(ibot,ikcen) + &
		 &   (pa(in)-pnr(ibot,ikcen))/ &
                 &    (1.0_wp + zrad*zdndr)*(zrad*zdndr_prime + zdndr*zrad_prime))/&  
                 &   (1.0_wp + zrad*zdndr)   

   ELSE
   
       zrtan = zrad   ! probably in a super-refracting layer
       
       zrtan_prime = zrad_prime
       
       
   ENDIF      	  

! if zrtan is within a 1 m of upper level set to upper level
   
   IF ((zrtan - pradius(ibot+1,ikcen)) > -1.0_wp) THEN
   
       ibot = ibot + 1
   
       zrtan = pradius(ibot,ikcen)
       
       zrtan_prime = pradius_prime(ibot,ikcen) 
              
   ENDIF     


!
!  set bending angle value  
!   

   zalpha_half(:) = 0.0_wp   
   zalpha_half_prime(:) = 0.0_wp

   DO iside = 1,2 
   
 
      pa_path(in,iside) = pa(in)
      pa_path_prime(in,iside) = 0.0_wp
      lfirst_1d = .TRUE.
      lone_d_calc = .FALSE.        
      zamult = 1.0_wp
      IF (iside == 2) zamult  = -1.0_wp
      
!
! initialise vector
!

      zy(1) = 0.0_wp           ! height above tangent point           
      zy(2) = 0.0_wp           ! theta
      zy(3) = ASIN(1.0_wp)     ! thi
      zy(4) = 0.0_wp           ! bending angle 
      
      
      zy_prime(:) = 0.0_wp
      
        
      DO i = ibot,klev-1
	
	
	 IF ( i < MIN(in_2d,klev-1) .AND. khoriz > 1) THEN
	
           IF (i == ibot) THEN
	 
	      ik = ikcen
              zdr_max = (pradius(i+1,ik)- zrtan)/ksplit	    	    
	      zdr_max_prime = (pradius_prime(i+1,ik)-zrtan_prime)/REAL(ksplit)
              zh = SQRT(2.0_wp*pradius(ibot,ik)*zdr_max)		    
	      zh_prime = &
	     &(pradius(ibot,ik)*zdr_max_prime + zdr_max*pradius_prime(ibot,ik))/zh
	    	    
           ELSE

              ik = INT((zy(2) + ztheta_tan)/pdsep)+1
	      ik = MIN(MAX(1,ik),khoriz)
     	      zdr_max = (pradius(i+1,ik)-pradius(i,ik))/REAL(ksplit)
              zdr_max_prime = (pradius_prime(i+1,ik)-pradius_prime(i,ik))/REAL(ksplit)
	    
	      IF (zdr_max < 0.0_wp .OR. pradius(i,ik) < 0.0_wp) THEN
	      
	          WRITE (*,*) 'gpsro -neg error',i,ibot,ik,ksplit,zdr_max
		  WRITE (*,*) 'gpsto -neg2',pradius(i+1,ik),pradius(i,ik),proc
	    
	      ENDIF 
	    
	    
	      zh_up = SQRT(2.0_wp*pradius(i,ik)*zdr_max)
	      zh_up_prime = &
	     &(pradius(i,ik)*zdr_max_prime + zdr_max*pradius_prime(i,ik))/zh_up
	      	    
	      zh = zdr_max/MAX(COS(zy(3)),1.0E-10_wp)
	    

              IF (COS(zy(3)) < 1.0E-10_wp) THEN
	    
	         zh_prime = 1.0E10_wp*zdr_max_prime
		
	      ELSE 
	    
	         zh_prime = zh/zdr_max*zdr_max_prime + zh*TAN(zy(3))*zy_prime(3)   	
	       
	      ENDIF 

! use zh_up when cos(phi) cose to 0.0
	      
	      IF (zh > zh_up) THEN
	      
	          zh = zh_up
		  zh_prime = zh_up_prime
		  
	      ENDIF  
	       

           ENDIF

! limit the step-length

 	  IF (zh > zhmax) THEN
	 
	     zh = zhmax
	     zh_prime = 0.0_wp
	    
	  ELSE IF (zh < zhmin) THEN
	 
	     zh = zhmin    
	     zh_prime = 0.0_wp
	 
	  ENDIF 


	   zh2 = 0.5_wp*zh	 
	   zh2_prime = 0.5_wp*zh_prime
	 
!
! now calculate the path-length with a RUNGE-KUTTA
!

           LEAVING=.FALSE.
	 
	   DO j = 1,ksplit	  
	    
	      zyt(:) = zy(:)	    
	      zyt_prime(:) = zy_prime(:)  
	  	    
	    
! first calculation of derivs

	      CALL ropp_fm_gpspderivs_tl(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	     &zrtan,zrtan_prime,zamult,prefrac,prefrac_prime,pradius,pradius_prime,zyt, &
	     &zyt_prime,zdydht(:,1),zdydht_prime(:,1)) 
	
	
	      zyt(:) = zy(:) + zdydht(:,1)*zh2	    
	      zyt_prime(:) = zy_prime(:) + zdydht(:,1)*zh2_prime + zdydht_prime(:,1)*zh2
	    
	  	    
! second call at new yt
	    
	      CALL ropp_fm_gpspderivs_tl(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	     & zrtan,zrtan_prime,zamult,prefrac,prefrac_prime,pradius,pradius_prime,zyt, &
	     & zyt_prime,zdydht(:,2),zdydht_prime(:,2)) 
 
              zyt(:) = zy(:) + zdydht(:,2)*zh2
	      zyt_prime(:) = zy_prime(:) + zdydht(:,2)*zh2_prime + zdydht_prime(:,2)*zh2

	    	    	    
! third call at new yt
	    
	      CALL ropp_fm_gpspderivs_tl(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	     & zrtan,zrtan_prime,zamult,prefrac,prefrac_prime,pradius,pradius_prime,zyt, &
	     & zyt_prime,zdydht(:,3),zdydht_prime(:,3)) 

              zyt(:) = zy(:) + zdydht(:,3)*zh
	      zyt_prime(:) = zy_prime(:) + zdydht(:,3)*zh_prime + zdydht_prime(:,3)*zh
	    
	    	    
! fourth last call

	      CALL ropp_fm_gpspderivs_tl(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	     & zrtan,zrtan_prime,zamult,prefrac,prefrac_prime,pradius,pradius_prime,zyt, &
	     & zyt_prime,zdydht(:,4),zdydht_prime(:,4)) 
	    	     	 
              zdydh(:) = (zdydht(:,1)+zdydht(:,4)+2.0_wp*(zdydht(:,2)+zdydht(:,3)))/6.0_wp

	      zdydh_prime(:) = &
	     & (zdydht_prime(:,1)+zdydht_prime(:,4)+ &
	     & 2.0_wp*(zdydht_prime(:,2)+zdydht_prime(:,3)))/6.0_wp
	   
	      
	      zyt(:) = zy(:) + zdydh(:)*zh	    
	      zyt_prime(:) = zy_prime(:) + zdydh(:)*zh_prime + zdydh_prime(:)*zh
	    
	    
!	      yt_prime = 0.0
	    
!
! check the radius - have we exited the level
!	    
	    
	      ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
	      ik = MIN(MAX(1,ik),khoriz-1)
	      ikp1 = ik+1
	 
! horizontal weighting factor	       
	    
	      IF ( zyt(2) < ztheta_max .AND. zyt(2) > ztheta_min) THEN
	    	       
                 zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
	         zhwt2 = 1.0_wp - zhwt1
	       
	         zhwt1_prime = -zyt_prime(2)/pdsep
	         zhwt2_prime = - zhwt1_prime 
	    
	      ELSE IF (zyt(2) < ztheta_min) THEN	    
	    
	         zhwt1 = 1.0_wp
	         zhwt2 = 0.0_wp
	       
	         zhwt1_prime = 0.0_wp
	         zhwt2_prime = 0.0_wp
	       
	      ELSE IF (zyt(2) > ztheta_max) THEN
	    
	         zhwt1 = 0.0_wp
	         zhwt2 = 1.0_wp
	       
	         zhwt1_prime = 0.0_wp
	         zhwt2_prime = 0.0_wp
	       
	      ENDIF    

	 
! radius of pressure level
	     
	      zrad = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)
	    
	      zrad_prime = zhwt1*pradius_prime(i+1,ik)+zhwt2*pradius_prime(i+1,ikp1) + &
	     & zhwt1_prime*pradius(i+1,ik)+zhwt2_prime*pradius(i+1,ikp1)
	       
	    
! if gone over the boundary scale h
	    
	      IF ( j == ksplit .OR. (zy(1)+zrtan - zrad) > 0.0_wp) THEN
	    
	    
	         LEAVING = .TRUE. 
		  
	         zdr_dtheta = 0.0_wp
	         zdr_dtheta_prime = 0.0_wp 
	    
	         IF (zyt(2) < ztheta_max .AND. zyt(2) > ztheta_min) THEN
	        
		    zdr_dtheta = (pradius(i+1,ikp1)-pradius(i+1,ik))/pdsep
	       
	            zdr_dtheta_prime = &
	           & (pradius_prime(i+1,ikp1)-pradius_prime(i+1,ik))/pdsep
	
	         ENDIF        	
			    
	         zhuse = zh - (zyt(1)+zrtan-zrad)/(zdydh(1) - zdr_dtheta*zdydh(2))
	    	    
	         zhuse_prime = zh_prime - &
	        & (zyt_prime(1) + zrtan_prime - zrad_prime)/(zdydh(1) - zdr_dtheta*zdydh(2))  &
	        & + (zyt(1)+zrtan-zrad)/(zdydh(1) - zdr_dtheta*zdydh(2))**2* &
	        & (zdydh_prime(1) - zdr_dtheta_prime*zdydh(2) - zdr_dtheta*zdydh_prime(2))  	    

! limit the step

               
	       IF (zhuse > zhmax) THEN
	       
	           zhuse = zhmax
	    
	           zhuse_prime = 0.0_wp
	    
	       ELSE IF (zhuse < zhmin) THEN
	       
	           zhuse = zhmin
		   
		   zhuse_prime = 0.0_wp
	    
	       ENDIF

		
		
	      ELSE
		
	         zhuse = zh
	         zhuse_prime = zh_prime			     
	             
	      ENDIF 
!
! update the position vector
!

	    	 
	      zy(:) = zy(:) + zdydh(:)*zhuse 
	      zy_prime(:) = zy_prime(:) + zdydh(:)*zhuse_prime + zdydh_prime(:)*zhuse


              zalpha_half(iside) = zy(4)
	      zalpha_half_prime(iside) = zy_prime(4)
	 
	      IF (LEAVING) EXIT
	                     
! try to maintain roughly the same radial increment by adjusting h
	
	      IF (j < ksplit) THEN
	    
	
	         zdr = (zrad-zy(1)-zrtan)/REAL(MAX(ksplit-j,1))
	         zdr_prime = (zrad_prime-zy_prime(1)-zrtan_prime)/ &
	       &        REAL(ksplit-j) 
		
	         zhnew = MIN(zh,zdr/MAX(1.0E-10_wp,COS(zy(3))))
	       
	    
	         IF (zhnew < zh) THEN
	    
	            zhnew_prime = zhnew/zdr*zdr_prime + zhnew*TAN(zy(3))*zy_prime(3)
	       
	         ELSE
	    
	            zhnew_prime = zh_prime    

	         ENDIF  
		
  	         zh = zhnew
	         zh_prime = zhnew_prime


! set minimum step-length
	    
	         IF (zh > zhmax) THEN
	       
	            zh = zhmax
	    
	            zh_prime = 0.0_wp
	    
	         ELSE IF (zh < zhmin) THEN
	       
	            zh = zhmin
		    
		    zh_prime = 0.0_wp
	    
	         ENDIF

	    
	         zh2 = 0.5_wp*zh
                 zh2_prime = 0.5_wp*zh_prime

              ENDIF 


	      ENDDO  ! complete path thru ith layer

          
      ELSE

!
! DO 1D calculation


         IF (lfirst_1d) THEN
	 
	 
            ik = NINT((zy(2) + ztheta_tan)/pdsep)+1
	    ik = MIN(MAX(1,ik),khoriz-1)
	    
	    pa_path(in,iside) = (1.0_wp+1.0E-6_wp*prefrac(i,ik))*((zy(1)+zrtan)*SIN(zy(3)))

            pa_path_prime(in,iside) = pa_path(in,iside)* &
	  &  (1.0E-6_wp/(1.0_wp+1.0E-6_wp*prefrac(i,ik))*prefrac_prime(i,ik) + &
	  &  (zy_prime(1)+zrtan_prime)/(zy(1)+zrtan) + &
	  &  COS(zy(3))/SIN(zy(3))*zy_prime(3) ) 
	    	    
 	    zaval = pa_path(in,iside)
            zaval_prime = pa_path_prime(in,iside)	    
	   	    
	    	    
	    zalpha_half(iside) = zy(4)
	    zalpha_half_prime(iside) = zy_prime(4)
	    
	    
	    
	    lfirst_1d = .FALSE.
	    
 	 ENDIF     

! Continue with 1D bending angle calculation

         
         IF ( i == ibot) THEN 
      
! we are doing a 1d calc for entire ray path
            
	    lone_d_calc = .TRUE. 	    
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


         ELSE 
      
            zref_low = prefrac(i,ik)
	    zref_low_prime = prefrac_prime(i,ik)
	     
	    znr_low  = pnr(i,ik) 
	    znr_low_prime = pnr_prime(i,ik)
	 	 
         ENDIF


	 
	 IF (i > ibot) THEN
	 
	    ztlow = SQRT(MAX(zkval(i,ik)*(pnr(i,ik) - zaval),1.0E-10_wp))
	    
	    IF (zkval(i,ik)*(pnr(i,ik) - zaval) > 1.0E-10_wp) THEN
	    
	       ztlow_prime = 0.5_wp*(zkval_prime(i,ik)*(pnr(i,ik)-zaval) + &
	     &  zkval(i,ik)*(pnr_prime(i,ik)-zaval_prime))/ztlow
	       
            ELSE
	    
	       ztlow_prime = 0.0_wp		
		
            ENDIF  		

         ENDIF 

	 ztup = SQRT(MAX(zkval(i,ik)*(pnr(i+1,ik)-zaval),1.0E-10_wp))
 
         IF (zkval(i,ik)*(pnr(i+1,ik)-zaval) > 1.0E-10_wp) THEN 
 
            ztup_prime = 0.5_wp*(zkval_prime(i,ik)*(pnr(i+1,ik)-zaval) + &
	 &   zkval(i,ik)*(pnr_prime(i+1,ik)-zaval_prime))/ztup

         ELSE
	    
	    ztup_prime = 0.0_wp
	    
	 ENDIF   

!
! calculate the error functions within this routine rather than an external function call.
!


         IF (i == ibot) THEN
	 	 		 
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
	    
	 ELSE IF (i > ibot .AND. i < klev-1) THEN
	 	   
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
	    	    
        ELSE
	 
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
	 	 
	    	 	    
	 ENDIF     
          
	 
	  
	 zdiff_erf = zerf_up - zerf_low 
	 zdiff_erf_prime = zerf_up_prime - zerf_low_prime
	 


! bending angle	  
	 
	 
         zdalpha    =  &
        & + 1.0E-6_wp * zRoot_halfPI* SQRT(zaval*zkval(i,ik)) & 
        & * zref_low*EXP(zkval(i,ik)*(znr_low-zaval))*zdiff_erf 
 
 
         zdalpha_prime = zdalpha*(  &
	              & zref_low_prime/MAX(1.0E-10_wp,zref_low)  + &
		      &	zdiff_erf_prime/MAX(1.0E-10_wp,zdiff_erf) + &
		      &	(0.5_wp/zaval - zkval(i,ik))*zaval_prime + &
		      &	(znr_low -zaval + 0.5_wp/zkval(i,ik))*zkval_prime(i,ik) + &
		      &	zkval(i,ik)*znr_low_prime ) 



        zalpha_half(iside) = zalpha_half(iside) + zdalpha 
        zalpha_half_prime(iside) = zalpha_half_prime(iside) + zdalpha_prime  


      ENDIF 
	 
	 
      ENDDO  ! i the layers

!
! if we performed a 1d calculation don't evaluate iside = 2
!
     
      IF (lone_d_calc) THEN
      
         zalpha_half(2) = zalpha_half(1)

         zalpha_half_prime(2) = zalpha_half_prime(1)
	  
	 EXIT  ! exiting the iside loop
     
     ENDIF      


   ENDDO ! iside

!
! the total bending angle adding both sides
!

   palpha(in) = zalpha_half(1) + zalpha_half(2) 
   palpha_prime(in) = zalpha_half_prime(1) + zalpha_half_prime(2)

ENDDO OBLOOP

RETURN

END SUBROUTINE ropp_fm_alpha2drk_tl
