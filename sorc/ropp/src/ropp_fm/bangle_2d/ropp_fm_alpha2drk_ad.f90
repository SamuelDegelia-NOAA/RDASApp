! $Id$

!****s* BendingAngle2d/ropp_fm_alpha2drk_ad *
!
! NAME
!    ropp_fm_alpha2drk_ad - Forward model calculating a bending
!                        angle profile from planar information.
!
! SYNOPSIS
!    call ropp_fm_alpha2drk_ad(kobs, klev, ...)
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
!           ksplit = splitting of model levels
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



SUBROUTINE ropp_fm_alpha2drk_ad(kobs,   & ! no.of observations
                       klev,   & ! no. of vertical levels
		       khoriz, & ! no. of horizontal layers  ODD
		       ksplit, &
		       pdsep,   & ! the angular spacing 
                       pa,      & ! impact parameter values
   		       prefrac, & ! refractivity
		       prefrac_hat, &
		       pradius, & ! radius values
		       pradius_hat, &
		       pnr,     &
		       pnr_hat, &
		       proc, &
		       pz_2d, &
		       pa_path_hat, &
		       palpha_hat)       ! partial path length along rays


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
REAL(KIND=wp),    INTENT(INOUT)  :: prefrac_hat(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(IN)  :: pradius(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(INOUT)  :: pradius_hat(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(IN)  :: pnr(klev,khoriz)
REAL(KIND=wp),    INTENT(INOUT)  :: pnr_hat(klev,khoriz)
REAL(KIND=wp),    INTENT(IN)  :: proc                   ! radius of curvature
REAL(KIND=wp),    INTENT(IN)  :: pz_2d
REAL(KIND=wp),    INTENT(INOUT) :: pa_path_hat(kobs,2)        
REAL(KIND=wp),    INTENT(INOUT) :: palpha_hat(kobs)   ! path length
		       
!
! local variables
!

INTEGER :: i,j,jj,in,ibot,jbot,ikbot,iside,ik,ikp1,ibot_old
INTEGER :: ikcen
REAL(KIND=wp), PARAMETER :: zhmax = 3.0E4_wp
REAL(KIND=wp), PARAMETER :: zhmin = 1.0E2_wp
REAL(KIND=wp) :: zrad(klev,ksplit),zdndr
REAL(KIND=wp) :: zrad_hat,zdndr_hat
REAL(KIND=wp) :: zhwt1,zhwt2
REAL(KIND=wp) :: zhwt1_hat,zhwt2_hat
REAL(KIND=wp) :: zamult
REAL(KIND=wp) :: zh(klev,ksplit),zh2,zhuse(klev,ksplit),zhnew,zh_up(klev)
REAL(KIND=wp) :: zh_hat,zh2_hat,zhuse_hat,zhnew_hat,zh_up_hat
REAL(KIND=wp) :: zy(4,klev,ksplit),zyt(4,5)
REAL(KIND=wp) :: zy_hat(4),zyt_hat(4)
REAL(KIND=wp) :: zdydh(4,klev,ksplit),zdydht(4,4)
REAL(KIND=wp) :: zdydh_hat(4),zdydht_hat(4,4)
REAL(KIND=wp) :: ztheta_tan,ztheta_min,ztheta_max
REAL(KIND=wp) :: zdr_max,zdr_dtheta(klev),zrtan,zdr(klev,ksplit)
REAL(KIND=wp) :: zdr_max_hat,zdr_dtheta_hat,zrtan_hat,zdr_hat
REAL(KIND=wp) :: zalpha_half(2)
REAL(KIND=wp) :: zalpha_half_hat(2)
REAL(KIND=wp) :: zkval(klev-1,khoriz)
REAL(KIND=wp) :: zkval_hat(klev-1,khoriz)
REAL(KIND=wp) :: ztlow(klev),ztup(klev),zdalpha(klev),zRoot_halfPI
REAL(KIND=wp) :: ztlow_hat,ztup_hat,zdalpha_hat
REAL(KIND=wp) :: zerf_up(klev),zerf_low(klev),ztl(klev),ztu(klev),zdiff_erf(klev),&
      &  znr_low(klev),zref_low(klev),zaval
REAL(KIND=wp) :: zerf_up_hat,zerf_low_hat,zt_hat,zdiff_erf_hat,znr_low_hat,zref_low_hat,zaval_hat


REAL(KIND=wp) :: zalpha(kobs),za_path(kobs,2)
INTEGER :: in_2d
INTEGER :: istep(klev)
LOGICAL :: lfirst_1d,LEAVING,lone_d_calc


!initialise local adjoint variables
!

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
zrad_hat = 0.0_wp
zhwt1_hat = 0.0_wp
zhwt2_hat = 0.0_wp
zdydht_hat(:,:) = 0.0_wp
zdydh_hat(:) = 0.0_wp
zdr_max_hat = 0.0_wp
zdr_dtheta_hat = 0.0_wp
zrad_hat = 0.0_wp
zdndr_hat = 0.0_wp
zhwt1_hat = 0.0_wp
zhwt2_hat = 0.0_wp
zdr_hat = 0.0_wp
zh_up_hat = 0.0_wp

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
      zkval(i,j) = MAX(1.0E-6_wp,zkval(i,j))
       
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

zalpha(:)=ropp_MDFV
za_path(:,:)= ropp_MDFV


zRoot_halfPI = SQRT(0.5_wp*pi)

OBLOOP: DO in=1,kobs
              
   IF (pa(in) < pnr(jbot,ikcen) .OR. pa(in) >= pnr(klev,ikcen)) CYCLE  
   
! adjoint code
    
   zalpha_half_hat(1) = zalpha_half_hat(1) + palpha_hat(in)
   zalpha_half_hat(2) = zalpha_half_hat(2) + palpha_hat(in)
   palpha_hat(in) = 0.0_wp
      
   ibot = jbot

   DO 

      IF (pnr(ibot+1,ikcen) - pa(in) > 1.0_wp) EXIT   ! assuming "a" is on one of the pressure levels

      ibot=ibot+1

   ENDDO

!
! calculate the radius at tangent point   
!   
   zrad(1,1) = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))   
   
   zdndr = 1.0E-6_wp*(prefrac(ibot+1,ikcen)-prefrac(ibot,ikcen))/ &
                 (pradius(ibot+1,ikcen)-pradius(ibot,ikcen)) 
  
		   
   IF ( zrad(1,1)*zdndr > -1.0_wp) THEN
  
       zrtan = pradius(ibot,ikcen) + &
             (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad(1,1)*zdndr)

   ELSE
   
       zrtan = zrad(1,1)   ! probably in a super-refracting layer
              
   ENDIF      	  

! if zrtan is within a 1 m of upper level set to upper level
   
   ibot_old = ibot
   
   IF ((zrtan - pradius(ibot+1,ikcen)) > -1.0_wp) THEN
   
       ibot = ibot + 1
          
       zrtan = pradius(ibot,ikcen)
       
   ENDIF     


!
!  set bending angle value  
!   

   zalpha_half(:) = 0.0_wp   


   DO iside = 1,2 
   
      istep(:) = 0
   
      za_path(in,iside) = pa(in)  ! adjoint = ??
      lfirst_1d = .TRUE.
      lone_d_calc = .FALSE.  
      zamult = 1.0_wp
      IF (iside == 2) zamult  = -1.0_wp
      
!
! initialise vector
!

      zy(1,ibot,1) = 0.0_wp           ! height above tangent point           
      zy(2,ibot,1) = 0.0_wp           ! theta
      zy(3,ibot,1) = ASIN(1.0_wp)     ! thi
      zy(4,ibot,1) = 0.0_wp           ! bending angle 
      
      
      DO i = ibot,klev-1
	
	 
	 IF ( i < MIN(in_2d,klev-1) .AND. khoriz > 1) THEN
	
            IF (i == ibot) THEN
	 
	       ik = ikcen
               zdr_max = (pradius(i+1,ik)- zrtan)/REAL(ksplit)	    	    
               zh(i,1) = SQRT(2.0_wp*pradius(ibot,ik)*zdr_max)		    
	    
            ELSE

               ik = INT((zy(2,i,1) + ztheta_tan)/pdsep)+1
	       ik = MIN(MAX(1,ik),khoriz)
     	       zdr_max = (pradius(i+1,ik)-pradius(i,ik))/REAL(ksplit)
	    
	       zh_up(i) = SQRT(2.0_wp*pradius(i,ik)*zdr_max)	    
	       zh(i,1) = zdr_max/MAX(COS(zy(3,i,1)),1.0E-10_wp)

! physical limit on step size
	       
	       zh(i,1) = MIN(zh_up(i),zh(i,1))	       

            ENDIF
	     

! limit the step-length
	 
	    IF (zh(i,1) > zhmax) THEN
	 
	      zh(i,1) = zhmax
	    
	    ELSE IF (zh(i,1) < zhmin) THEN
	 
	      zh(i,1) = zhmin    
	    	 
	    ENDIF 


	    zh2 = 0.5_wp*zh(i,1)	 
	 
!
! now calculate the path-length with a RUNGE-KUTTA
!
	 
	    LEAVING = .FALSE. 
	   
	    DO j = 1,ksplit	  
	   
	       istep(i) = istep(i) + 1
	   	    	    
	       zyt(:,1) = zy(:,i,j)	    
	    
! first calculation of derivs

	       CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	       zrtan,zamult,prefrac,pradius,zyt(:,1),zdydht(:,1)) 
	
	       zyt(:,2) = zy(:,i,j) + zdydht(:,1)*zh2	    
	    
	  	    
! second call at new yt

	       CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	       zrtan,zamult,prefrac,pradius,zyt(:,2),zdydht(:,2)) 

	    
               zyt(:,3) = zy(:,i,j) + zdydht(:,2)*zh2
	    	    	    
! third call at new yt
	    
	    
	      CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	      zrtan,zamult,prefrac,pradius,zyt(:,3),zdydht(:,3)) 

               zyt(:,4) = zy(:,i,j) + zdydht(:,3)*zh(i,j)
	    	    
! fourth last call

	       CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	       zrtan,zamult,prefrac,pradius,zyt(:,4),zdydht(:,4)) 

               zdydh(:,i,j) = (zdydht(:,1)+zdydht(:,4)+2.0_wp*(zdydht(:,2)+zdydht(:,3)))/6.0_wp
	   
	       zyt(:,5) = zy(:,i,j) + zdydh(:,i,j)*zh(i,j)	    
	    
!
! check the radius - have we exited the level
!	    
	    
	       ik = INT((zyt(2,5) + ztheta_tan)/pdsep)+1
	       ik = MIN(MAX(1,ik),khoriz-1)
	       ikp1 = ik+1
	 
! horizontal weighting factor       
	    
	       IF ( zyt(2,5) < ztheta_max .AND. zyt(2,5) > ztheta_min) THEN
	    	       
                  zhwt1 = (REAL(ik)*pdsep - (zyt(2,5)+ztheta_tan))/pdsep    
	          zhwt2 = 1.0_wp - zhwt1
	       
	    
	       ELSE IF (zyt(2,5) < ztheta_min) THEN	    
	    
	          zhwt1 = 1.0_wp
	          zhwt2 = 0.0_wp
	       
	       ELSE IF (zyt(2,5) > ztheta_max) THEN
	    
	          zhwt1 = 0.0_wp
	          zhwt2 = 1.0_wp
	       
	       ENDIF    

	 
! horizontal weighting factor	       
! radius of pressure level	     

	       zrad(i,j) = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)
	    
	    
! if gone over the boundary scale h
	    
	       IF ( j == ksplit .OR.(zyt(1,5)+zrtan-zrad(i,j)) > 0.0_wp  ) THEN
	    
	          LEAVING = .TRUE. 
	    
	          zdr_dtheta(i) = 0.0_wp
	    
	          IF (zyt(2,5) < ztheta_max .AND. zyt(2,5) > ztheta_min) THEN
	        
		     zdr_dtheta(i) = (pradius(i+1,ikp1)-pradius(i+1,ik))/pdsep
	       
	         ENDIF        	
			    
	         zhuse(i,j) = &
	         zh(i,j) - (zyt(1,5)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))

	        
	         IF (zhuse(i,j) > zhmax) THEN
	       
	            zhuse(i,j) = zhmax
	    
	         ELSE IF (zhuse(i,j) < zhmin) THEN
	       
	           zhuse(i,j) = zhmin
		   
	         ENDIF

	    	 
	       ELSE
		
	         zhuse(i,j) = zh(i,j)
	             
	       ENDIF 
	 

!!!!               huse(i,j) = 20000.0 

!
! update the position vector
!	    	 
	      IF ( j == ksplit .OR. LEAVING) THEN
	    	    
	         zy(:,i+1,1) = zy(:,i,j) + zdydh(:,i,j)*zhuse(i,j)
	    
	      ELSE
	    
	         zy(:,i,j+1) = zy(:,i,j) + zdydh(:,i,j)*zhuse(i,j) 	       

	      ENDIF   
	    
	    
	      IF (LEAVING) EXIT 
	    
! try to maintain roughly the same radial increment by adjusting h
	    
		    
	      IF (j < ksplit) THEN

                 zdr(i,j) = (zrad(i,j)-zy(1,i,j+1)-zrtan)/REAL(ksplit-j)
	         zhnew = MIN(zh(i,j),zdr(i,j)/MAX(1.0E-10_wp,COS(zy(3,i,j+1))))	    
	         zh(i,j+1) = MAX(MIN(zhmax,zhnew),zhmin)

	         zh2 = 0.5_wp*zh(i,j+1)
	       
	      ENDIF   

	    ENDDO  ! complete path thru ith layer


      ELSE

!
! DO 1D calculation


         IF (lfirst_1d) THEN
	 
	 
            ik = NINT((zy(2,i,1) + ztheta_tan)/pdsep)+1
	    ik = MIN(MAX(1,ik),khoriz-1)
	    
	    za_path(in,iside) = &
           &(1.0_wp+1.0E-6_wp*prefrac(i,ik))*((zy(1,i,1)+zrtan)*SIN(zy(3,i,1)))
	    	    
 	    zaval = za_path(in,iside)
	    zalpha_half(iside) = zy(4,i,1)
	    
	    lfirst_1d = .FALSE.
	    
 	 ENDIF     

         ! Continue with 1D bending angle calculation

         IF ( i == ibot) THEN 
 
 
 ! we are doing a 1d calc for entire ray path
            
	    lone_d_calc = .TRUE. 	    
      
            zref_low(i) = prefrac(ibot,ik)*EXP(-zkval(ibot,ik)*(pa(in)-pnr(ibot,ik)))
	    
	    zaval = pa(in) 
	    znr_low(i) = pa(in)
	
        ELSE 
      
            zref_low(i) = prefrac(i,ik) 
	    znr_low(i)  = pnr(i,ik) 
	 
	 ENDIF

         ztlow(i) = 0.0_wp
	 IF (i > ibot) ztlow(i) = SQRT(MAX(zkval(i,ik)*(pnr(i,ik) - zaval),1.0E-10_wp))

	 ztup(i) = SQRT(MAX(zkval(i,ik)*(pnr(i+1,ik)-zaval),1.0E-10_wp))



!
! calculate the error functions within this routine rather than an external function call.
!

         IF (i == ibot) THEN
	 	 		 
	    zerf_low(i) = 0.0_wp	 
		 
	    ztu(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztup(i))
	    
	    zerf_up(i)= &
	 &   1.0_wp-(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztu(i))*ztu(i))*ztu(i)*EXP(-(ztup(i)*ztup(i)))	    
	 
	 ELSE IF (i > ibot .AND. i < klev-1) THEN
	 	   
! lower
	    ztl(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztlow(i)) 
	    
	    zerf_low(i) = &
	  & -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztl(i))*ztl(i))*ztl(i)*EXP(-(ztlow(i)*ztlow(i))) 

! upper

	    ztu(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztup(i))
	    
	    zerf_up(i)= &
	    -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztu(i))*ztu(i))*ztu(i)*EXP(-(ztup(i)*ztup(i)))	    
	    	    
         ELSE
	 
	    zerf_up(i) = 0.0_wp 
	 
	    ztl(i) = 1.0_wp/(1.0_wp+0.47047_wp*ztlow(i)) 
	    
	    zerf_low(i) = &
	 & -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*ztl(i))*ztl(i))*ztl(i)*EXP(-(ztlow(i)*ztlow(i))) 
	    
	 ENDIF     
          
	  
	 zdiff_erf(i) = zerf_up(i) - zerf_low(i) 
	 

! bending angle	  
	 
         zdalpha(i)    =  &
        & + 1.0E-6_wp * zRoot_halfPI* SQRT(zaval*zkval(i,ik)) & 
        & * zref_low(i)*EXP(zkval(i,ik)*(znr_low(i)-zaval))*zdiff_erf(i) 
 
         zalpha_half(iside) = zalpha_half(iside) + zdalpha(i) 


      ENDIF 

	 
	 
      ENDDO  ! i the layers


!
! now start the adjoint
!    
      IF (lone_d_calc) THEN
      
         zalpha_half_hat(1) = zalpha_half_hat(1) + zalpha_half_hat(2)
	 zalpha_half_hat(2) = 0.0_wp
 
      ENDIF 
 
 
! loop through levels
      
      DO i = klev-1,ibot,-1      
         
         
      
         IF (i >= MIN(in_2d,klev-1)) THEN

! 1d calculation	 
	 	 
	    zdalpha_hat = zdalpha_hat + zalpha_half_hat(iside)
	    zalpha_half_hat(iside) = zalpha_half_hat(iside)  
	    	
	    zref_low_hat = zref_low_hat + &
	 &   zdalpha(i)/MAX(1.0E-10_wp,zref_low(i))*zdalpha_hat
	 
	    zdiff_erf_hat = zdiff_erf_hat + &
	 &  zdalpha(i)/MAX(1.0E-10_wp,zdiff_erf(i))*zdalpha_hat
	 
	    zaval_hat = zaval_hat + &
	 &  zdalpha(i)*(0.5_wp/zaval - zkval(i,ik))*zdalpha_hat
	 
	    zkval_hat(i,ik) = zkval_hat(i,ik) + &
	 &  zdalpha(i)*(znr_low(i) -zaval + 0.5_wp/zkval(i,ik))*zdalpha_hat 
            
	    znr_low_hat = znr_low_hat + &
	 &  zdalpha(i)*zkval(i,ik)*zdalpha_hat
	    
	    zdalpha_hat = 0.0

! diff erf
      
            zerf_up_hat = zerf_up_hat + zdiff_erf_hat
	    zerf_low_hat = zerf_low_hat - zdiff_erf_hat
	    zdiff_erf_hat = 0.0_wp
      
         IF (i == ibot) THEN
	 	 		 	    
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
	    
	    
	    
	 ELSE IF (i > ibot .AND. i < klev-1) THEN
	 	   
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
	        
	   
	    	    
        ELSE
	 
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
	 
	 
	    zerf_up_hat = 0.0 
	 	 
	    	 	    
	 ENDIF     

! tup
!!!!         tup_hat = 0.0

      
         IF (zkval(i,ik)*(pnr(i+1,ik)-zaval) > 1.0E-10_wp) THEN 
 
            zkval_hat(i,ik) = zkval_hat(i,ik) + &
	 &   0.5_wp*(pnr(i+1,ik)-zaval)/ztup(i)*ztup_hat
	    
            pnr_hat(i+1,ik) = pnr_hat(i+1,ik) + 0.5_wp*zkval(i,ik)/ztup(i)*ztup_hat
	    zaval_hat = zaval_hat - 0.5_wp*zkval(i,ik)/ztup(i)*ztup_hat
	    
	    ztup_hat = 0.0_wp	    

         ELSE
	    
	    ztup_hat = 0.0_wp
	    
	 ENDIF   

! tlow

!!!         tlow_hat = 0.0 

	 IF (i > ibot) THEN
	 
	    
	    IF (zkval(i,ik)*(pnr(i,ik) - zaval) > 1.0E-10) THEN
	    
	       zkval_hat(i,ik) = zkval_hat(i,ik) + &
	    &  0.5_wp*(pnr(i,ik)-zaval)/ztlow(i)*ztlow_hat
	       
	       pnr_hat(i,ik) = pnr_hat(i,ik) + &
	    &  0.5_wp*zkval(i,ik)/ztlow(i)*ztlow_hat
	       
	       zaval_hat = zaval_hat - 0.5_wp*zkval(i,ik)/ztlow(i)*ztlow_hat
	       
	       ztlow_hat = 0.0_wp
	       
	       
            ELSE
	    
	       ztlow_hat = 0.0_wp		
		
            ENDIF  		


         ENDIF 
         
	 
	 ztlow_hat = 0.0_wp
	 
         IF ( i == ibot) THEN 
      
            znr_low_hat = 0.0_wp
	    zaval_hat = 0.0_wp
	    
	    pa_path_hat(in,iside) = 0.0_wp
	    
	    prefrac_hat(i,ik) = prefrac_hat(i,ik) + &
	 &  zref_low(i)/prefrac(i,ik)*zref_low_hat
	    
	    zkval_hat(i,ik) = zkval_hat(i,ik) - &
	 &  zref_low(i)*(pa(in)-pnr(i,ik))*zref_low_hat
	    
	    pnr_hat(i,ik)=pnr_hat(i,ik) + zref_low(i)*zkval(i,ik)*zref_low_hat
	    zref_low_hat = 0.0_wp
	    

         ELSE 
      
	    prefrac_hat(i,ik) = prefrac_hat(i,ik) + zref_low_hat
	    zref_low_hat = 0.0_wp
	    	     
	    pnr_hat(i,ik) = pnr_hat(i,ik) + znr_low_hat
	    znr_low_hat = 0.0 
	
	 	 
         ENDIF
	 
         IF (i == MIN(in_2d,klev-1)) THEN
!
! impact parameter variation
!	 

            zy_hat(4) = zy_hat(4) + zalpha_half_hat(iside) 
            zalpha_half_hat(iside) = 0.0_wp


	    pa_path_hat(in,iside) = pa_path_hat(in,iside) + zaval_hat
	    zaval_hat = 0.0_wp  
	    
            prefrac_hat(i,ik)=prefrac_hat(i,ik)+&
	 &  1.0E-6_wp*za_path(in,iside)/(1.0_wp+1.0E-6_wp*prefrac(i,ik))*pa_path_hat(in,iside)
	    
	    zy_hat(1) = zy_hat(1) + za_path(in,iside)/(zy(1,i,1)+zrtan)*pa_path_hat(in,iside)
	    zrtan_hat = zrtan_hat + za_path(in,iside)/(zy(1,i,1)+zrtan)*pa_path_hat(in,iside)
	    
	    zy_hat(3) = zy_hat(3) + &
	    za_path(in,iside)*COS(zy(3,i,1))/SIN(zy(3,i,1))*pa_path_hat(in,iside)
	    
	    pa_path_hat(in,iside) = 0.0_wp
	    
 
         ENDIF 
      
      
         ELSE 

! 2d bit	 
	       
           DO j = istep(i),1,-1
                    
		    
              IF (j < istep(i)) THEN
      
                 zh_hat = zh_hat + 0.5_wp*zh2_hat
	         zh2_hat = 0.0_wp

! limiting the size of the step
	       
 	         zhnew = zh(i,j+1) ! just for clarity
	       
	         IF (ABS(zhnew-zhmax)  < 1.0E-3_wp*SPACING(zhnew)) zh_hat = 0.0_wp  
	         IF (ABS(zhnew-zhmin)  < 1.0E-3_wp*SPACING(zhnew)) zh_hat = 0.0_wp  
	 
	         zhnew_hat = zhnew_hat + zh_hat
	         zh_hat = 0.0_wp
            
                 zhnew = zh(i,j+1) ! just for clarity
	       
               IF (zhnew < zh(i,j)) THEN
	    
	          
	          zdr_hat = zdr_hat + zhnew/zdr(i,j)*zhnew_hat
	          zy_hat(3) = zy_hat(3) + zhnew*TAN(zy(3,i,j+1))*zhnew_hat
	          zhnew_hat = 0.0_wp
	       
	       ELSE
	    
	          zh_hat = zh_hat + zhnew_hat
	          zhnew_hat = 0.0_wp   
	            
               ENDIF 
            
               zrad_hat = zrad_hat + zdr_hat/REAL(ksplit-j)
	       zy_hat(1) = zy_hat(1) - zdr_hat/REAL(ksplit-j)
	       zrtan_hat = zrtan_hat - zdr_hat/REAL(ksplit-j)
	       zdr_hat = 0.0_wp
           
            ENDIF 
      
! updating the vector. 
          
	   zy_hat(4) = zy_hat(4) + zalpha_half_hat(iside)
	   zalpha_half_hat(iside) = 0.0_wp    
	   	   
!!	    y_prime(:) = y_prime(:) + dydh(:)*huse_prime + dydh_prime(:)*huse

            zdydh_hat(:) = zdydh_hat(:) + zy_hat(:)*zhuse(i,j)	    
	    DO jj = 1,4
               zhuse_hat = zhuse_hat + zdydh(jj,i,j)*zy_hat(jj)
	    ENDDO   
            zy_hat(:) = zy_hat(:)


!!!            huse_hat = 0.0

 
!
! need to recalculate the yt(:,:) and dydht(:,:) at intermediate points
!

            zh2 = 0.5_wp*zh(i,j)
	    zyt(:,1) = zy(:,i,j)	    
	    
! first calculation of derivs

	    CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	    zrtan,zamult,prefrac,pradius,zyt(:,1),zdydht(:,1)) 
	
	    zyt(:,2) = zy(:,i,j) + zdydht(:,1)*zh2	    
	    

	  	    
! second call at new yt

	    CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	    zrtan,zamult,prefrac,pradius,zyt(:,2),zdydht(:,2)) 

	    
            zyt(:,3) = zy(:,i,j) + zdydht(:,2)*zh2


	    	    	    
! third call at new yt
	    
	    
	    CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	    zrtan,zamult,prefrac,pradius,zyt(:,3),zdydht(:,3)) 

            zyt(:,4) = zy(:,i,j) + zdydht(:,3)*zh(i,j)


	    	    
! fourth last call

	    CALL ropp_fm_gpspderivs(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	    zrtan,zamult,prefrac,pradius,zyt(:,4),zdydht(:,4)) 

	    zyt(:,5) = zy(:,i,j) + zdydh(:,i,j)*zh(i,j)	    

	    ik = INT((zyt(2,5) + ztheta_tan)/pdsep)+1
	    ik = MIN(MAX(1,ik),khoriz-1)
	    ikp1 = ik+1
	 
! horizontal weighting factor       
	    
	    IF ( zyt(2,5) < ztheta_max .AND. zyt(2,5) > ztheta_min) THEN
	    	       
               zhwt1 = (REAL(ik)*pdsep - (zyt(2,5)+ztheta_tan))/pdsep    
	       zhwt2 = 1.0_wp - zhwt1
	       
	    
	    ELSE IF (zyt(2,5) < ztheta_min) THEN	    
	    
	       zhwt1 = 1.0_wp
	       zhwt2 = 0.0_wp
	       
	    ELSE IF (zyt(2,5) > ztheta_max) THEN
	    
	       zhwt1 = 0.0_wp
	       zhwt2 = 1.0_wp
	       
	    ENDIF    


	    IF ( j == istep(i)) THEN
	    
	       IF (ABS(zhuse(i,j)-zhmax)  < 1.0E-3_wp*SPACING(zhmin)) zhuse_hat = 0.0_wp  
	       IF (ABS(zhuse(i,j)-zhmin)  < 1.0E-3_wp*SPACING(zhmin)) zhuse_hat = 0.0_wp  
			
	       zh_hat = zh_hat + zhuse_hat
	       	       
	       zyt_hat(1) = zyt_hat(1) - zhuse_hat/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))
	       
	       zrtan_hat = zrtan_hat - zhuse_hat/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))
	       
	       zrad_hat = zrad_hat + zhuse_hat/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))
	       
	       zdydh_hat(1) = zdydh_hat(1) + zhuse_hat* &
	   &   (zyt(1,5)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))**2
	       
	       zdr_dtheta_hat = zdr_dtheta_hat - zhuse_hat*zdydh(2,i,j)* &
	   &   (zyt(1,5)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))**2
	       
	       zdydh_hat(2) = zdydh_hat(2) - zhuse_hat*zdr_dtheta(i)* &
	   &   (zyt(1,5)+zrtan-zrad(i,j))/(zdydh(1,i,j) - zdr_dtheta(i)*zdydh(2,i,j))**2	
	       zhuse_hat = 0.0_wp
		
	       
	       IF (zyt(2,5) < ztheta_max .AND. zyt(2,5) > ztheta_min) THEN
	        	       
!	          dr_dtheta_prime = &
!	          (radius_prime(i+1,kp1)-radius_prime(i+1,k))/pdsep
	
	          pradius_hat(i+1,ikp1) = pradius_hat(i+1,ikp1) + zdr_dtheta_hat/pdsep
		  pradius_hat(i+1,ik) = pradius_hat(i+1,ik) - zdr_dtheta_hat/pdsep
		  zdr_dtheta_hat = 0.0_wp
		  
	       ENDIF
	       
	       
	       zdr_dtheta_hat = 0.0_wp	       
	       	
		
	    ELSE
	       
	       zh_hat = zh_hat + zhuse_hat
	       zhuse_hat = 0.0_wp
	      	             
	    ENDIF 
	    

!
! radius at current position
!
      
!	    rad_prime = hwt1*radius_prime(i+1,k)+hwt2*radius_prime(i+1,kp1) + &
!	    hwt1_prime*radius(i+1,k)+hwt2_prime*radius(i+1,kp1)
            
            pradius_hat(i+1,ik) = pradius_hat(i+1,ik) + zhwt1*zrad_hat
	    pradius_hat(i+1,ikp1) = pradius_hat(i+1,ikp1) + zhwt2*zrad_hat
	    zhwt1_hat = zhwt1_hat + pradius(i+1,ik)*zrad_hat
	    zhwt2_hat = zhwt2_hat + pradius(i+1,ikp1)*zrad_hat
	    zrad_hat = 0.0_wp

            
 	    IF ( zyt(2,5) < ztheta_max .AND. zyt(2,5) > ztheta_min) THEN
	    	       
!	       hwt1_prime = -yt_prime(2)/pdsep
!	       hwt2_prime = - hwt1_prime 
	       
	       zhwt1_hat = zhwt1_hat - zhwt2_hat
	       zhwt2_hat = 0.0_wp
	       
	       zyt_hat(2) = zyt_hat(2) - zhwt1_hat/pdsep
	       zhwt1_hat = 0.0_wp
	       
	    
	    ELSE IF (zyt(2,5) < ztheta_min) THEN	    
	    	       
	       zhwt1_hat = 0.0_wp
	       zhwt2_hat = 0.0_wp
	       
	    ELSE IF (zyt(2,5) > ztheta_max) THEN
	    
	       zhwt1_hat = 0.0_wp
	       zhwt2_hat = 0.0_wp
	       
	    ENDIF    

!	    yt_prime(:) = y_prime(:) + dydh(:)*h_prime + dydh_prime(:)*h
 
!            yt_hat = 0.0
 
 
            zy_hat(:) = zy_hat(:) +  zyt_hat(:)
	    zdydh_hat(:) = zdydh_hat(:) + zyt_hat(:)*zh(i,j)
	    DO jj = 1,4
	       zh_hat = zh_hat + zdydh(jj,i,j)*zyt_hat(jj)
	    ENDDO
	    zyt_hat(:) = 0.0_wp   
           
! gradient used on this step

!	    dydh_prime(:) = &
!	    (dydht_prime(:,1)+dydht_prime(:,4)+2.0*(dydht_prime(:,2)+dydht_prime(:,3)))/6.0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!            
!!	    dydht_hat(:,1) = dydht_hat(:,1) + dydh_hat(:)
!!	    dydh_hat(:)=0.0
	    
            
	    zdydht_hat(:,1) = zdydht_hat(:,1) + zdydh_hat(:)/6.0_wp
	    zdydht_hat(:,4) = zdydht_hat(:,4) + zdydh_hat(:)/6.0_wp
	    zdydht_hat(:,2) = zdydht_hat(:,2) + zdydh_hat(:)/3.0_wp
	    zdydht_hat(:,3) = zdydht_hat(:,3) + zdydh_hat(:)/3.0_wp
	    zdydh_hat(:) = 0.0_wp
	
! 4th call
	    
	    CALL ropp_fm_gpspderivs_ad(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	 &  zrtan,zrtan_hat,zamult,prefrac,prefrac_hat,pradius,pradius_hat,zyt(:,4), &
	 &  zyt_hat,zdydht_hat(:,4)) 

 
!	    yt_prime(:) = y_prime(:) + dydht(:,3)*h_prime + dydht_prime(:,3)*h



	    zy_hat(:) = zy_hat(:) + zyt_hat(:)
	    zdydht_hat(:,3) = zdydht_hat(:,3) + zyt_hat(:)*zh(i,j)
	    DO jj = 1,4
	       zh_hat = zh_hat + zyt_hat(jj)*zdydht(jj,3)
	       
	    ENDDO
	    zyt_hat(:) = 0.0_wp

! 3rd call
	    
	    CALL ropp_fm_gpspderivs_ad(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	  & zrtan,zrtan_hat,zamult,prefrac,prefrac_hat,pradius,pradius_hat,zyt(:,3), &
	  & zyt_hat,zdydht_hat(:,3)) 

	    
!	    yt_prime(:) = y_prime(:) + dydht(:,2)*h2_prime + dydht_prime(:,2)*h2
	       
	    zy_hat(:) = zy_hat(:) + zyt_hat(:)
	    zdydht_hat(:,2) = zdydht_hat(:,2) + zh2*zyt_hat(:)
	    DO jj = 1,4
	       zh2_hat = zh2_hat + zyt_hat(jj)*zdydht(jj,2)
	       
	    ENDDO
	    zyt_hat(:) = 0.0_wp

 
! 2nd call
	    
	    CALL ropp_fm_gpspderivs_ad(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
       &    zrtan,zrtan_hat,zamult,prefrac,prefrac_hat,pradius,pradius_hat,zyt(:,2), &
       &    zyt_hat,zdydht_hat(:,2))


!	    yt_prime(:) = y_prime(:) + dydht(:,1)*h2_prime + dydht_prime(:,1)*h2
	     
	    zy_hat(:) = zy_hat(:) + zyt_hat(:)
	    zdydht_hat(:,1) = zdydht_hat(:,1) + zh2*zyt_hat(:)
	    DO jj = 1,4
	       zh2_hat = zh2_hat + zyt_hat(jj)*zdydht(jj,1)

	    ENDDO
	    zyt_hat(:) = 0.0_wp


! 1st call
	        
	    CALL ropp_fm_gpspderivs_ad(klev,khoriz,i,pdsep,ztheta_min,ztheta_max,ztheta_tan,&
	&   zrtan,zrtan_hat,zamult,prefrac,prefrac_hat,pradius,pradius_hat,zyt(:,1), &
	&   zyt_hat,zdydht_hat(:,1))

	     	     	    
	    zy_hat(:) = zy_hat(:) + zyt_hat(:)
	    zyt_hat(:) = 0.0_wp
	    	          
           
         ENDDO ! j
      
      
         zh_hat = zh_hat + 0.5_wp*zh2_hat
	 zh2_hat = 0.0_wp

!	 
! had problems with this line of code	 
!	 

	 IF (ABS(zh(i,1) - zhmax) < 1.0E3_wp*SPACING(zh(i,1))) zh_hat = 0.0_wp
	 IF (ABS(zh(i,1) - zhmin) < 1.0E3_wp*SPACING(zh(i,1))) zh_hat = 0.0_wp
	 	 	 
	 IF (i == ibot) THEN

	    ik = ikcen
            zdr_max = (pradius(i+1,ik)- zrtan)/REAL(ksplit)	    	    
	 
	    zdr_max_hat = zdr_max_hat + pradius(ibot,ik)/zh(i,1)*zh_hat
	    pradius_hat(ibot,ik) = pradius_hat(ibot,ik) + zdr_max/zh(i,1)*zh_hat	    
	    zh_hat = 0.0_wp
	    	    
            pradius_hat(i+1,ik) = pradius_hat(i+1,ik) + zdr_max_hat/REAL(ksplit)
	    zrtan_hat = zrtan_hat - zdr_max_hat/REAL(ksplit)
	    zdr_max_hat = 0.0_wp 
	    
	    
         ELSE

            ik = INT((zy(2,i,1) + ztheta_tan)/pdsep)+1
	    ik = MIN(MAX(1,ik),khoriz)
     	    zdr_max = (pradius(i+1,ik)-pradius(i,ik))/REAL(ksplit)

! physical limit of step
	    
	    IF (ABS(zh(i,1) - zh_up(i)) < 1.0E3_wp*SPACING(zh(i,1))) THEN
	    
	       zh_up_hat = zh_up_hat + zh_hat
	       zh_hat = 0.0_wp
	       
	    ENDIF   
	       	    
	   
            IF (COS(zy(3,i,1)) < 1.0E-10_wp) THEN
	    
!!	        h_prime = 1.0E10*dr_max_prime
		
		zdr_max_hat = zdr_max_hat + 1.0E10_wp*zh_hat
		zh_hat = 0.0_wp
		
	    ELSE 
	    
!!	       h_prime = h/dr_max*dr_max_prime + h*TAN(y(3))*y_prime(3)   		       
	       
	       zdr_max_hat = zdr_max_hat + zh(i,1)/zdr_max*zh_hat
	       zy_hat(3) = zy_hat(3) + zh(i,1)*TAN(zy(3,i,1))*zh_hat
	       zh_hat = 0.0_wp
	       	       	       
	    ENDIF 

! new limit on step size 

            zdr_max_hat = zdr_max_hat + pradius(i,ik)/zh_up(i)*zh_up_hat
	    pradius_hat(i,ik) = pradius_hat(i,ik) + zdr_max/zh_up(i)*zh_up_hat	    
	    zh_up_hat = 0.0_wp
           
            pradius_hat(i+1,ik) = pradius_hat(i+1,ik) + zdr_max_hat/REAL(ksplit)
	    pradius_hat(i,ik) = pradius_hat(i,ik) - zdr_max_hat/REAL(ksplit)
	    zdr_max_hat = 0.0_wp 
             

	 ENDIF
         
	 
	 ENDIF ! 1d or 2d
	   
            
      ENDDO ! i
      
      zy_hat(:) = 0.0   

      pa_path_hat(in,iside) = 0.0  

!
! don't do iside = 2 if its a 1d calculation
!      
      IF (lone_d_calc) EXIT

   ENDDO ! iside

   zalpha_half_hat(:) = 0.0_wp
 
! might have been over-written

   zrad(1,1) = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))

! if zrtan was close to upper model level

   IF (ibot_old /= ibot) THEN
   
      pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + zrtan_hat
      
      zrtan_hat = 0.0_wp
   
   ENDIF 
    
   IF ( zrad(1,1)*zdndr > -1.0_wp) THEN
  
!!       rad_hat = rad_hat + rtan_hat
!!       rtan_hat = 0.0        
    
  
!       rtan_prime = radius_prime(ibot,kcen) - &
!                    (nr_prime(ibot,kcen) + &
!		    (a(n)-nr(ibot,kcen))/(1.0 + rad(1,1)*dndr)*(rad(1,1)*dndr_prime + dndr*rad_prime))/&  
!                    (1.0 + rad(1,1)*dndr)   

       pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + zrtan_hat 
       pnr_hat(ibot,ikcen) = pnr_hat(ibot,ikcen) - zrtan_hat/(1.0_wp + zrad(1,1)*zdndr)
       
       zdndr_hat = zdndr_hat - &
    &  (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad(1,1)*zdndr)**2*zrad(1,1)*zrtan_hat	
       zrad_hat = zrad_hat - &
    &  (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad(1,1)*zdndr)**2*zdndr*zrtan_hat
       zrtan_hat = 0.0_wp
       
       
   ELSE
       
       zrad_hat = zrad_hat + zrtan_hat
       zrtan_hat = 0.0_wp       
       
   ENDIF      	  
  
  
!!  dndr_hat = 0.0
  
!   dndr_prime = (1.0E-6*(refrac_prime(ibot+1,kcen)-refrac_prime(ibot,kcen)) -  &
!                 dndr*(radius_prime(ibot+1,kcen)-radius_prime(ibot,kcen))) &		 
!		 /(radius(ibot+1,kcen)-radius(ibot,kcen))
 
   prefrac_hat(ibot+1,ikcen) = prefrac_hat(ibot+1,ikcen) + &
 &  1.0E-6_wp*zdndr_hat/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))

   prefrac_hat(ibot,ikcen) = prefrac_hat(ibot,ikcen) - &
 &  1.0E-6_wp*zdndr_hat/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))

   pradius_hat(ibot+1,ikcen) = pradius_hat(ibot+1,ikcen) - &
 &  zdndr/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))*zdndr_hat

   pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + &
 &  zdndr/(pradius(ibot+1,ikcen)-pradius(ibot,ikcen))*zdndr_hat
   zdndr_hat = 0.0_wp

   pradius_hat(ibot,ikcen) = pradius_hat(ibot,ikcen) + 0.5_wp*zrad_hat
   pradius_hat(ibot+1,ikcen) = pradius_hat(ibot+1,ikcen) + 0.5_wp*zrad_hat
   zrad_hat = 0.0_wp
   

ENDDO OBLOOP ! all the observations

palpha_hat(:) = 0.0_wp
pa_path_hat(:,:) = 0.0_wp


!
! adjoint of the kvals
!

DO i = 1,klev-1

   DO j = 1,khoriz
         
      IF (zkval(i,j) > 1.0E-6_wp) THEN
      
	pnr_hat(i,j) = pnr_hat(i,j) + &
      &	zkval(i,j)/MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j)))*zkval_hat(i,j)
	pnr_hat(i+1,j) = pnr_hat(i+1,j) - &
      &	zkval(i,j)/MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j)))*zkval_hat(i,j)
	prefrac_hat(i,j) = prefrac_hat(i,j) + &
      &	zkval_hat(i,j)/(prefrac(i,j)*MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j))))
        prefrac_hat(i+1,j) = prefrac_hat(i+1,j) - &
      &	zkval_hat(i,j)/(prefrac(i+1,j)*MAX(1.0_wp,(pnr(i+1,j)-pnr(i,j))))	

        
	zkval_hat(i,j) = 0.0_wp	    		    
		    
      ELSE
      
        zkval_hat(i,j) = 0.0_wp
            
      ENDIF        

      
   ENDDO
   
ENDDO   

RETURN

END SUBROUTINE ropp_fm_alpha2drk_ad
