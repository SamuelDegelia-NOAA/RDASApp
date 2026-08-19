! $Id$

!****s* BendingAngle2d/ropp_fm_alpha2drk_ec *
!
! NAME
!    ropp_fm_alpha2drk_ec - calculate the bending angle "alpha" for impact PARAMETERs a
!
! SYNOPSIS
!    call ropp_fm_alpha2drk_ec(kobs, klev, ...)
! 
! DESCRIPTION
!    It evaluates the bending angle integral for each impact PARAMETER
!
! INPUTS
!  
!           kobs   =  number of observed bending angles 
!           klev   =  number of vertical levels
!           khoriz =  number of horizontal locations
!           ksplit = splitting of model levels
!           pdsep  =  angular spacing
!           pa     =  impact PARAMETERs
!           prefrac=  refractivity values
!           pradius=  radius values
!           pnr    =  nr product 

!
! OUTPUT
! 
!           pa_path = impact PARAMETER at end points of ray path
!           palpha  = bending angle values
!
! NOTES
!     1) calculate the exponential decay of refractivity with nr between model levels
!     2) evaluates the bending angle integral for each impact PARAMETER
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


SUBROUTINE ropp_fm_alpha2drk_ec(kobs,    & ! no.of observations
                           & klev,    & ! no. of vertical levels
                           & khoriz,  & ! no. of horizontal layers  odd
                           & ksplit,  &  
                           & pdsep,   & ! the angular spacing 
                           & pa,      & ! impact PARAMETER values
                           & prefrac, & ! refractivity
                           & pradius, & ! radius values
                           & pnr,     &
                           & proc,    &
                           & pz_2d,   &
                           & pa_path, &
                           & palpha)!!,  & KaLo
!!                           & k_levm,  &  KaLo
!!                           & p_pwt)      KaLo


  USE ropp_utils, ONLY: ropp_MDFV
  USE typesizes, ONLY: wp => EightByteREAL
  USE ropp_fm_constants, ONLY : pi
  USE ropp_fm, not_this => ropp_fm_alpha2drk_ec

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: kobs                 ! size of ob. vector
  INTEGER, INTENT(IN)  :: klev                 ! no. of refractivity levels
  INTEGER, INTENT(IN)  :: khoriz               ! no. of horizontal locations
  INTEGER, INTENT(IN)  :: ksplit
  REAL(wp),    INTENT(IN)  :: pdsep                ! angular spacing of grid
  REAL(wp),    INTENT(IN)  :: pa(kobs)             ! impact PARAMETER 
  REAL(wp),    INTENT(IN)  :: prefrac(klev,khoriz) ! refractivity values on levels
  REAL(wp),    INTENT(IN)  :: pradius(klev,khoriz) ! radius values
  REAL(wp),    INTENT(IN)  :: pnr(klev,khoriz)
  REAL(wp),    INTENT(IN)  :: proc                 ! radius of curvature
  REAL(wp),    INTENT(IN)  :: pz_2d
  REAL(wp),    INTENT(OUT) :: pa_path(kobs,2)        
  REAL(wp),    INTENT(OUT) :: palpha(kobs)         ! path length

  !!INTEGER , INTENT(OUT) :: k_levm(kobs) ! for the enkf   KaLo I probably do not need them                
  !!REAL(wp), INTENT(OUT) :: p_pwt(kobs)  ! for the enkf   KaLo I probably do not need them

! local variables

  INTEGER :: i,j,in,ibot,jbot,ikbot,iside,ik,ikp1,in_2d,ikcen,jj,isplit,i_below
  REAL(wp), PARAMETER :: zhmax = 5.0e4_wp
  REAL(wp), PARAMETER :: zhmin = 1.0e2_wp
  REAL(wp), PARAMETER :: dn_dx_max = 0.157_wp ! setting the maxiumin N gradients
  REAL(wp) :: zrad,zdndr
  REAL(wp) :: zhwt1,zhwt2
  REAL(wp) :: zamult
  REAL(wp) :: zh,zh2,zhuse,zh_up
  REAL(wp) :: zy(4),zyt(4)
  REAL(wp) :: zdydh(4)
  REAL(wp) :: ztheta_tan,ztheta_min,ztheta_max
  REAL(wp) :: zdr_max,zdr_dtheta,zrtan,zdr
  REAL(wp) :: zalpha_half(2)
  REAL(wp) :: zkval(klev-1,khoriz)
  REAL(wp) :: ztlow,ztup,zdalpha,zroot_halfpi
  REAL(wp) :: zerf_up,zerf_low,zt,zdiff_erf,znr_low,zref_low,zaval
  REAL(wp) :: zrad_up,zrad_low,zref_up,zkval_theta,zdndr2,zdn_dx,zed
  LOGICAL :: llfirst_1d,lleaving,llone_d_calc,ll_intercept


!-------------------------------------------------------------------------------
! 2. Set up the central profile kcen 
!-------------------------------------------------------------------------------

  ikcen = khoriz/2 + 1
  ztheta_tan = REAL(ikcen-1)*pdsep 
  ztheta_min = -ztheta_tan
  ztheta_max =  ztheta_tan

!-------------------------------------------------------------------------------
! 3. Set the kvals used in the 1d calculation
!-------------------------------------------------------------------------------

  zkval(:,:) = 1.5e-4_wp ! climatological value 

  DO i = 1,klev-1
    DO j = 1, khoriz
   
      IF (prefrac(i,j) > 0.0_wp .AND. prefrac(i+1,j) > 0.0_wp) THEN
   
       zkval(i,j) = LOG(prefrac(i,j)/prefrac(i+1,j))/MAX((pnr(i+1,j) - pnr(i,j)),1.0_wp)
       zkval(i,j) = MAX(1.0e-6_wp,zkval(i,j))
       zkval(i,j) = MIN(zkval(i,j),(dn_dx_max/prefrac(i,j)))
      
      ENDIF        
           
    ENDDO 
  ENDDO   


!-------------------------------------------------------------------------------
! 4. Set n_2d level. For levels below n_2d we do 2D ray bending calculation
! above n_2d we do the 1D calculation
!-------------------------------------------------------------------------------

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

    IF ((pnr(ikbot,ikcen) - pnr(ikbot-1,ikcen)) < 1.0_wp) EXIT 

    ikbot = ikbot - 1

  ENDDO
 
  jbot = MAX(jbot,ikbot)


!-------------------------------------------------------------------------------
! 5. Set the outputs to missing
!-------------------------------------------------------------------------------

  palpha(:)   = ropp_MDFV
  pa_path(:,:)= ropp_MDFV
  !! KaLo p_pwt(:)    = ropp_MDFV   !

!-------------------------------------------------------------------------------
! 6. 2D bending angle calculation
!-------------------------------------------------------------------------------

  zroot_halfpi = SQRT(0.5_wp*pi)

  obloop: DO in=1,kobs
      
    IF (pa(in) < pnr(jbot,ikcen) .OR. pa(in) > pnr(klev-3,ikcen)) CYCLE  
         
    ibot = jbot

    DO 

      IF (pnr(ibot+1,ikcen) - pa(in) > 1.0_wp) EXIT 

      ibot=ibot+1

     ENDDO

! 6.1 Calculate the radius at tangent point   
! -----------------------------------------

    zrad = 0.5_wp*(pradius(ibot,ikcen)+pradius(ibot+1,ikcen))
    zdndr = 1.0e-6_wp*(prefrac(ibot+1,ikcen)-prefrac(ibot,ikcen))/ &
               &  (pradius(ibot+1,ikcen)-pradius(ibot,ikcen)) 
  
    IF ( zrad*zdndr > -1.0_wp) THEN
  
      zrtan = pradius(ibot,ikcen) + &
             & (pa(in)-pnr(ibot,ikcen))/(1.0_wp + zrad*zdndr)

    ELSE
   
      zrtan = zrad   ! probably in a super-refracting layer
       
    ENDIF   

! 6.2 If zrtan is within a 1 m of upper level set to upper level
! --------------------------------------------------------------
   
    IF ((zrtan - pradius(ibot+1,ikcen)) > -1.0_wp) THEN

      ibot = ibot + 1
          
      zrtan = pradius(ibot,ikcen)
       
    ENDIF     

! 6.3 save the tangent level number for the enkf
! --------------------------------------------------------------
 
    !! k_levm(in) = ibot KaLo
   
! 6.4 for computing the pressure at the tangent point   
! --------------------------------------------------------------
   
    !! KaLo p_pwt(in) = (pradius(ibot+1,ikcen)-zrtan)/ &
!!&  MAX(1.0_wp,(pradius(ibot+1,ikcen)-pradius(ibot,ikcen)))

  !! KaLo  p_pwt(in) = MIN(MAX(p_pwt(in),0.0_wp),1.0_wp)

! 6.5 don't calculate ba's that may hit orography 
! --------------------------------------------------------------
        
    i_below = 0
   
    DO i = ikcen-1, ikcen+1
   
      IF (prefrac(ibot,i) < 0.0_wp .OR.prefrac(ibot,i) == ropp_MDFV) i_below = i_below + 1
      
    ENDDO
   
    IF (i_below /= 0) cycle    
                       

! 6.6 set bending angle value  
! ---------------------------  

    zalpha_half(:) = 0.0_wp
   
    ll_intercept = .false.

    DO iside = 1,2 
      
      pa_path(in,iside) = pa(in)
      llfirst_1d = .true.
      llone_d_calc = .false.
      zamult = 1.0_wp
      IF (iside == 2) zamult  = -1.0_wp
      

! 6.7 initialise vector
! ---------------------------  


      zy(1) = 0.0_wp           ! height above tangent point           
      zy(2) = 0.0_wp           ! theta
      zy(3) = ASIN(1.0_wp)     ! thi
      zy(4) = 0.0_wp           ! bending angle 
        
      DO i = ibot,klev-1
        
        IF ( i < MIN(in_2d,klev-1)) THEN  
       
! 6.7.1 set the splitting  

          isplit = ksplit

! 6.7.2 smaller steps near tangent point.

          IF (i - ibot < 2) isplit = 2*ksplit


          IF (i == ibot) THEN
         
            ik = ikcen         

            zdr_max = (pradius(i+1,ik)- zrtan)/REAL(isplit) 
                 
            zh = SQRT(2.0_wp*pradius(ibot,ik)*zdr_max)
            
          ELSE 
 
            ik = INT((zy(2) + ztheta_tan)/pdsep)+1
            ik = MIN(MAX(1,ik),khoriz)
      
            IF ((prefrac(i,ik) < 0.0_wp)   .OR.&
             &prefrac(i,ik) == ropp_MDFV) THEN 
      
              ll_intercept = .true.
      
            ELSE
      
              zdr_max = (pradius(i+1,ik)- pradius(i,ik))/REAL(isplit)               
              zh_up = SQRT(2.0_wp*pradius(i,ik)*zdr_max)                                        
              zh = zdr_max/MAX(COS(zy(3)),1.0e-10_wp)        
              zh = MIN(zh_up,zh) 
        
            ENDIF 

          ENDIF

          IF (ll_intercept) EXIT 

! 6.8 estimate the step-length        
! -----------------------------

                 
! 6.8.1 limit to horizontal distance between grid points


          zh = MAX(MIN(zh,zhmax),zhmin)          
         
          zh2 = 0.5_wp*zh


! 6.9 now calculate the path with a runge-kutta
! ----------------------------------------------

          lleaving = .false. 
         
          DO  j = 1,isplit 
                 
            DO jj = 1, 2 
                            
              zyt(:) = zy(:)  
            
              IF (jj == 2) zyt(:) = zyt(:) + zdydh(:)*zh2   ! use gradients from jj=1 loop

! 6.9.1 where are we in the plane    
                 
                ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
                ik = MIN(MAX(1,ik),khoriz-1)
                ikp1 = ik+1

! 6.10 intercepted the orography
! -----------------------------

                IF ((prefrac(i,ik) < 0.0_wp)   .OR.&
                   &   (prefrac(i,ikp1) < 0.0_wp) .OR. &
                   &    prefrac(i,ik) == ropp_MDFV       .OR. &
                   &    prefrac(i,ikp1) == ropp_MDFV ) THEN 

                ! intercepted the orography - EXIT

                  ll_intercept = .true.

                ENDIF

                IF (ll_intercept) EXIT

! 6.11 horizontal weighting factor               
! ----------------------------------
            
                IF ( zyt(2) < ztheta_max .AND. zyt(2) > ztheta_min) THEN
                           
                       zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
                       zhwt2 = 1.0_wp - zhwt1
            
                ELSEIF (zyt(2) < ztheta_min) THEN            
            
                       zhwt1 = 1.0_wp
                       zhwt2 = 0.0_wp
               
                ELSEIF (zyt(2) > ztheta_max) THEN
                       
                       zhwt1 = 0.0_wp
                       zhwt2 = 1.0_wp
               
                ENDIF

!6.12 calculate the gradients at that point
! ----------------------------------

                zdydh(1) = MAX(1.0e-10_wp,COS(zyt(3)))       

                zdydh(2) = zamult*SIN(zyt(3))/(zyt(1)+zrtan)

!6.13 interpolate refractivity AND radius to point in plane.
! ---------------------------------------------------------
    
                zref_up  = zhwt1*prefrac(i+1,ik)+zhwt2*prefrac(i+1,ikp1)
                zref_low = zhwt1*prefrac(i,ik) + zhwt2*prefrac(i,ikp1)

                zrad_up = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)
                zrad_low = zhwt1*pradius(i,ik) + zhwt2*pradius(i,ikp1)
   
! 6.14 estimate radial gradient
! ----------------------------------
   
                IF ((zref_up - zref_low) > -1.0e-10_wp) THEN
      
                  zdndr2 = 1.0e-6_wp*(zref_up-zref_low)/(zrad_up-zrad_low)  ! +ve refrac gradient
   
                ELSE
   
                  zkval_theta = LOG(zref_low/zref_up)/(zrad_up-zrad_low)

                  zed = MAX(0.0_wp,(zyt(1)+zrtan-zrad_low))
         
                  zdndr = - 1.0e-6_wp*zkval_theta*zref_low*EXP(-zkval_theta*zed)

                  zdndr2 = MAX(-0.75e-7_wp,zdndr)
  
                ENDIF  
  
                  zdydh(3) = -sin(zyt(3))*(1.0_wp/(zyt(1)+zrtan) + zdndr2)

                  zdydh(4) = - sin(zyt(3))*zdndr2
  
              ENDDO  ! jj

! 6.15 ray has intercepted the model surface
! ---------------------------------------------

              IF (ll_intercept) EXIT 
        
! 6.16 update with latest estimate of gradient            
! ----------------------------------
   
              zyt(:) = zy(:) + zdydh(:)*zh
            
! 6.17 check the radius - have we EXITed the level
! ----------------------------------
                        
              ik = INT((zyt(2) + ztheta_tan)/pdsep)+1
              ik = MIN(MAX(1,ik),khoriz-1)
              ikp1 = ik+1
            
! 6.18 next step
! ----------------------------------
      
              IF ((prefrac(i,ik) < 0.0_wp)   .OR.&
              &   (prefrac(i,ikp1) < 0.0_wp) .OR. &
              &    prefrac(i,ik) == ropp_MDFV       .OR. &
              &    prefrac(i,ikp1) == ropp_MDFV ) ll_intercept = .true.


! 6.19 will next step hit orography
! ----------------------------------
      
              IF (ll_intercept) EXIT 
                     
! 6.20 horizontal weighting factor      !this is repetative, Why? Runge Kutta = 2 KaLo???         
! ----------------------------------
            
              IF ( zyt(2) < ztheta_max .AND. zyt(2) > ztheta_min) THEN
                           
                zhwt1 = (REAL(ik)*pdsep - (zyt(2)+ztheta_tan))/pdsep    
                zhwt2 = 1.0_wp - zhwt1
            
              ELSEIF (zyt(2) < ztheta_min) THEN            
            
                zhwt1 = 1.0_wp
                zhwt2 = 0.0_wp
               
              ELSEIF (zyt(2) > ztheta_max) THEN
            
                zhwt1 = 0.0_wp
                zhwt2 = 1.0_wp
               
              ENDIF    
                              
                    
! 6.21 radius of pressure level             
! ----------------------------------

              zrad = zhwt1*pradius(i+1,ik)+zhwt2*pradius(i+1,ikp1)   
      
            
! 6.22 IF gone over the boundary scale h
! ----------------------------------
            
              IF ( j == isplit .OR. (zyt(1)+zrtan - zrad) > 0.0_wp ) THEN
            
                lleaving = .true.
            
                zdr_dtheta = 0.0_wp
            
                IF (zyt(2) < ztheta_max .AND. zyt(2) > ztheta_min) &            
                 & zdr_dtheta = (pradius(i+1,ikp1)-pradius(i+1,ik))/pdsep
        
                  zhuse = zh - (zyt(1)+zrtan-zrad)/(zdydh(1) - zdr_dtheta*zdydh(2))

                  zhuse = MAX(MIN(zhuse,zhmax),zhmin)
                                             
                ELSE 
            
                  zhuse = zh  
                           
              ENDIF 
         
                        
! 6.23 update the position vector
! ----------------------------------

              zy(:) = zy(:) + zdydh(:)*zhuse
                   
              IF (lleaving) EXIT  

! 6.24 try to maintain roughly the same radial increment by adjusting h
! ----------------------------------------------------------------------

              IF (j < isplit) THEN
         
                zdr = (zrad-zy(1)-zrtan)/REAL(isplit-j)
         
                zh = MIN(zh,zdr/MAX(cos(zy(3)),1.0e-10_wp))
            
                zh = MAX(MIN(zh,zhmax),zhmin)
            
                zh2 = 0.5_wp*zh
                                
              ENDIF 

            ENDDO  ! complete path thru ith layer

          ELSE 

! ------------------------------------------------------------------------------
! 7. Do 1D calculation
! ------------------------------------------------------------------------------


            IF (llfirst_1d) THEN
         
              ik = NINT((zy(2) + ztheta_tan)/pdsep)+1
              ik = MIN(MAX(1,ik),kHORIz-1)
              pa_path(in,iside) = &
             &(1.0_wp+1.0e-6_wp*prefrac(i,ik))*((zy(1)+zrtan)*sin(zy(3)))
              zaval = pa_path(in,iside)
   
! testing.
   
              zaval = pa(in)  !KaLo???
              zalpha_half(iside) = zy(4)
              llfirst_1d = .false.
            
            ENDIF     

! 7.1 Continue with 1D bending angle calculation
! ----------------------------------------------

            IF ( i == ibot) THEN 

! we are doing a 1d calc for entire ray path
      
              llone_d_calc = .true. 
              zref_low = prefrac(ibot,ik)*EXP(-zkval(ibot,ik)*(pa(in)-pnr(ibot,ik)))
              zaval = pa(in)
              pa_path(in,iside) = pa(in)  
              znr_low = pa(in)
        
            ELSE 
      
              zref_low = prefrac(i,ik) 
              znr_low  = pnr(i,ik) 
         
           ENDIF

         
           IF ((prefrac(i+1,ik)-prefrac(i,ik)) > -1.0e-10_wp) THEN  
!
! 7.2 allow the refractivity to increase with height when calculating bending angle 
! occurs in ~ 8% of cases.
! --------------------------------------------------------------------------------

! assume a constant gradient 
    
             zdn_dx = (prefrac(i+1,ik)-prefrac(i,ik))/(pnr(i+1,ik)-pnr(i,ik))
       
             ztup = SQRT( pnr(i+1,ik)-pa(in))       
      
             ztlow = 0.0_wp
       
             IF (i > ibot)  ztlow = SQRT( pnr(i,ik)-pa(in))
           
             zdalpha  = - 1.0e-6_wp*SQRT(2.0_wp*pa(in))* zdn_dx*(ztup-ztlow)
      
           ELSE

             ztlow = 0.0_wp
             IF (i > ibot) ztlow = SQRT(MAX(zkval(i,ik)*(pnr(i,ik) - zaval),1.0e-10_wp))

             ztup = SQRT(MAX(zkval(i,ik)*(pnr(i+1,ik)-zaval),1.0e-10_wp))


! 7.2 Calculate the error functions within this routine rather than an external function call.
! -----------------------------------------------------------------------------

             IF (i == ibot) THEN
                                   
               zerf_low = 0.0_wp         
                 
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztup)
            
               zerf_up= &
              &1.0_wp-(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztup*ztup))            
         
             ELSEIF (i > ibot .AND. i < klev-1) THEN
                    
! lower
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztlow) 
            
               zerf_low = -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztlow*ztlow)) 

! upper
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztup)
            
               zerf_up= -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztup*ztup))            
                        
             ELSE
         
               zerf_up = 0.0_wp 
         
               zt = 1.0_wp/(1.0_wp+0.47047_wp*ztlow) 
            
               zerf_low = -(0.3480242_wp-(0.0958798_wp-0.7478556_wp*zt)*zt)*zt*EXP(-(ztlow*ztlow)) 
            
             ENDIF     
          
          
             zdiff_erf = zerf_up - zerf_low 

! bending angle          
         
             zdalpha    =  &
            & 1.0e-6_wp * zroot_halfpi* SQRT(zaval*zkval(i,ik)) & 
            & * zref_low*EXP(zkval(i,ik)*(znr_low-zaval))*zdiff_erf 
 
 
          ENDIF 
 
          zalpha_half(iside) = zalpha_half(iside) + zdalpha 
          

        ENDIF 
      
        IF (ll_intercept) EXIT   ! the model level loop                                
          
      ENDDO  ! i the layers
      
      IF (ll_intercept) EXIT   ! the iside loop              

! IF we performed a 1d calculation don't evaluate iside = 2

      IF (llone_d_calc) THEN
      
        zalpha_half(2) = zalpha_half(1)
          
        EXIT  ! EXITing the iside loop
     
      ENDIF      

    ENDDO ! iside

! ------------------------------------------------------------------------------
! 8. The total bending angle adding both sides
! ------------------------------------------------------------------------------

    IF (ll_intercept) THEN

! hit the model orography along 2d path

      palpha(in) = ropp_MDFV
   
    ELSE
       
      palpha(in) = zalpha_half(1) + zalpha_half(2) 
      
    ENDIF  
     
   
  ENDDO obloop

RETURN

END SUBROUTINE ropp_fm_alpha2drk_ec
