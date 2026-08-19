! $Id$

!****s* BendingAngle2d/ropp_fm_gpspderivs_ad *
!
! NAME
!    ropp_fm_gpspderivs_ad - calculates the refractivity derivatives used in
!                 the ray-tracer 
!          
!
! SYNOPSIS
!    call ropp_fm_gpspderivs_ad(klev,khoriz, ...)
! 
! DESCRIPTION
!    This routine calculates the refractivity gradients used in the
!    raytracer. (ADJOINT CODE) 
!
! INPUTS
!
!           klev   =  number of vertical levels
!           khoriz =  number of horizontal locations
!           kk      = in the kkth box in the horizontal
!           pdsep  =  angular spacing
!	    ptheta_min = minium theta 
!           ptheta_max = maximum theta 
!	    ptheta_tan = theta at tangent point & 
!	    prtan = radius of tangent point
!           pamult = 1 or -1 depending on direction
!           prefrac = refractivity
!           pradius = radius value
!           py = current location
!
! OUTPUT
!
!           pdydh = derivs used in the RK calculation
!
! NOTES
!    
!     We limit the magnitude of the radial refractivity gradient to 1.5e-7, to stop the ray
!     getting stuck 
!
!     More information on method in Healy et al, (2007), Q.J.R. Meteorol.Soc., vol 133, 1213-1217, 
!     section 3.2  
!    
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

SUBROUTINE ropp_fm_gpspderivs_ad(klev,   & ! no.of observations
		   khoriz, & ! no. of horizontal layers  ODD
		   kk, &
		   pdsep, &
		   ptheta_min, & 
		   ptheta_max, & 
		   ptheta_tan, & 
		   prtan, &
		   prtan_hat, &
		   pamult, &
		   prefrac, &
		   prefrac_hat, &
		   pradius, &
		   pradius_hat,&
		   py, &
		   py_hat, &
		   pdydh_hat)
		   
USE typesizes, only: wp => EightByteReal

IMPLICIT NONE
		   
INTEGER, INTENT(IN)  :: klev           ! no. of refractivity levels
INTEGER, INTENT(IN)  :: khoriz         ! no. of horizontal locations
INTEGER, INTENT(IN)  :: kk 
REAL(KIND=wp),    INTENT(IN)  :: pdsep           ! angular spacing of grid
REAL(KIND=wp),    INTENT(IN)  :: ptheta_min
REAL(KIND=wp),    INTENT(IN)  :: ptheta_max
REAL(KIND=wp),    INTENT(IN)  :: ptheta_tan
REAL(KIND=wp),    INTENT(IN)  :: prtan
REAL(KIND=wp),    INTENT(INOUT) :: prtan_hat    
REAL(KIND=wp),    INTENT(IN)    :: pamult          !
REAL(KIND=wp),    INTENT(IN)    :: prefrac(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(INOUT) :: prefrac_hat(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(IN)    :: pradius(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(INOUT) :: pradius_hat(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(IN)    :: py(4)   !  current location
REAL(KIND=wp),    INTENT(INOUT) :: py_hat(4)   !  current location
REAL(KIND=wp),    INTENT(INOUT) :: pdydh_hat(4)

! local

INTEGER :: ik,ikp1
REAL(KIND=wp) :: zhwt1,zhwt2
REAL(KIND=wp) :: zhwt1_hat,zhwt2_hat
REAL(KIND=wp) :: zref_up,zref_low
REAL(KIND=wp) :: zrad_up,zrad_low
REAL(KIND=wp) :: zref_up_hat,zref_low_hat
REAL(KIND=wp) :: zrad_up_hat,zrad_low_hat

REAL(KIND=wp) :: zkval,zdndr,zdndr2
REAL(KIND=wp) :: zkval_hat,zdndr_hat,zdndr2_hat
REAL(KIND=wp) :: zdydh(4)
REAL(KIND=wp) :: zed,zed_hat,zed_dum

! local adjoint variables

zref_up_hat = 0.0_wp
zref_low_hat = 0.0_wp
zrad_up_hat = 0.0_wp
zrad_low_hat = 0.0_wp
zhwt1_hat = 0.0_wp
zhwt2_hat = 0.0_wp
zkval_hat = 0.0_wp
zdndr_hat = 0.0_wp
zdndr2_hat = 0.0_wp
zed_hat = 0.0_wp

! the easy bits

IF (COS(py(3)) < 1.0E-10_wp) THEN

   zdydh(1) = 1.0E-10_wp

ELSE

   zdydh(1) = COS(py(3))

ENDIF 

zdydh(2) = pamult*SIN(py(3))/(py(1)+prtan)

IF ( py(2) >= ptheta_min .AND. py(2) <= ptheta_max) THEN

   ik = INT((py(2) + ptheta_tan)/pdsep)+1
   ik = MIN(MAX(1,ik),khoriz)
   ikp1 = MIN(khoriz,ik+1)	

! horizontal weighting factor	       
	       
   zhwt1 = (REAL(ik)*pdsep - (py(2)+ptheta_tan))/pdsep       
   zhwt2 = 1.0_wp - zhwt1
   
ELSE IF (py(2) < ptheta_min ) THEN

   ik = 1
   ikp1 = 2
   zhwt1 = 1.0_wp
   zhwt2 = 0.0_wp
   
ELSE IF  (py(2) > ptheta_max) THEN

   ik = khoriz -1 
   ikp1 = khoriz
   zhwt1 = 0.0_wp
   zhwt2 = 1.0_wp
   
ENDIF    

zref_up  = zhwt1*prefrac(kk+1,ik)+zhwt2*prefrac(kk+1,ikp1)

zref_low = zhwt1*prefrac(kk,ik) + zhwt2*prefrac(kk,ikp1)

zrad_up = zhwt1*pradius(kk+1,ik)+zhwt2*pradius(kk+1,ikp1)

zrad_low = zhwt1*pradius(kk,ik) + zhwt2*pradius(kk,ikp1)

! radial gradient of refractivity

zkval = LOG(zref_low/zref_up)/(zrad_up-zrad_low)

zed = MAX(0.0_wp,(py(1)+prtan-zrad_low))
zed_dum = py(1)+prtan-zrad_low

!!!zdndr = - 1.0E-6_wp*zkval*zref_low*EXP(-zkval*(py(1)+prtan-zrad_low))

zdndr = - 1.0E-6_wp*zkval*zref_low*EXP(-zkval*zed)

zdndr2 = MAX(-1.5E-7_wp,zdndr)

zdydh(3) = -SIN(py(3))*(1.0_wp/(py(1)+prtan) + zdndr2)

zdydh(4) = - SIN(py(3))*zdndr


py_hat(3) = py_hat(3) - COS(py(3))*zdndr*pdydh_hat(4)
zdndr_hat = zdndr_hat - SIN(py(3))*pdydh_hat(4)
pdydh_hat(4) = 0.0_wp


py_hat(3) = py_hat(3) - COS(py(3))*(1.0_wp/(py(1)+prtan) + zdndr2)*pdydh_hat(3)
zdndr2_hat = zdndr2_hat - SIN(py(3))*pdydh_hat(3)
py_hat(1) = py_hat(1) + SIN(py(3))/(py(1)+prtan)**2*pdydh_hat(3)
prtan_hat = prtan_hat + SIN(py(3))/(py(1)+prtan)**2*pdydh_hat(3)
pdydh_hat(3) = 0.0_wp


IF (zdndr > -1.5E-7_wp) THEN

   zdndr_hat = zdndr_hat + zdndr2_hat
   zdndr2_hat = 0.0_wp

ELSE

   zdndr2_hat = 0.0_wp

ENDIF

zkval_hat = zkval_hat + zdndr*(1.0_wp/zkval - zed)*zdndr_hat
zref_low_hat = zref_low_hat + zdndr/zref_low*zdndr_hat
zed_hat = zed_hat - zdndr*zkval*zdndr_hat 
zdndr_hat = 0.0_wp
IF (zed_dum < 0.0_wp) THEN

   zed_hat = 0.0_wp
   
ELSE

   py_hat(1) = py_hat(1) + zed_hat
   prtan_hat = prtan_hat + zed_hat
   zrad_low_hat = zrad_low_hat - zed_hat
   zed_hat = 0.0_wp    

ENDIF

zref_low_hat = zref_low_hat + zkval_hat/(zref_low*(zrad_up-zrad_low))
zref_up_hat = zref_up_hat - zkval_hat/(zref_up*(zrad_up-zrad_low))
zrad_low_hat = zrad_low_hat + zkval/(zrad_up-zrad_low)*zkval_hat
zrad_up_hat = zrad_up_hat - zkval/(zrad_up-zrad_low)*zkval_hat
zkval_hat = 0.0_wp

pradius_hat(kk,ik) = pradius_hat(kk,ik) + zhwt1*zrad_low_hat
zhwt1_hat = zhwt1_hat + pradius(kk,ik)*zrad_low_hat
pradius_hat(kk,ikp1) = pradius_hat(kk,ikp1) + zhwt2*zrad_low_hat
zhwt2_hat = zhwt2_hat + pradius(kk,ikp1)*zrad_low_hat
zrad_low_hat = 0.0_wp

pradius_hat(kk+1,ik) = pradius_hat(kk+1,ik) + zhwt1*zrad_up_hat
zhwt1_hat = zhwt1_hat + pradius(kk+1,ik)*zrad_up_hat
pradius_hat(kk+1,ikp1) = pradius_hat(kk+1,ikp1) + zhwt2*zrad_up_hat
zhwt2_hat = zhwt2_hat + pradius(kk+1,ikp1)*zrad_up_hat
zrad_up_hat = 0.0_wp


prefrac_hat(kk,ik) = prefrac_hat(kk,ik) + zhwt1*zref_low_hat
zhwt1_hat = zhwt1_hat + prefrac(kk,ik)*zref_low_hat
prefrac_hat(kk,ikp1) = prefrac_hat(kk,ikp1) + zhwt2*zref_low_hat
zhwt2_hat = zhwt2_hat + prefrac(kk,ikp1)*zref_low_hat 
zref_low_hat = 0.0_wp
 
prefrac_hat(kk+1,ik) = prefrac_hat(kk+1,ik) + zhwt1*zref_up_hat
zhwt1_hat = zhwt1_hat + prefrac(kk+1,ik)*zref_up_hat
prefrac_hat(kk+1,ikp1) = prefrac_hat(kk+1,ikp1) + zhwt2*zref_up_hat
zhwt2_hat = zhwt2_hat + prefrac(kk+1,ikp1)*zref_up_hat 
zref_up_hat = 0.0_wp



IF ( py(2) >= ptheta_min .AND. py(2) <= ptheta_max) THEN

   zhwt1_hat = zhwt1_hat - zhwt2_hat
   zhwt2_hat = 0.0_wp
   
   py_hat(2) = py_hat(2) - zhwt1_hat/pdsep
   zhwt1_hat = 0.0_wp  
   
ELSE IF (py(2) < ptheta_min ) THEN

   zhwt1_hat = 0.0_wp
   zhwt2_hat = 0.0_wp
   
ELSE IF (py(2) > ptheta_max) THEN

   zhwt1_hat = 0.0_wp
   zhwt2_hat = 0.0_wp
   
ENDIF    


py_hat(3) = py_hat(3) + (pamult*COS(py(3)))/(py(1)+prtan)*pdydh_hat(2)
py_hat(1) = py_hat(1) - zdydh(2)/(py(1)+prtan)*pdydh_hat(2)
prtan_hat = prtan_hat - zdydh(2)/(py(1)+prtan)*pdydh_hat(2)
pdydh_hat(2) = 0.0_wp


IF (COS(py(3)) < 1.0E-10_wp) THEN

   pdydh_hat(1) = 0.0_wp

ELSE

   py_hat(3) = py_hat(3) - SIN(py(3))*pdydh_hat(1)
   pdydh_hat(1) = 0.0_wp

ENDIF 



RETURN



END SUBROUTINE ropp_fm_gpspderivs_ad



