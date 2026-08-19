! $Id$

!****s* BendingAngle2d/ropp_fm_gpspderivs_tl *
!
! NAME
!    ropp_fm_gpspderivs_tl - calculates the refractivity derivatives used in
!                 the ray-tracer 
!          
!
! SYNOPSIS
!    call ropp_fm_gpspderivs_tl(klev,khoriz, ...)
! 
! DESCRIPTION
!    This routine calculates the refractivity gradients used in the
!    raytracer.  (TANGENT LINEAR CODE)
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


SUBROUTINE ropp_fm_gpspderivs_tl(klev,   & ! no.of observations
		   khoriz, & ! no. of horizontal layers  ODD
		   kk, &
		   pdsep, &
		   ptheta_min, & 
		   ptheta_max, & 
		   ptheta_tan, & 
		   prtan, &
		   prtan_prime, &
		   pamult, &
		   prefrac, &
		   prefrac_prime, &
		   pradius, &
		   pradius_prime,&
		   py, &
		   py_prime, &
		   pdydh, &
		   pdydh_prime)


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
REAL(KIND=wp),    INTENT(IN)  :: prtan_prime    
REAL(KIND=wp),    INTENT(IN)  :: pamult          !
REAL(KIND=wp),    INTENT(IN)  :: prefrac(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(IN)  :: prefrac_prime(klev,khoriz)   ! refractivity values on levels
REAL(KIND=wp),    INTENT(IN)  :: pradius(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(IN)  :: pradius_prime(klev,khoriz)   ! radius values
REAL(KIND=wp),    INTENT(IN)  :: py(4)   !  current location
REAL(KIND=wp),    INTENT(IN)  :: py_prime(4)   !  current location
REAL(KIND=wp),    INTENT(OUT) :: pdydh(4)
REAL(KIND=wp),    INTENT(OUT) :: pdydh_prime(4)

! local

INTEGER :: ik,ikp1
REAL(KIND=wp) :: zhwt1,zhwt2
REAL(KIND=wp) :: zhwt1_prime,zhwt2_prime
REAL(KIND=wp) :: zref_up,zref_low
REAL(KIND=wp) :: zrad_up,zrad_low
REAL(KIND=wp) :: zref_up_prime,zref_low_prime
REAL(KIND=wp) :: zrad_up_prime,zrad_low_prime

REAL(KIND=wp) :: zkval,zdndr,zdndr2
REAL(KIND=wp) :: zkval_prime,zdndr_prime,zdndr2_prime
REAL(KIND=wp) :: zed,zed_prime

! the easy bits


IF (COS(py(3)) < 1.0E-10_wp) THEN

   pdydh(1) = 1.0E-10_wp
   pdydh_prime(1) = 0.0_wp

ELSE

   pdydh(1) = COS(py(3))
   pdydh_prime(1) = - SIN(py(3))*py_prime(3)


ENDIF 


pdydh(2) = pamult*SIN(py(3))/(py(1)+prtan)
pdydh_prime(2) = (pamult*COS(py(3))*py_prime(3)-pdydh(2)*&
(py_prime(1) + prtan_prime))/(py(1)+prtan)

IF ( py(2) >= ptheta_min .AND. py(2) <= ptheta_max) THEN

   ik = INT((py(2) + ptheta_tan)/pdsep)+1
   ik = MIN(MAX(1,ik),khoriz)
   ikp1 = MIN(khoriz,ik+1)	

! horizontal weighting factor	       
	       
   zhwt1 = (REAL(ik)*pdsep - (py(2)+ptheta_tan))/pdsep 
   zhwt1_prime = - py_prime(2)/pdsep
      
   zhwt2 = 1.0_wp - zhwt1
   zhwt2_prime = - zhwt1_prime
   
   
ELSE IF (py(2) < ptheta_min ) THEN

   ik = 1
   ikp1 = 2
   zhwt1 = 1.0_wp
   zhwt1_prime = 0.0_wp
   zhwt2 = 0.0_wp
   zhwt2_prime = 0.0_wp
   
ELSE IF  (py(2) > ptheta_max) THEN

   ik = khoriz -1 
   ikp1 = khoriz
   zhwt1 = 0.0_wp
   zhwt1_prime = 0.0_wp
   zhwt2 = 1.0_wp
   zhwt2_prime = 0.0_wp
   
ENDIF    


zref_up  = zhwt1*prefrac(kk+1,ik)+zhwt2*prefrac(kk+1,ikp1)

zref_up_prime = zhwt1*prefrac_prime(kk+1,ik) + &
               prefrac(kk+1,ik)*zhwt1_prime + &
	       zhwt2*prefrac_prime(kk+1,ikp1) + &
	       prefrac(kk+1,ikp1)*zhwt2_prime 


zref_low = zhwt1*prefrac(kk,ik) + zhwt2*prefrac(kk,ikp1)

zref_low_prime = zhwt1*prefrac_prime(kk,ik) + &
                prefrac(kk,ik)*zhwt1_prime + &
		zhwt2*prefrac_prime(kk,ikp1) + &
		prefrac(kk,ikp1)*zhwt2_prime 


zrad_up = zhwt1*pradius(kk+1,ik)+zhwt2*pradius(kk+1,ikp1)

zrad_up_prime = zhwt1*pradius_prime(kk+1,ik) + &
               pradius(kk+1,ik)*zhwt1_prime + &
	       zhwt2*pradius_prime(kk+1,ikp1) + &
	       pradius(kk+1,ikp1)*zhwt2_prime 

zrad_low = zhwt1*pradius(kk,ik) + zhwt2*pradius(kk,ikp1)

zrad_low_prime = zhwt1*pradius_prime(kk,ik) + &
                pradius(kk,ik)*zhwt1_prime + &
		zhwt2*pradius_prime(kk,ikp1) + &
		pradius(kk,ikp1)*zhwt2_prime 


! radial gradient of refractivity

zkval = LOG(zref_low/zref_up)/(zrad_up-zrad_low)


zkval_prime = (zref_low_prime/zref_low - zref_up_prime/zref_up + &
              zkval*(zrad_low_prime - zrad_up_prime))/(zrad_up-zrad_low)


zed = MAX((py(1)+prtan-zrad_low),0.0_wp)

IF ((py(1)+prtan-zrad_low) >  0.0_wp) THEN

   zed_prime = py_prime(1) + prtan_prime - zrad_low_prime
   
ELSE   

   zed_prime = 0.0_wp

ENDIF 

!!zdndr = - 1.0E-6_wp*zkval*zref_low*EXP(-zkval*(py(1)+prtan-zrad_low))

zdndr = - 1.0E-6_wp*zkval*zref_low*EXP(-zkval*zed)

zdndr_prime = zdndr*((1.0_wp/zkval - zed)*zkval_prime + &
                    zref_low_prime/zref_low - &
		    zkval*zed_prime)


zdndr2 = MAX(-1.5E-7_wp,zdndr)

IF (zdndr > -1.5E-7_wp) THEN

   zdndr2_prime = zdndr_prime

ELSE

   zdndr2_prime = 0.0_wp

ENDIF


pdydh(3) = -SIN(py(3))*(1.0_wp/(py(1)+prtan) + zdndr2)
   
pdydh_prime(3)  = - COS(py(3))*(1.0_wp/(py(1)+prtan) + zdndr2)*py_prime(3) - &
                   SIN(py(3))*(zdndr2_prime - 1.0_wp/(py(1)+prtan)**2* &
		   (py_prime(1) + prtan_prime))


pdydh(4) = - SIN(py(3))*zdndr

pdydh_prime(4)  = - COS(py(3))*zdndr*py_prime(3) - SIN(py(3))*zdndr_prime


RETURN



END SUBROUTINE ropp_fm_gpspderivs_tl



