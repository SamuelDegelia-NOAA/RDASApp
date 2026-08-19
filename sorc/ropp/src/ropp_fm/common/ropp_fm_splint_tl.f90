SUBROUTINE ropp_fm_splint_tl(klev,knewlev,khoriz,zg,zg_tl,logn,logn_tl,y2, &
&                         y2_tl,zg_int,zg_int_tl,refrac_int_tl)  
 
! NAME
!   ropp_fm_splint_tl  
!   
! INTERFACE
!   ropp_fm_splint is called from ropp_fm_spline_log_tl
!
! DESCRIPTION
!   Interpolate refractivity onto finer grid
!   Does spline interpolation according to Numerical Recipes p.110

!   klev   =  number of full model levels
!   khoriz =  no. of horizontal locations
!            
!   zg     =  geopotential height on the full model levels
!   refrac =  refractivity on full model levels

! MODIFICATIONS
!
!     Sean Healy      ECMWF               2018/06/26 : Original
!     K. Lonitz       ECMWF               2023/02/06 : input into ropp
!
!
! -------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils

  IMPLICIT NONE

! subroutine args. 

  INTEGER,INTENT(IN)    :: klev  ! no. of p levels in state vec.
  INTEGER,INTENT(IN)    :: khoriz
  INTEGER,INTENT(IN)    :: knewlev ! no. of interpolated leves.

  REAL(wp)   ,INTENT(IN)    :: zg(klev,khoriz) 
  REAL(wp)   ,INTENT(IN)    :: zg_tl(klev,khoriz) 
  REAL(wp)   ,INTENT(IN)    :: logn(klev,khoriz)
  REAL(wp)   ,INTENT(IN)    :: logn_tl(klev,khoriz)
  REAL(wp)   ,INTENT(IN)    :: y2(klev,khoriz)
  REAL(wp)   ,INTENT(IN)    :: y2_tl(klev,khoriz)

  REAL(wp)   ,INTENT(IN)    :: zg_int(knewlev) 
  REAL(wp)   ,INTENT(IN)    :: zg_int_tl(knewlev) 

  REAL(wp)   ,INTENT(OUT)   :: refrac_int_tl(knewlev,khoriz) 

! local variables
 
  
  REAL(wp) :: refrac_int(knewlev,khoriz) 
  
  INTEGER  :: i,j,ipos
  REAL(wp) :: a,b,h, ref_int                     ! for Spline
  REAL(wp) :: a_tl,b_tl,h_tl, ref_int_tl         ! for Spline
  REAL(wp) :: uval, uval_tl                      ! for Spline

! calculate the geometric heights

  refrac_int(:,:)    = ropp_MDFV
  refrac_int_tl(:,:) = 0.0_wp
  

  DO j = 1,khoriz 
    
    DO i= 1,knewlev


       refrac_int(i,j) = 0.0_wp  !missing?

       IF ((zg_int(i) - zg(1,j)) < -1.0_wp) CYCLE ! test for below the surface at j   

       ipos = 1

! find location in vertical
     
       DO 

         IF (zg(ipos+1,j) > zg_int(i) .OR. ipos+1 == klev) EXIT
           
         ipos=ipos+1

       ENDDO  
          
       ipos = MIN(MAX(1,ipos),klev-1)
     
       h   = zg(ipos+1,j) - zg(ipos,j)
     
       h_tl = zg_tl(ipos+1,j) - zg_tl(ipos,j)
       
       a    = (zg(ipos+1,j) - zg_int(i))/h
     
       a_tl = (zg_tl(ipos+1,j) - zg_int_tl(i))/h - (a/h)*h_tl

       b    = 1.0_wp - a
       
       b_tl =  - a_tl

! interpolated Log(refrac)
     
       uval =((a**3-a)*y2(ipos,j)+(b**3-b)*y2(ipos+1,j))*(h**2)/6.0_wp
         
       uval_tl=((a**3-a)*y2_tl(ipos,j)+(b**3-b)*y2_tl(ipos+1,j) + &
&      (3.0_wp*a**2-1.0_wp)*y2(ipos,j)  *a_tl + &
&      (3.0_wp*b**2-1.0_wp)*y2(ipos+1,j)*b_tl)*(h**2)/6.0_wp + &
&      ((a**3-a)*y2(ipos,j)+(b**3-b)*y2(ipos+1,j))*h*h_tl/3.0_wp
              
       ref_int = a*logn(ipos,j)+b*logn(ipos+1,j) + uval

       ref_int_tl = a*logn_tl(ipos,j)+b*logn_tl(ipos+1,j) + &
&      a_tl*logn(ipos,j)+b_tl*logn(ipos+1,j) + uval_tl


! interpolated refractivity
        
       refrac_int(i,j)    = EXP(ref_int)
       
       refrac_int_tl(i,j) = refrac_int(i,j) * ref_int_tl
       
    ENDDO ! i 

  ENDDO ! j


END SUBROUTINE ropp_fm_splint_tl
