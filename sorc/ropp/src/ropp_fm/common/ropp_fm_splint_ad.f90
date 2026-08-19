SUBROUTINE ropp_fm_splint_ad(klev,knewlev,khoriz,zg,zg_ad,logn,logn_ad,y2, &
&                         y2_ad,zg_int,zg_int_ad,refrac_int_ad)  
 
! NAME
!   ropp_fm_splint_ad  
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
!     K. Lonitz       ECMWF               2023/02/15 : input into ropp
!
!
! -------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal


  IMPLICIT NONE

! subroutine args. 

  INTEGER,INTENT(IN)    :: klev  ! no. of p levels in state vec.
  INTEGER,INTENT(IN)    :: khoriz
  INTEGER,INTENT(IN)    :: knewlev ! no. of interpolated leves.

  REAL(wp)   ,INTENT(IN)    :: zg(klev,khoriz) 
  REAL(wp)   ,INTENT(INOUT) :: zg_ad(klev,khoriz) 
  REAL(wp)   ,INTENT(IN)    :: logn(klev,khoriz)
  REAL(wp)   ,INTENT(INOUT) :: logn_ad(klev,khoriz)
  REAL(wp)   ,INTENT(IN)    :: y2(klev,khoriz)
  REAL(wp)   ,INTENT(INOUT) :: y2_ad(klev,khoriz)

  REAL(wp)   ,INTENT(IN)    :: zg_int(knewlev) 
  REAL(wp)   ,INTENT(INOUT) :: zg_int_ad(knewlev) 

  REAL(wp)   ,INTENT(INOUT) :: refrac_int_ad(knewlev,khoriz) 

! local variables
 
  
  REAL(wp) :: refrac_int(knewlev,khoriz) 
  
  INTEGER  :: i,j,ipos
  REAL(wp) :: a,b,h, ref_int                     ! for Spline
  REAL(wp) :: a_ad,b_ad,h_ad, ref_int_ad         ! for Spline
  REAL(wp) :: uval, uval_ad                      ! for Spline

! calculate the geometric heights
  
  a_ad = 0.0_wp
  b_ad = 0.0_wp
  h_ad = 0.0_wp
  uval_ad = 0.0_wp
  ref_int_ad = 0.0_wp

  DO j = 1,khoriz 
    
    DO i= 1,knewlev

      IF ((zg_int(i) - zg(1,j)) < -1.0_wp) CYCLE ! test for below the surface at j   

      ipos = 1

! find location in vertical
     
      DO 

        IF (zg(ipos+1,j) > zg_int(i) .OR. ipos+1 == klev) EXIT
           
        ipos=ipos+1

      ENDDO  
          
      ipos = MIN(MAX(1,ipos),klev-1)
     
      h    = zg(ipos+1,j) - zg(ipos,j)
            
      a    = (zg(ipos+1,j) - zg_int(i))/h

      b    = 1.0_wp - a
       
! interpolated Log(refrac)
     
      uval =((a**3-a)*y2(ipos,j)+(b**3-b)*y2(ipos+1,j))*(h**2)/6.0_wp
     
      ref_int = a*logn(ipos,j)+b*logn(ipos+1,j) + uval

! interpolated refractivity
        
      !refrac_int(i,j)    = EXP(ref_int)
      ref_int   = EXP(ref_int)

!!! adjoint
      !refrac_int_tl(i,j) = refrac_int(i,j) * ref_int_tl
      
      ref_int_ad = ref_int_ad + ref_int * refrac_int_ad(i,j)
      refrac_int_ad(i,j) = 0.0_wp
      
      !     ref_int_tl = a*logn_tl(ipos,j)+b*logn_tl(ipos+1,j) + &
      !&      a_tl*logn(ipos,j)+b_tl*logn(ipos+1,j) + uval_tl

      uval_ad = uval_ad + ref_int_ad
      b_ad = b_ad + logn(ipos+1,j)*ref_int_ad
      a_ad = a_ad + logn(ipos,j)*ref_int_ad
      logn_ad(ipos+1,j) = logn_ad(ipos+1,j) + b*ref_int_ad
      logn_ad(ipos,j)   = logn_ad(ipos,j)   + a*ref_int_ad
      ref_int_ad = 0.0_wp
    
      !uval_tl=((a**3-a)*y2_tl(ipos,j)+(b**3-b)*y2_tl(ipos+1,j) + &
      !& (3.0_wp*a**2-1.0_wp)*y2(ipos,j)  *a_tl + &
      !& (3.0_wp*b**2-1.0_wp)*y2(ipos+1,j)*b_tl)*(h**2)/6.0_wp + &
      !& ((a**3-a)*y2(ipos,j)+(b**3-b)*y2(ipos+1,j))*h*h_tl/3.0_wp
         
      h_ad = h_ad + ((a**3-a)*y2(ipos,j)+(b**3-b)*y2(ipos+1,j))*h*uval_ad/3.0_wp     
      b_ad = b_ad + (3.0_wp*b**2-1.0_wp)*y2(ipos+1,j)*h**2*uval_ad/6.0_wp     
      a_ad = a_ad + (3.0_wp*a**2-1.0_wp)*y2(ipos,j)  *h**2*uval_ad/6.0_wp
      y2_ad(ipos+1,j) = y2_ad(ipos+1,j) + (b**3-b)*h**2*uval_ad/6.0_wp
      y2_ad(ipos,j)   = y2_ad(ipos,j)   + (a**3-a)*h**2*uval_ad/6.0_wp
      uval_ad = 0.0_wp
           
      !b_tl =  - a_tl

      a_ad = a_ad - b_ad
      b_ad = 0.0_wp

      !a_tl = (zg_tl(ipos+1,j) - zg_int_tl(i))/h - (a/h)*h_tl

      h_ad = h_ad - (a/h)*a_ad
      zg_int_ad(i)  = zg_int_ad(i)    - a_ad/h
      zg_ad(ipos+1,j) = zg_ad(ipos+1,j) + a_ad/h
      a_ad = 0.0_wp
 
      !h_tl = zg_tl(ipos+1,j) - zg_tl(ipos,j)

      zg_ad(ipos,j)   = zg_ad(ipos,j)   - h_ad
      zg_ad(ipos+1,j) = zg_ad(ipos+1,j) + h_ad
      h_ad = 0.0_wp

      !refrac_int_tl(i,j) = 0.0_wp  !missing?
      refrac_int_ad(i,j) = 0.0_wp

    ENDDO ! i 

  ENDDO ! j

refrac_int_ad(:,:) = 0.0_wp

END SUBROUTINE ropp_fm_splint_ad
