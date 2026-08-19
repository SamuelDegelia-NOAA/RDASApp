SUBROUTINE ropp_fm_spline_init_ad(klev,x,x_ad,y,y_ad,y2,y2_ad)  
 

! NAME
!   ropp_fm_spline_init  
!   
! INTERFACE
!
! ropp_fm_spline_init is called from ropp_fm_spline_log
!       
! DESCRIPTION
! 
!  Does initialisation for spline interpolation according to Numerical Recipes p.109
!  It generates second derivatives y2 with lower and upper boundaru conditions set to "natural" (u(n),u(1),ye(n),y2(1) = 0)
!
! MODIFICATIONS
!
!     Sean Healy      ECMWF               2018/06/26 : Original
!     K. Lonitz       ECMWF               2023/02/15 : input into ropp

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE


  INTEGER,INTENT(IN)        :: klev    ! no. of model levels in state vec.
  REAL(wp)   ,INTENT(IN)    :: x(klev) 
  REAL(wp)   ,INTENT(INOUT) :: x_ad(klev)
  REAL(wp)   ,INTENT(IN)    :: y(klev) 
  REAL(wp)   ,INTENT(INOUT) :: y_ad(klev) 
  REAL(wp)   ,INTENT(OUT)   :: y2(klev)
  REAL(wp)   ,INTENT(INOUT) :: y2_ad(klev) 

! local variables

  INTEGER  :: i
  REAL(wp) :: u(klev), u_ad(klev)          
  REAL(wp) :: sig(klev),p(klev)
  REAL(wp) :: sig_ad, p_ad
  REAL(wp) :: dy_dx_up(klev),dy_dx_low(klev) 
  REAL(wp) :: dy_dx_up_ad,dy_dx_low_ad 
  REAL(wp) :: py_dum(klev)
  REAL(wp) :: dum(klev), dum_ad   


  y2(:)        = 0.0_wp
  u(:)         = 0.0_wp
  u_ad(:)      = 0.0_wp
  sig_ad       = 0.0_wp
  p_ad         = 0.0_wp
  dy_dx_low_ad = 0.0_wp
  dy_dx_up_ad  = 0.0_wp
  dum_ad       = 0.0_wp
  


  DO i = 2,klev-1

     sig(i) = (x(i)-x(i-1))/(x(i+1)-x(i-1))

     p(i)=sig(i)*y2(i-1)+2.0_wp

     y2(i)=(sig(i)-1.0_wp)/p(i)
      
     dy_dx_up(i)  = (y(i+1)-y(i))/(x(i+1)-x(i))
        
     dy_dx_low(i) = (y(i)-y(i-1))/(x(i)-x(i-1))
   
     dum(i) = 6.0_wp*(dy_dx_up(i) - dy_dx_low(i))/(x(i+1)-x(i-1))
  
     u(i)=(dum(i) - sig(i)*u(i-1))/p(i)
      
  ENDDO ! i

  y2(klev)    = 0.0_wp  ! for clarity

  DO i = klev-1,1,-1

   py_dum(i) = y2(i)
   
   y2(i) = py_dum(i)*y2(i+1) + u(i)
      
  ENDDO

!!! Adjoint (note order!)

  DO i = 1,klev-1

   !y2_tl(i) = py_dum(i)*y2_tl(i+1)  + y2(i+1)*y2_tl(i) + u_tl(i)

   y2_ad(i+1) =  y2_ad(i+1) + py_dum(i)*y2_ad(i)

   u_ad(i)    = u_ad(i)     + y2_ad(i)

   y2_ad(i)   = y2(i+1)*y2_ad(i)

  ENDDO
  
  y2_ad(klev) = 0.0_wp  


  DO i = klev-1,2,-1
    
    !u_tl(i) = (dum_tl - (sig_tl*u(i-1)+sig*u_tl(i-1)))/p - &
     !&(u(i)/p)*p_tl
   
    dum_ad = dum_ad + u_ad(i)/p(i)
    sig_ad = sig_ad - (u(i-1)/p(i))*u_ad(i)
    u_ad(i-1) = u_ad(i-1) - (sig(i)/p(i))*u_ad(i)
    p_ad = p_ad - (u(i)/p(i))*u_ad(i)
    u_ad(i) = 0.0_wp
 
    !dum_tl = 6.0*(dy_dx_up_tl - dy_dx_low_tl)/(x(i+1)-x(i-1)) - &
     !&(dum/(x(i+1)-x(i-1)))*(x_tl(i+1)-x_tl(i-1))

    dy_dx_up_ad  = dy_dx_up_ad  + (6.0_wp/(x(i+1)-x(i-1)))*dum_ad
    dy_dx_low_ad = dy_dx_low_ad - (6.0_wp/(x(i+1)-x(i-1)))*dum_ad
    x_ad(i+1) = x_ad(i+1) - (dum(i)/(x(i+1)-x(i-1)))*dum_ad
    x_ad(i-1) = x_ad(i-1) + (dum(i)/(x(i+1)-x(i-1)))*dum_ad
    dum_ad = 0.0_wp

    !dy_dx_low_tl = (y_tl(i)-y_tl(i-1))/(x(i)-x(i-1)) - &
    !&(dy_dx_low/(x(i)-x(i-1)))*(x_tl(i)-x_tl(i-1))

    y_ad(i)   = y_ad(i)   + dy_dx_low_ad/(x(i)-x(i-1))
    y_ad(i-1) = y_ad(i-1) - dy_dx_low_ad/(x(i)-x(i-1))
    x_ad(i)   = x_ad(i)   - (dy_dx_low(i)/(x(i)-x(i-1)))*dy_dx_low_ad
    x_ad(i-1) = x_ad(i-1) + (dy_dx_low(i)/(x(i)-x(i-1)))*dy_dx_low_ad
    dy_dx_low_ad = 0.0_wp

    !dy_dx_up_tl  = (y_tl(i+1)-y_tl(i))/(x(i+1)-x(i)) - &
    !&(dy_dx_up/(x(i+1)-x(i)))*(x_tl(i+1)-x_tl(i))
 
    y_ad(i+1) = y_ad(i+1) + dy_dx_up_ad/(x(i+1)-x(i))
    y_ad(i)   = y_ad(i)   - dy_dx_up_ad/(x(i+1)-x(i))
    x_ad(i+1) = x_ad(i+1) - (dy_dx_up(i)/(x(i+1)-x(i)))*dy_dx_up_ad
    x_ad(i)   = x_ad(i)   + (dy_dx_up(i)/(x(i+1)-x(i)))*dy_dx_up_ad
    dy_dx_up_ad = 0.0_wp

    !y2_tl(i)=sig_tl/p - (y2(i)/p)*p_tl
  
    sig_ad = sig_ad + y2_ad(i)/p(i)
    p_ad = p_ad - (py_dum(i)/p(i))*y2_ad(i)
    y2_ad(i) = 0.0_wp

    !p_tl = sig_tl*y2(i-1) + sig*y2_tl(i-1)

    sig_ad = sig_ad + py_dum(i-1)*p_ad
    y2_ad(i-1) = y2_ad(i-1) + sig(i)*p_ad
    p_ad = 0.0_wp

    !sig_tl = (x_tl(i)-x_tl(i-1))/(x(i+1)-x(i-1)) - &
    !& (sig/(x(i+1)-x(i-1)))*(x_tl(i+1)-x_tl(i-1))

    x_ad(i)   = x_ad(i)   + sig_ad/(x(i+1)-x(i-1))
    x_ad(i-1) = x_ad(i-1) - sig_ad/(x(i+1)-x(i-1))
    x_ad(i+1) = x_ad(i+1) - (sig(i)/(x(i+1)-x(i-1)))*sig_ad
    x_ad(i-1) = x_ad(i-1) + (sig(i)/(x(i+1)-x(i-1)))*sig_ad
    sig_ad = 0.0_wp
 
  ENDDO ! i

  !u_tl(:) = 0.0_wp

  u_ad(:) = 0.0_wp
  
  !y2_tl(:,:) = 0.0_wp  

  y2_ad(:) = 0.0_wp  

END SUBROUTINE ropp_fm_spline_init_ad
