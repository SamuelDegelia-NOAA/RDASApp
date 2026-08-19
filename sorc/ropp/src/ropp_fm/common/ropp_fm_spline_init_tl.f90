SUBROUTINE ropp_fm_spline_init_tl(klev,x,x_tl,y,y_tl,y2,y2_tl)  
 

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
!     K. Lonitz       ECMWF               2023/02/06 : input into ropp

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE


  INTEGER,INTENT(IN)        :: klev    ! no. of model levels in state vec.
  REAL(wp)   ,INTENT(IN)    :: x(klev) 
  REAL(wp)   ,INTENT(IN)    :: x_tl(klev)
  REAL(wp)   ,INTENT(IN)    :: y(klev) 
  REAL(wp)   ,INTENT(IN)    :: y_tl(klev) 
  REAL(wp)   ,INTENT(OUT)   :: y2(klev)
  REAL(wp)   ,INTENT(OUT)   :: y2_tl(klev) 

! local variables

  INTEGER  :: i
  REAL(wp) :: u(klev), u_tl(klev)          
  REAL(wp) :: sig,sig_tl
  REAL(wp) :: p,p_tl
  REAL(wp) :: dy_dx_up,dy_dx_low 
  REAL(wp) :: dy_dx_up_tl,dy_dx_low_tl 
  REAL(wp) :: py_dum
  REAL(wp) :: dum, dum_tl   


  y2(:)    = 0.0_wp
  y2_tl(:) = 0.0_wp
  u(:)     = 0.0_wp
  u_tl(:)  = 0.0_wp


  DO i = 2,klev-1

     sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))

     sig_tl = (x_tl(i)-x_tl(i-1))/(x(i+1)-x(i-1)) - &
 & (sig/(x(i+1)-x(i-1)))*(x_tl(i+1)-x_tl(i-1))

     p=sig*y2(i-1)+2.0_wp

     p_tl = sig_tl*y2(i-1) + sig*y2_tl(i-1)

     y2(i)=(sig-1.0_wp)/p
      
     y2_tl(i)=sig_tl/p - (y2(i)/p)*p_tl

     dy_dx_up  = (y(i+1)-y(i))/(x(i+1)-x(i))
        
     dy_dx_up_tl  = (y_tl(i+1)-y_tl(i))/(x(i+1)-x(i)) - &
    &(dy_dx_up/(x(i+1)-x(i)))*(x_tl(i+1)-x_tl(i))
 
     dy_dx_low = (y(i)-y(i-1))/(x(i)-x(i-1))
   
     dy_dx_low_tl = (y_tl(i)-y_tl(i-1))/(x(i)-x(i-1)) - &
    &(dy_dx_low/(x(i)-x(i-1)))*(x_tl(i)-x_tl(i-1))

     dum = 6.0_wp*(dy_dx_up - dy_dx_low)/(x(i+1)-x(i-1))
  
     dum_tl = 6.0_wp*(dy_dx_up_tl - dy_dx_low_tl)/(x(i+1)-x(i-1)) - &
     &(dum/(x(i+1)-x(i-1)))*(x_tl(i+1)-x_tl(i-1))


     u(i)=(dum - sig*u(i-1))/p
      
     u_tl(i) = (dum_tl - (sig_tl*u(i-1)+sig*u_tl(i-1)))/p - &
     &(u(i)/p)*p_tl

  
  ENDDO ! I

  y2(klev)    = 0.0_wp  ! for clarity
  y2_tl(klev) = 0.0_wp  

  DO i = klev-1,1,-1

   py_dum = y2(i)
   
   y2(i) = py_dum*y2(i+1) + u(i)
      
   y2_tl(i) = py_dum*y2_tl(i+1)  + y2(i+1)*y2_tl(i) + u_tl(i)

   
  ENDDO

END SUBROUTINE ropp_fm_spline_init_tl
