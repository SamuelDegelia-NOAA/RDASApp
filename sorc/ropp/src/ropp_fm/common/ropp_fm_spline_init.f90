SUBROUTINE ropp_fm_spline_init(klev,x,y,y2)  
 

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
!  It generates second derivatives y2 with lower and upper boundary conditions set to "natural" (u(n),u(1),ye(n),y2(1) = 0)
!
! INPUT
!   klev   =  number of initial vertical levels
!   y      =  data to be interpolated (lives on x)
!   x      =  coordinate values
!
! OUTPUT
!   y2         =  second derivative of data 
!
! MODIFICATIONS
!
!     Sean Healy      ECMWF               2018/06/26 : Original
!     K. Lonitz       ECMWF               2023/10/25 : input into ropp

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal

  IMPLICIT NONE


  INTEGER,INTENT(IN)        :: klev    
  REAL(wp)   ,INTENT(IN)    :: x(klev) 
  REAL(wp)   ,INTENT(IN)    :: y(klev)
  REAL(wp)   ,INTENT(OUT)   :: y2(klev) 


! local variables

  INTEGER  :: i
  REAL(wp) :: u(klev)          
  REAL(wp) :: sig,p


  y2(:) = 0.0_wp
  u(:)  = 0.0_wp


  DO i = 2,klev-1

     sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))

     p=sig*y2(i-1)+2.0_wp

     y2(i)=(sig-1.0_wp)/p
      
     u(i)=(6.0_wp*((y(i+1)-y(i))/&
    &(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/ &
    &(x(i+1)-x(i-1))-sig*u(i-1))/p
  
  ENDDO ! I

  y2(klev) = 0.0  ! for clarity

  DO i = klev-1,1,-1

   y2(i) = y2(i)*y2(i+1) + u(i)
   
  ENDDO

END SUBROUTINE ropp_fm_spline_init
