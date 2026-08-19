SUBROUTINE ropp_fm_splint(klev,knewlev,khoriz,zg,logn,y2, &
&                         zg_int,refrac_int)  
 
! NAME
!   ropp_fm_splint  
!   
! INTERFACE
!   ropp_fm_splint is called from ropp_fm_spline_log
!
! DESCRIPTION
!   Interpolate refractivity onto finer (vertical) grid
!   Does spline interpolation according to Numerical Recipes p.110
!
! INPUT
!   klev       =  number of initial vertical levels
!   knewlev    =  number of interpolated vertical levels
!   khoriz     =  no. of horizontal locations
!   y2         =  second derivative of data calculated in ropp_fm_spline_init
!   logn       =  logarithm of data to be interpolated (y, e.g. refractivity, bending angle,...)
!   zg         =  geopential height on initial vertical levels
!
! OUTPUT
!   zg_int     =  geopotential height on interpolated vertical levels
!   refrac_int =  refractivity on interpolated vertical levels
!
! MODIFICATIONS
!
!     Sean Healy      ECMWF               2018/06/26 : Original
!     K. Lonitz       ECMWF               2023/10/25 : input into ropp
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
  REAL(wp)   ,INTENT(IN)    :: logn(klev,khoriz)
  REAL(wp)   ,INTENT(IN)    :: y2(klev,khoriz)

  REAL(wp)   ,INTENT(IN)    :: zg_int(knewlev) 

  REAL(wp)   ,INTENT(OUT)   :: refrac_int(knewlev,khoriz) 

! local variables

  INTEGER  :: i,j,ipos
  REAL(wp) :: a,b,h, ref_int         ! for Spline

! calculate the geometric heights

  refrac_int(:,:) = ropp_MDFV 
 
  DO j = 1,khoriz 
    
    DO i= 1,knewlev
      
       refrac_int(i,j) = ropp_MDFV 

       IF ((zg_int(i) - zg(1,j)) < -1.0_wp) CYCLE ! test for below the surface at j   

       ipos = 1

! find location in vertical
     
       DO 

         IF (zg(ipos+1,j) > zg_int(i) .OR. ipos+1 == klev) EXIT
           
         ipos=ipos+1

       ENDDO  
          
       ipos = MIN(MAX(1,ipos),klev-1)
     
       h = zg(ipos+1,j) - zg(ipos,j)
     
       a = (zg(ipos+1,j) - zg_int(i))/h
     
       b = 1.0_wp - a

! interpolated Log(refrac)
     
       ref_int = a*logn(ipos,j)+b*logn(ipos+1,j) + &
      &((a**3-a)*y2(ipos,j)+(b**3-b)*y2(ipos+1,j))*(h**2)/6.0_wp

! interpolated refractivity
        
       refrac_int(i,j) = EXP(ref_int)
    ENDDO ! i 

  ENDDO ! j
  

END SUBROUTINE ropp_fm_splint
