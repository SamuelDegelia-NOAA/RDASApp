! $Id$

SUBROUTINE ropp_fm_abel(nr, refrac, temp, roc, Tgrad_oper, impact, bangle)

!****s* BendingAngle/ropp_fm_abel *
!
! NAME
!    ropp_fm_abel - Forward model calculating a one dimensional bending angle
!                   profile from refractivity / impact parameter profile at
!                   state vector levels using a Fast Abel Transform
!
! SYNOPSIS
!    call ropp_fm_abel(nr, refrac, temp, roc, Tgrad_oper, impact, bangle)
! 
! DESCRIPTION
!    This routine calculates bending angles at a given set of impact parameters
!    from a vertical profile of rafractivity given at the state vector's set of
!    x = nr levels. 
!
! INPUTS
!    real(wp), dimension(:) :: nr          ! x = nr product
!    real(wp), dimension(:) :: refrac      ! Refractivity values
!    real(wp), dimension(:) :: temp        ! temperature values
!    real(wp),              :: roc         ! radius of curvature
!    LOGICAL                :: Tgrad_oper  ! Use temp. gradient oper.
!    real(wp), dimension(:) :: impact      ! Impact parameters
!
! OUTPUT
!    real(wp), dimension(:) :: bangle      ! Forward modelled bending angles
!
! NOTES
!    The interpolation of bending angles calculated at the state vector's
!    geopotential levels to the observation vector's impact parameters is
!    carried out assuming that bending angle varies exponentially with
!    impact parameter.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_bangle_1d
!    ropp_fm_abel_ad
!    ropp_fm_abel_tl
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils, ONLY: ropp_MDFV, ropp_ZERO, ropp_ZDTV
  USE ropp_fm_constants, ONLY: pi, imp_ht_min

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)  :: nr             ! x = nr product
  REAL(wp), DIMENSION(:), INTENT(in)  :: refrac         ! Refractivity
  REAL(wp), DIMENSION(:), INTENT(in)  :: temp           ! Temperature
  REAL(wp)              , INTENT(in)  :: roc            ! Radius of curvature
  LOGICAL               , INTENT(in)  :: Tgrad_oper     ! Use temp. gradient oper.

  REAL(wp), DIMENSION(:), INTENT(in)  :: impact         ! Impact parameter
  REAL(wp), DIMENSION(:), INTENT(out) :: bangle         ! Bending angle

  REAL(wp), DIMENSION(:), ALLOCATABLE :: kval           ! Exponential decay rate
  REAL(wp), DIMENSION(:), ALLOCATABLE :: beta           ! Temperature gradient
  REAL(wp), DIMENSION(:), ALLOCATABLE :: nr_mid         ! Average nr product
  REAL(wp), DIMENSION(:), ALLOCATABLE :: temp_mid       ! Average temp. of two levels
  REAL(wp), DIMENSION(:), ALLOCATABLE :: dval           ! Useful in BA computation
  
  REAL(wp)                            :: t_upper        ! Upper bound integral
  REAL(wp)                            :: t_lower        ! Lower bound integral
  REAL(wp)                            :: refrac_low     ! Refrac at lower level
  REAL(wp)                            :: nr_low         ! x=nr at lower level
  REAL(wp)                            :: zed            ! Impact height
  

  REAL(wp)                            :: integral_diff  ! Integral approximation
  REAL(wp)                            :: erf_up, erf_low
  REAL(wp)                            :: int_up, int_low
  REAL(wp)                            :: zt
  REAL(wp)                            :: dn_dx,p1,p2,p3
  
  REAL(wp), PARAMETER                 :: a = 0.3480242_wp
  REAL(wp), PARAMETER                 :: b = 0.0958798_wp
  REAL(wp), PARAMETER                 :: c = 0.7478556_wp

  INTEGER                             :: n_lev, n_lower, n_impact
  INTEGER                             :: i, i_bot, l

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  n_lev = SIZE(nr)
  n_impact = SIZE(impact)
  
  ALLOCATE(kval(n_lev-1))
  ALLOCATE(beta(n_lev-1))  
  ALLOCATE(nr_mid(n_lev-1))
  ALLOCATE(temp_mid(n_lev-1))
  ALLOCATE(dval(n_lev-1))
 
!-------------------------------------------------------------------------------
! 3. Calculate lowest usable level (because of superrefraction)
!-------------------------------------------------------------------------------

  n_lower = 1
  DO i = n_lev, 2, -1
     IF (nr(i) - nr(i-1) < 10.0_wp) THEN
        n_lower = i
        EXIT
     ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! 4. Calculate exponential decay rate and temp. gradient between levels
!-------------------------------------------------------------------------------

  DO i = 1, n_lev - 1

     kval(i) = LOG(refrac(i)/refrac(i+1)) / MAX(1.0_wp, (nr(i+1)-nr(i)))
     kval(i) = MAX(1.0e-6_wp, kval(i))

! limit the maximum kval so that refractivity gradient is ~ half critical value    
     
     kval(i) = MIN(kval(i),0.157_wp/refrac(i))
     
! beta is the temperature gradient
        
     beta(i) = (temp(i+1) - temp(i)) / MAX(1.0_wp, (nr(i+1)-nr(i)))

! mean nr

     nr_mid(i)   = 0.5_wp*(nr(i) + nr(i+1))

! mean temp.

     temp_mid(i) = 0.5_wp*(temp(i) + temp(i+1))

     dval(i) = (nr(i) -nr_mid(i))**2

  ENDDO

!-------------------------------------------------------------------------------
! 5. Calculate bending angles for observational heights
!-------------------------------------------------------------------------------

  bangle(:) = ropp_MDFV

  DO l = 1, n_impact

     IF (impact(l) < nr(n_lower) .OR. impact(l) >= nr(n_lev)) THEN
        CYCLE
     ENDIF

!    5.1 Find bottom state vector level
!    ----------------------------------

     i_bot = n_lower
     DO WHILE (impact(l) >= nr(i_bot + 1) .AND. i_bot < n_lev)
        i_bot = i_bot + 1
     ENDDO

!    5.2 Loop over all levels above
!    ------------------------------

     bangle(l) = ropp_ZERO
     

     DO i = i_bot, n_lev - 1

!       5.2.1 Values of refractivity and impact parameter at lower level
!       ----------------------------------------------------------------

        refrac_low = refrac(i)
        nr_low = nr(i) 


        IF (refrac(i+1)-refrac(i) > - ropp_ZDTV ) THEN

!       5.2.2 If the refractivity gradient is +ve with height
!       -------------------------------------------------------------------

! This will handle the cases where the refractivity goes up with height

           dn_dx = (refrac(i+1)-refrac(i))/(nr(i+1)-nr(i))

           t_upper = SQRT( nr(i+1)-impact(l))

           t_lower = 0.0_wp

           IF (i > i_bot)  t_lower = SQRT( nr(i)-impact(l))

           bangle(l) = bangle(l) - &
             2.0E-6_wp*SQRT(2.0_wp*impact(l))* dn_dx*(t_upper-t_lower) 

        ELSE

!       5.2.3 Upper and lower bounds of the "normal" integral
!       -------------------------------------------------------------------

           t_upper = SQRT(kval(i) * (nr(i+1) - impact(l)))

           IF (i == i_bot) THEN
              t_lower = 0.0_wp
           ELSE
              t_lower = SQRT(kval(i) * (nr(i) - impact(l)))
           ENDIF

!       5.2.4 Error functions
!       --------------

        ! Approximate error function with polynomial

           zt = 1.0_wp / (1.0_wp + 0.47047_wp * t_lower)
           erf_low = 1.0_wp - (a-(b-c*zt)*zt) * zt * EXP(-(t_lower*t_lower))
           zt = 1.0_wp / (1.0_wp + 0.47047_wp*t_upper)
           erf_up = 1.0_wp - (a-(b-c*zt)*zt) * zt * EXP(-(t_upper*t_upper))

           IF (i == n_lev-1) erf_up = 1.0_wp   ! limit at infinity


!       5.2.5 New terms for integral that now allows kval to vary within layer
!       ------------------------------------------------------------------
        
         ! These p1, p2,p3 values correspond the case where dT/dx = beta = 0, i.e kval is constant!

           p1 = kval(i)
           p2 = 0.0_wp
           p3 = 0.0_wp

         ! impact height of level

           zed = nr(i) - roc

           IF ( i < n_lev-1 .AND. zed > imp_ht_min .AND. Tgrad_oper) THEN

         ! compute the "p" values for temp. gradient beta

             p1 = kval(i)*(1.0_wp + beta(i)/temp_mid(i)* &
               (0.5_wp*kval(i)*((impact(l)-nr_mid(i))**2 - dval(i)) - &
               (impact(l)-nr_mid(i))))

             p2 = kval(i)*beta(i)/temp_mid(i)* &
               (kval(i)*(impact(l)-nr_mid(i))-1.0_wp)

             p3 = 0.5_wp*kval(i)**2*beta(i)/temp_mid(i)

           ENDIF

           int_up = SQRT(pi/kval(i))* &
            (p1 + 0.5_wp/kval(i)*(p2+1.5_wp*p3/kval(i)))*erf_up

           int_up = int_up - EXP(-kval(i)*(nr(i+1)-impact(l)))* &
             SQRT(nr(i+1)-impact(l))/kval(i)*(p2 + p3* ( &
             (nr(i+1)-impact(l))+1.5_wp/kval(i)))

         ! lower limit of integral

           int_low = 0.0_wp

           IF (i > i_bot) THEN

              int_low = SQRT(pi/kval(i))* &
               (p1 + 0.5_wp/kval(i)*(p2+ 1.5_wp*p3/kval(i)))*erf_low

              int_low = int_low - EXP(-kval(i)*(nr(i)-impact(l)))* &
               SQRT(nr(i)-impact(l))/kval(i)*(p2 + p3* ( &
               (nr(i)-impact(l))+1.5_wp/kval(i)))

           ENDIF

        integral_diff = int_up - int_low

!       5.2.6 Bending angle value
!       -------------------------

           bangle(l) = bangle(l) &
                     + 1.0e-6_wp * SQRT(2.0_wp*impact(l)) &
                     * refrac_low * EXP(kval(i) * (nr_low - impact(l))) &
                     * integral_diff

        ENDIF

     ENDDO

  ENDDO

  DEALLOCATE(beta)
  DEALLOCATE(nr_mid)
  DEALLOCATE(temp_mid)
  DEALLOCATE(dval)
  DEALLOCATE(kval)

END SUBROUTINE ropp_fm_abel
