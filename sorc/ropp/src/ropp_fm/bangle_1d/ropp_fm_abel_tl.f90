! $Id$

SUBROUTINE ropp_fm_abel_tl(nr, refrac, temp, temp_tl, roc, Tgrad_oper, impact, nr_tl, refrac_tl, bangle_tl)

!****s* BendingAngle/ropp_fm_abel_tl *
!
! NAME
!    ropp_fm_abel_tl - Tangent linear of ropp_fm_abel().
!
! SYNOPSIS
!    call ropp_fm_abel_tl(nr, refrac, impact, nr_tl, refrac_tl, bangle_tl)
! 
! DESCRIPTION
!    This routine is the tangent linear of ropp_fm_abel.
!
! INPUTS
!    real(wp), dimension(:) :: nr          ! x=nr product of state levels
!    real(wp), dimension(:) :: refrac      ! Refractivity values
!    real(wp), dimension(:) :: temp        ! Temperature values
!    real(wp), dimension(:) :: temp_tl     ! Temperature values pertns
!    real(wp),              :: roc         ! Radius of curvature
!    LOGICAL                :: Tgrad_oper  ! Use temp. gradient oper.
!    real(wp), dimension(:) :: impact      ! Observation's impact parameters
!    real(wp), dimension(:) :: nr_tl       ! x=nr perturbations
!    real(wp), dimension(:) :: refrac_tl   ! Refractivity perturbations
!
! OUTPUT
!    real(wp), dimension(:) :: bangle_tl   ! Bending angle perturbations
!
! NOTES
!    The lengths of the arrays nr, nr_tl, refrac and refrac_tl must be
!    equal, as must the lengths of impact and bangle_tl.
!
! SEE ALSO
!    ropp_fm_types
!    rpp_fm_bangle_1d
!    ropp_fm_abel
!    ropp_fm_abel_ad
!
! REFERENCES
!    This routine was created with the help of the 
!
!      Tangent linear and Adjoint Model Compiler,  TAMC 5.3.2.
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
  USE ropp_utils, ONLY: ropp_ZDTV
  USE ropp_fm_constants, ONLY: pi, imp_ht_min

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)    :: nr             ! x=nr product
  REAL(wp), DIMENSION(:), INTENT(in)    :: refrac         ! Refractivity
  REAL(wp), DIMENSION(:), INTENT(in)    :: impact         ! Impact parameter
  REAL(wp), DIMENSION(:), INTENT(in)    :: temp           ! Temperature
  REAL(wp), DIMENSION(:), INTENT(in)    :: temp_tl        ! Temperature
  REAL(wp)              , INTENT(in)    :: roc            ! Radius of curvature
  LOGICAL               , INTENT(in)    :: Tgrad_oper     ! Use temp. gradient oper.
   
  REAL(wp), DIMENSION(:), INTENT(inout) :: nr_tl          ! x=nr perturbation
  REAL(wp), DIMENSION(:), INTENT(inout) :: refrac_tl      ! Refrac perturbation
  REAL(wp), DIMENSION(:), INTENT(inout) :: bangle_tl      ! Bangle perturbation
 
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval           ! Exponential decay rate
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval_tl
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: beta           ! Temperature gradient
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: beta_tl
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: nr_mid         ! Average nr product
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: nr_mid_tl
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: temp_mid       ! Average temp. of two levels
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: temp_mid_tl
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: dval           ! Useful in BA computation
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: dval_tl

  REAL(wp)                              :: t_lower        ! Lower bound of integral
  REAL(wp)                              :: t_lower_tl
  REAL(wp)                              :: t_upper        ! Upper bound of integral
  REAL(wp)                              :: t_upper_tl 
  REAL(wp)                              :: refrac_low     ! Refrac at lower level
  REAL(wp)                              :: refrac_low_tl
  REAL(wp)                              :: nr_low         ! x=nr at lower level
  REAL(wp)                              :: nr_low_tl
  REAL(wp)                              :: factor
  REAL(wp)                              :: zed            ! Impact height

  REAL(wp)                              :: integral_diff  ! Integral approximation
  REAL(wp)                              :: integral_diff_tl
  REAL(wp)                              :: erf_up
  REAL(wp)                              :: erf_low
  REAL(wp)                              :: zt_up
  REAL(wp)                              :: zt_low
  REAL(wp)                              :: erf_up_tl
  REAL(wp)                              :: erf_low_tl
  REAL(wp)                              :: zt_up_tl
  REAL(wp)                              :: zt_low_tl

  REAL(wp)                              :: uval,part1,part2,part3
  REAL(wp)                              :: uval_tl,part1_tl,part2_tl,part3_tl
   
  REAL(wp)                              :: int_up, int_low
  REAL(wp)                              :: int_up_tl, int_low_tl
  
  REAL(wp)                              :: dn_dx,p1,p2,p3
  REAL(wp)                              :: dn_dx_tl,p1_tl,p2_tl,p3_tl
  
  REAL(wp), PARAMETER                   :: a=0.3480242_wp
  REAL(wp), PARAMETER                   :: b=0.0958798_wp
  REAL(wp), PARAMETER                   :: c=0.7478556_wp

  INTEGER                               :: n_lev, n_lower, n_impact
  INTEGER                               :: i, i_bot, l

!-------------------------------------------------------------------------------
! 2. Useful variables
!-------------------------------------------------------------------------------

  n_lev = SIZE(nr)
  n_impact = SIZE(impact)

  ALLOCATE(kval(n_lev-1))
  ALLOCATE(kval_tl(n_lev-1))
  
  ALLOCATE(beta(n_lev-1))
  ALLOCATE(beta_tl(n_lev-1))
  
  ALLOCATE(nr_mid(n_lev-1))
  ALLOCATE(nr_mid_tl(n_lev-1))
  
  ALLOCATE(temp_mid(n_lev-1))
  ALLOCATE(temp_mid_tl(n_lev-1))

  ALLOCATE(dval(n_lev-1))
  ALLOCATE(dval_tl(n_lev-1))

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
! 4. Calculate exponential decay rate between levels
!-------------------------------------------------------------------------------

  DO i = 1, n_lev - 1

     factor = 1.0_wp / MAX(1.0_wp, nr(i+1) - nr(i))

     kval(i)    = LOG(refrac(i)/refrac(i+1)) / MAX(1.0_wp, nr(i+1) - nr(i))
     kval_tl(i) = - nr_tl(i+1) &
                       * (0.5_wp - SIGN(0.5_wp, 1.0_wp - (nr(i+1) - nr(i)))) &
                                   * kval(i) * factor                        &
                  + nr_tl(i) &
                    * (0.5_wp - SIGN(0.5_wp, 1.0_wp - (nr(i+1) - nr(i)))) &
                                * kval(i) * factor      &
                  - refrac_tl(i+1) * factor / refrac(i+1)   &
                  + refrac_tl(i) * factor / refrac(i)

     kval(i)    = MAX(1.0e-6_wp, kval(i))
     kval_tl(i) = kval_tl(i) * (0.5_wp - SIGN(0.5_wp, 1.0e-6_wp - kval(i)))

! limit the size if the refractivity gradient

     IF (kval(i) > 0.157_wp/refrac(i)) THEN

         kval(i) = 0.157_wp/refrac(i)

         kval_tl(i) = - 0.157_wp/refrac(i)**2*refrac_tl(i)

     ENDIF

! beta is the temperature gradient

     beta(i) = (temp(i+1) - temp(i))/MAX(1.0_wp,(nr(i+1)-nr(i)))

     IF ((nr(i+1)-nr(i)) > 1.0_wp) THEN

        beta_tl(i) = &
         (temp_tl(i+1) - temp_tl(i))/(nr(i+1) - nr(i)) - &
         (nr_tl(i+1) - nr_tl(i))*beta(i)/(nr(i+1) - nr(i))

     ELSE

        beta_tl(i) =temp_tl(i+1) - temp_tl(i) 

     ENDIF

! mean nr

     nr_mid(i)   = 0.5_wp*(nr(i) + nr(i+1))
     nr_mid_tl(i) = 0.5_wp*(nr_tl(i) + nr_tl(i+1))

! mean temp.

     temp_mid(i) = 0.5_wp*(temp(i) + temp(i+1))
     temp_mid_tl(i) = 0.5_wp*(temp_tl(i) + temp_tl(i+1))

! useful for integral

     dval(i) = (nr(i) -nr_mid(i))**2
     dval_tl(i) = 2.0_wp*(nr(i) -nr_mid(i))*(nr_tl(i) -nr_mid_tl(i))

  ENDDO

!-------------------------------------------------------------------------------
! 5. Calculate bending angles for observational heights
!-------------------------------------------------------------------------------

  bangle_tl(:) = 0.0_wp

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

     bangle_tl(l) = 0.0_wp

     DO i = i_bot, n_lev - 1
        
!       5.2.1 Values of refractivity and impact parameter at lower level
!       ----------------------------------------------------------------

        refrac_low    = refrac(i) 
        refrac_low_tl = refrac_tl(i)
        nr_low    = nr(i) 
        nr_low_tl = nr_tl(i)

        IF (refrac(i+1)-refrac(i) > - ropp_ZDTV ) THEN

!       5.2.2 If the refractivity gradient is +ve with height
!       -------------------------------------------------------------------

! This will handle the cases where the refractivity goes up with height

           dn_dx = (refrac(i+1)-refrac(i))/(nr(i+1)-nr(i))

           dn_dx_tl = &
             (refrac_tl(i+1)-refrac_tl(i))/(nr(i+1)-nr(i)) - &
             dn_dx/(nr(i+1)-nr(i))*(nr_tl(i+1)-nr_tl(i))

           t_upper = SQRT( nr(i+1)-impact(l))
           t_upper_tl = 0.5_wp/SQRT(nr(i+1)-impact(l))*nr_tl(i+1)

           t_lower = 0.0_wp
           t_lower_tl = 0.0_wp

           IF (i > i_bot) THEN

              t_lower = SQRT(nr(i)-impact(l))
              t_lower_tl = 0.5_wp/SQRT(nr(i)-impact(l))*nr_tl(i)

           ENDIF

!!           bangle(l) = bangle(l) - &
!!           2.0E-6_wp*SQRT(2.0_wp*impact(l))* dn_dx*(t_upper-t_lower)

           bangle_tl(l) = bangle_tl(l) - &
             2.0e-6_wp*SQRT(2.0_wp*impact(l))* &
             (dn_dx_tl*(t_upper-t_lower) + dn_dx*(t_upper_tl-t_lower_tl))

        ELSE

!       5.2.3 Upper and lower bounds of the integral
!       
!-------------------------------------------------------------------

        t_upper    = SQRT(kval(i) * (nr(i+1) - impact(l)))

        t_upper_tl = nr_tl(i+1) * 0.5_wp * kval(i) / t_upper  &
                   + kval_tl(i) * 0.5_wp * (nr(i+1) - impact(l)) / t_upper

        IF (i == i_bot) THEN
           t_lower    = 0.0_wp
           t_lower_tl = 0.0_wp
        ELSE
           t_lower    = SQRT(kval(i) * (nr(i) - impact(l)))
           t_lower_tl = nr_tl(i) * 0.5_wp * kval(i) / t_lower   &
                      + kval_tl(i) * 0.5_wp * (nr(i) - impact(l)) / t_lower
        ENDIF

!       5.2.4 Error function
!       --------------------

        ! Approximate error function with polynomial
        
        zt_low = 1.0_wp / (1.0_wp+0.47047_wp*t_lower)
        zt_low_tl = -(0.47047_wp*t_lower_tl) * (zt_low*zt_low)

        erf_low = 1.0_wp - (a-(b-c*zt_low)*zt_low) * zt_low *     &
                    EXP(-(t_lower*t_lower))
        erf_low_tl = ((a-(b-c*zt_low)*zt_low)*zt_low*2.0_wp*t_lower*t_lower_tl &
                     -(a-(2.0_wp*b-3.0_wp*c*zt_low)*zt_low)*zt_low_tl)         &
                     * EXP(-(t_lower*t_lower))

        zt_up = 1.0_wp / (1.0_wp + 0.47047_wp * t_upper)
        zt_up_tl = -(0.47047_wp * t_upper_tl) * (zt_up*zt_up)

        erf_up = 1.0_wp - (a-(b-c*zt_up)*zt_up) * zt_up *         &
                   EXP(-(t_upper*t_upper))
        erf_up_tl = ((a-(b-c*zt_up)*zt_up)*zt_up*2.0_wp*t_upper*t_upper_tl &
                    -(a-(2.0_wp*b-3.0_wp*c*zt_up)*zt_up)*zt_up_tl)         &
                    * EXP(-(t_upper*t_upper))

        IF (i == n_lev-1) THEN

        ! upper most level erf_up = 1.0

            erf_up = 1.0_wp
            erf_up_tl = 0.0_wp

        ENDIF


!       5.2.5 New terms for integral that now allows kval to vary within layer
!       ------------------------------------------------------------------

         ! these p1, p2,p3 values correspond the case where dT/dx = beta = 0, i.e kval is constant!

           p1 = kval(i)
           p2 = 0.0_wp
           p3 = 0.0_wp

           p1_tl = kval_tl(i)
           p2_tl = 0.0_wp
           p3_tl = 0.0_wp

         ! impact height of level

           zed = nr(i) - roc

           IF ( i < n_lev-1 .AND. zed > imp_ht_min .AND. Tgrad_oper) THEN

         ! compute the "p" values for temp. gradient beta

             p1 = kval(i)*(1.0_wp + beta(i)/temp_mid(i)* &
               (0.5_wp*kval(i)*((impact(l)-nr_mid(i))**2 - dval(i)) - &
               (impact(l)-nr_mid(i))))

         ! split the TL code for clarity

             uval =  beta(i)/temp_mid(i)
             uval_tl = beta_tl(i)/temp_mid(i)-uval/temp_mid(i)*temp_mid_tl(i)

             part1 = uval
             part1_tl =uval_tl

             part2 = 0.5_wp*kval(i)*((impact(l)-nr_mid(i))**2 - dval(i)) - &
                     (impact(l)-nr_mid(i))

             part2_tl = 0.5_wp*((impact(l)-nr_mid(i))**2 - dval(i))*kval_tl(i) - &
               0.5_wp*kval(i)*(2.0_wp*(impact(l)-nr_mid(i))*nr_mid_tl(i) + & 
               dval_tl(i)) + nr_mid_tl(i)

             p1_tl = kval_tl(i)* p1/kval(i) + & 
                     kval(i)*(part1*part2_tl + part1_tl*part2)

             p2 = kval(i)*beta(i)/temp_mid(i)* &
               (kval(i)*(impact(l)-nr_mid(i))-1.0_wp)

             part1 = kval(i)*uval 
             part1_tl = uval*kval_tl(i)+ kval(i)*uval_tl

             part2 = kval(i)*(impact(l)-nr_mid(i))-1.0_wp
             part2_tl =(impact(l)-nr_mid(i))*kval_tl(i) - kval(i)*nr_mid_tl(i)

             p2_tl =part1*part2_tl + part2*part1_tl 

             p3 = 0.5_wp*kval(i)**2*beta(i)/temp_mid(i)
             p3_tl = 0.5_wp*kval(i)**2*uval_tl + uval*kval(i)*kval_tl(i)

           ENDIF

           int_up = SQRT(pi/kval(i))* &
             (p1 + 0.5_wp/kval(i)*(p2+1.5_wp*p3/kval(i)))*erf_up

           part1 = SQRT(pi/kval(i))
           part1_tl = - 0.5_wp*part1/kval(i)*kval_tl(i)

           part2 = p1 + 0.5_wp/kval(i)*(p2+1.5_wp*p3/kval(i))

           part2_tl = &
           p1_tl -0.5_wp/kval(i)**2*(p2+1.5_wp*p3/kval(i))*kval_tl(i) + &
             0.5_wp/kval(i)*(p2_tl + 1.5_wp/kval(i)*(p3_tl - p3/kval(i)*kval_tl(i)))

           part3 = erf_up
           part3_tl = erf_up_tl

           int_up_tl = part2*part3*part1_tl + &
                       part1*part3*part2_tl + &
                       part1*part2*part3_tl

           int_up = int_up - EXP(-kval(i)*(nr(i+1)-impact(l)))* &
             SQRT(nr(i+1)-impact(l))/kval(i)*(p2 + p3* ( &
             (nr(i+1)-impact(l))+1.5_wp/kval(i)))

         ! second part of int_up

           part1 = EXP(-kval(i)*(nr(i+1)-impact(l)))
           part1_tl = -part1*(kval_tl(i)*(nr(i+1)-impact(l))+kval(i)*nr_tl(i+1))

           part2 = SQRT(nr(i+1)-impact(l))/kval(i)
           part2_tl = -part2/kval(i)*kval_tl(i)+0.5_wp*part2/(nr(i+1)-impact(l))*nr_tl(i+1)

           part3 = p2 + p3* (nr(i+1)-impact(l)+1.5_wp/kval(i))
           part3_tl = p2_tl + p3_tl*(nr(i+1)-impact(l)+1.5_wp/kval(i)) + &
             p3*(nr_tl(i+1) -1.5_wp/kval(i)**2*kval_tl(i))

           int_up_tl = int_up_tl - part2*part3*part1_tl - &
                                   part1*part3*part2_tl - &
                                   part1*part2*part3_tl 

         ! lower limit of integral

           int_low = 0.0_wp
           int_low_tl = 0.0_wp

           IF (i > i_bot) THEN

              int_low = SQRT(pi/kval(i))* &
                (p1 + 0.5_wp/kval(i)*(p2+ 1.5_wp*p3/kval(i)))*erf_low

              part1 = SQRT(pi/kval(i))

              part1_tl = - 0.5_wp*part1/kval(i)*kval_tl(i)

              part2 = p1 + 0.5_wp/kval(i)*(p2+1.5_wp*p3/kval(i))

              part2_tl = p1_tl - &
                0.5_wp/kval(i)**2*(p2+1.5_wp*p3/kval(i))*kval_tl(i) + &
                0.5_wp/kval(i)*(p2_tl + 1.5_wp/kval(i)*(p3_tl - p3/kval(i)*kval_tl(i)))

              part3 = erf_low
              part3_tl = erf_low_tl

              int_low_tl = part2*part3*part1_tl + &
                           part1*part3*part2_tl + &
                           part1*part2*part3_tl

              int_low = int_low - EXP(-kval(i)*(nr(i)-impact(l)))* &
                SQRT(nr(i)-impact(l))/kval(i)*(p2 + p3* ( &
                (nr(i)-impact(l))+1.5_wp/kval(i)))

         ! split into parts

              part1 = EXP(-kval(i)*(nr(i)-impact(l)))
              part1_tl = -part1*(kval_tl(i)*(nr(i)-impact(l))+kval(i)*nr_tl(i))

              part2 = SQRT(nr(i)-impact(l))/kval(i)
              part2_tl = -part2/kval(i)*kval_tl(i)+0.5_wp*part2/(nr(i)-impact(l))*nr_tl(i)

              part3 = p2 + p3* (nr(i)-impact(l)+1.5_wp/kval(i))
              part3_tl = p2_tl + p3_tl*(nr(i)-impact(l)+1.5_wp/kval(i)) + &
                p3*(nr_tl(i) -1.5_wp/kval(i)**2*kval_tl(i))

              int_low_tl =  int_low_tl - part2*part3*part1_tl - &
                                         part1*part3*part2_tl - &
                                         part1*part2*part3_tl

           ENDIF

           integral_diff = int_up - int_low

           integral_diff_tl = int_up_tl - int_low_tl

!       5.2.4 Bending angle value
!       -------------------------

           factor  = 1.0e-6_wp * SQRT(2.0_wp * impact(l)) &
                         * EXP(kval(i) * (nr_low - impact(l)))

           bangle_tl(l) = bangle_tl(l) &
                     + factor*integral_diff*refrac_low_tl &
                     + factor*refrac_low*integral_diff_tl &
                     + factor*refrac_low*integral_diff    &
                     *(kval_tl(i)*(nr_low - impact(l))    &
                     + kval(i)*nr_low_tl) 

        ENDIF

     ENDDO

  ENDDO

  DEALLOCATE(kval)
  DEALLOCATE(kval_tl)

  DEALLOCATE(beta)
  DEALLOCATE(beta_tl)
  
  DEALLOCATE(nr_mid)
  DEALLOCATE(nr_mid_tl)
  
  DEALLOCATE(temp_mid)
  DEALLOCATE(temp_mid_tl)

  DEALLOCATE(dval)
  DEALLOCATE(dval_tl)

END SUBROUTINE ropp_fm_abel_tl
