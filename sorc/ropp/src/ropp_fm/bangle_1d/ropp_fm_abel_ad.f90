! $Id$

SUBROUTINE ropp_fm_abel_ad(nr, refrac, temp, temp_ad, roc, Tgrad_oper, impact, nr_ad, refrac_ad, bangle_ad)

!****s* BendingAngle/ropp_fm_abel_ad *
!
! NAME
!    ropp_fm_abel_ad - Adjoint of ropp_fm_abel().
!
! SYNOPSIS
!    call ropp_fm_abel_ad(nr, refrac, impact, nr_ad, refrac_ad, bangle_ad)
! 
! DESCRIPTION
!    This routine is the adjoint of ropp_fm_abel.
!
! INPUTS
!    real(wp), dimension(:) :: nr          ! x=nr product of state vector
!    real(wp), dimension(:) :: refrac      ! Refractivity values
!    real(wp), dimension(:) :: impact      ! Observation's impact parameters
!    real(wp), dimension(:) :: temp        ! Temperature values
!    real(wp), dimension(:) :: temp_ad     ! Temperature values ads
!    real(wp),              :: roc         ! Radius of curvature
!    LOGICAL                :: Tgrad_oper  ! Use temp. gradient oper.
!    real(wp), dimension(:) :: nr_ad       ! x=nr adjoint
!    real(wp), dimension(:) :: bangle_ad   ! Adjoint forcing
!
! OUTPUT
!    real(wp), dimension(:) :: nr_ad       ! Updated x=nr adjoint
!    real(wp), dimension(:) :: refrac_ad   ! Refractivity adjoint
!    real(wp), dimension(:) :: bangle_ad   ! Bending angle adjoint
!
! NOTES
!    The lengths of the arrays nr, nr_ad, refrac and refrac_ad must be
!    equal.
!
! SEE ALSO
!    ropp_fm_types
!    rpp_fm_bangle_1d
!    ropp_fm_abel
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
  USE ropp_fm_constants, ONLY: pi, imp_ht_min
  USE ropp_utils, ONLY: ropp_ZDTV

  IMPLICIT NONE

  REAL(wp), DIMENSION(:), INTENT(in)    :: nr             ! x=nr product state
  REAL(wp), DIMENSION(:), INTENT(in)    :: refrac         ! Refractivity
  REAL(wp), DIMENSION(:), INTENT(in)    :: impact         ! Impact observations
  REAL(wp), DIMENSION(:), INTENT(in)    :: temp           ! Temperature
  REAL(wp), DIMENSION(:), INTENT(inout) :: temp_ad        ! Temperature adjoint
  REAL(wp)              , INTENT(in)    :: roc            ! Radius of curvature
  LOGICAL               , INTENT(in)    :: Tgrad_oper     ! Use temp. gradient oper.

  REAL(wp), DIMENSION(:), INTENT(inout) :: nr_ad          ! x=nr adjoint
  REAL(wp), DIMENSION(:), INTENT(inout) :: refrac_ad      ! Refractivity adjoint
  REAL(wp), DIMENSION(:), INTENT(inout) :: bangle_ad      ! Bending angle adjoint

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval           ! Exponential decay rate
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval_tmp
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: kval_ad

  REAL(wp), DIMENSION(:), ALLOCATABLE   :: beta           ! Temperature gradient
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: beta_ad
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: nr_mid         ! Average nr product
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: nr_mid_ad
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: temp_mid       ! Average temp. of two levels
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: temp_mid_ad
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: dval           ! Useful in BA computation
  REAL(wp), DIMENSION(:), ALLOCATABLE   :: dval_ad

  REAL(wp)                              :: t_lower        ! Lower bound integral
  REAL(wp)                              :: t_lower_ad 
  REAL(wp)                              :: t_upper        ! Upper bound integral
  REAL(wp)                              :: t_upper_ad  
  REAL(wp)                              :: refrac_low     ! Refractivity lower lvl
  REAL(wp)                              :: refrac_low_ad
  REAL(wp)                              :: nr_low         ! x=nr at lower level
  REAL(wp)                              :: nr_low_ad
  REAL(wp)                              :: factor
  REAL(wp)                              :: zed            ! Impact height

  REAL(wp)                              :: integral_diff  ! Integral approx
  REAL(wp)                              :: integral_diff_ad
  REAL(wp)                              :: erf_up
  REAL(wp)                              :: erf_low
  REAL(wp)                              :: zt_up
  REAL(wp)                              :: zt_low
  REAL(wp), PARAMETER                   :: a=0.3480242_wp
  REAL(wp), PARAMETER                   :: b=0.0958798_wp
  REAL(wp), PARAMETER                   :: c=0.7478556_wp
  REAL(wp)                              :: erf_up_ad
  REAL(wp)                              :: erf_low_ad
  REAL(wp)                              :: zt_up_ad
  REAL(wp)                              :: zt_low_ad
  
  REAL(wp)                              :: uval,part1,part2,part3
  REAL(wp)                              :: uval_ad,part1_ad,part2_ad,part3_ad
   
  REAL(wp)                              :: int_up, int_low
  REAL(wp)                              :: int_up_ad, int_low_ad
  
  REAL(wp)                              :: dn_dx,p1,p2,p3
  REAL(wp)                              :: dn_dx_ad,p1_ad,p2_ad,p3_ad

  INTEGER                               :: n_lev, n_lower, n_impact
  INTEGER                               :: i, i_bot, l

!-------------------------------------------------------------------------------
! 2. Useful variables; also setting adjoint variables to zero
!-------------------------------------------------------------------------------

  n_lev = SIZE(nr)
  n_impact = SIZE(impact)

  ALLOCATE(kval(n_lev-1))
  ALLOCATE(kval_tmp(n_lev-1))
  ALLOCATE(kval_ad(n_lev-1))

  kval_ad(:)      = 0.0_wp

  ALLOCATE(beta(n_lev-1))
  ALLOCATE(beta_ad(n_lev-1))

  beta_ad(:) = 0.0_wp

  ALLOCATE(nr_mid(n_lev-1))
  ALLOCATE(nr_mid_ad(n_lev-1))

  nr_mid_ad(:) = 0.0_wp

  ALLOCATE(temp_mid(n_lev-1))
  ALLOCATE(temp_mid_ad(n_lev-1))

  temp_mid_ad(:) = 0.0_wp

  ALLOCATE(dval(n_lev-1))
  ALLOCATE(dval_ad(n_lev-1))

  dval_ad(:) = 0.0_wp

  integral_diff_ad = 0.0_wp

  nr_low_ad       = 0.0_wp
  refrac_low_ad   = 0.0_wp
  t_lower_ad      = 0.0_wp
  t_upper_ad      = 0.0_wp
  erf_low_ad      = 0.0_wp
  erf_up_ad       = 0.0_wp
  zt_up_ad        = 0.0_wp
  zt_low_ad       = 0.0_wp

! revised BA

  int_up_ad       = 0.0_wp
  int_low_ad      = 0.0_wp

  uval_ad         = 0.0_wp
  part1_ad        = 0.0_wp
  part2_ad        = 0.0_wp
  part3_ad        = 0.0_wp
  
  dn_dx_ad        = 0.0_wp
  p1_ad           = 0.0_wp
  p2_ad           = 0.0_wp
  p3_ad           = 0.0_wp

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

     kval(i) = LOG(refrac(i)/refrac(i+1))/MAX(1.0_wp, (nr(i+1)-nr(i)))
     kval(i) = MAX(1.0e-6_wp, kval(i))

! save for adjoint

     kval_tmp(i) = kval(i)

! limit the size if the refractivity gradient

     kval(i) = MIN(kval(i),0.157_wp/refrac(i))

! beta is the temperature gradient

     beta(i) = (temp(i+1) - temp(i))/MAX(1.0_wp,(nr(i+1)-nr(i)))

! mean nr

     nr_mid(i)   = 0.5_wp*(nr(i) + nr(i+1))

! mean temp.

     temp_mid(i) = 0.5_wp*(temp(i) + temp(i+1))

! useful for integral

     dval(i) = (nr(i) -nr_mid(i))**2

  ENDDO

!-------------------------------------------------------------------------------
! 5. Adjoint code - bending angle calculation
!-------------------------------------------------------------------------------

  DO l = n_impact, 1, -1

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

     DO i = n_lev - 1, i_bot, -1

        IF (refrac(i+1)-refrac(i) > - ropp_ZDTV ) THEN

           dn_dx = (refrac(i+1)-refrac(i))/(nr(i+1)-nr(i))

           t_upper = SQRT( nr(i+1)-impact(l))

           t_lower = 0.0_wp

           IF (i > i_bot) t_lower = SQRT( nr(i)-impact(l))

           dn_dx_ad = dn_dx_ad - 2.0e-6_wp*SQRT(2.0_wp*impact(l))* &
             (t_upper-t_lower)*bangle_ad(l) 

           t_upper_ad = t_upper_ad -2.0e-6_wp*SQRT(2.0_wp*impact(l))* & 
             dn_dx*bangle_ad(l)

           t_lower_ad = t_lower_ad +2.0e-6_wp*SQRT(2.0_wp*impact(l))* & 
             dn_dx*bangle_ad(l)

           bangle_ad(l) = bangle_ad(l)

           IF (i > i_bot) THEN

              nr_ad(i) = nr_ad(i) + 0.5_wp/SQRT(nr(i)-impact(l))*t_lower_ad
              t_lower_ad = 0.0_wp

           ENDIF

           t_lower_ad = 0.0_wp

           nr_ad(i+1) = nr_ad(i+1) +0.5_wp/SQRT( nr(i+1)-impact(l))*t_upper_ad 
           t_upper_ad = 0.0_wp

           refrac_ad(i+1) = refrac_ad(i+1) + dn_dx_ad/(nr(i+1)-nr(i))
           refrac_ad(i  ) = refrac_ad(i  ) - dn_dx_ad/(nr(i+1)-nr(i))

           nr_ad(i+1) = nr_ad(i+1) - dn_dx/(nr(i+1)-nr(i))*dn_dx_ad
           nr_ad(i  ) = nr_ad(i  ) + dn_dx/(nr(i+1)-nr(i))*dn_dx_ad
           dn_dx_ad = 0.0_wp

        ELSE

!       5.2.1 Values of refractivity and impact parameter at lower level
!       ----------------------------------------------------------------

        refrac_low = refrac(i)
        nr_low = nr(i) 

!       5.2.2 Upper and lower bounds of the integral
!       -------------------------------------------------------------------

        t_upper = SQRT(kval(i) * (nr(i+1) - impact(l)))

        IF (i == i_bot) THEN
           t_lower = 0.0_wp
        ELSE
           t_lower = SQRT(kval(i) * (nr(i) - impact(l)))
        ENDIF

!       5.2.3 Integral
!       --------------

        ! Approximate error function with polynomial
        zt_low = 1.0_wp / (1.0_wp + 0.47047_wp * t_lower)
        erf_low = 1.0_wp-(a-(b-c*zt_low)*zt_low)*zt_low*EXP(-(t_lower*t_lower))
        zt_up = 1.0_wp / (1.0_wp + 0.47047_wp * t_upper)
        erf_up = 1.0_wp - (a-(b-c*zt_up)*zt_up)*zt_up*EXP(-(t_upper*t_upper))

        IF (i == n_lev-1) erf_up = 1.0_wp

!       5.2.5 New terms for integral that now allows kval to vary within layer
!       ------------------------------------------------------------------

         ! these p1, p2,p3 values correspond the case where dT/dx = beta = 0, i.e kval is constant!

           p1 = kval(i)
           p2 = 0.0_wp
           p3 = 0.0_wp

         ! impact height of level

           zed = nr(i) - roc

           IF ( i < n_lev-1 .AND. zed > imp_ht_min .AND. Tgrad_oper) THEN

         ! compute the "p" values for temp. gradient beta 

             p1 = kval(i)*(1.0_wp + beta(i)/temp_mid(i)* &
              (0.5_wp*kval(i)*((impact(l)-nr_mid(i))**2 - dval(i)) -(impact(l)-nr_mid(i))))

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

!       5.2.4 Bending angle value
!       -------------------------
!
!           bangle(l) = bangle(l) &
!                     + 1.0e-6_wp * SQRT(2.0_wp*impact(l)) & 
!                     * refrac_low * EXP(kval(i) * (nr_low - impact(l)))     &
!                     * integral_diff

!       5.2.5 Adjoint bending angle value
!       ---------------------------------

        factor  = 1.0e-6_wp * SQRT(2.0_wp * impact(l)) &
                         * EXP(kval(i) * (nr_low - impact(l)))

        refrac_low_ad = refrac_low_ad + factor*integral_diff*bangle_ad(l)

        integral_diff_ad = integral_diff_ad + factor*refrac_low*bangle_ad(l)

        kval_ad(i) = kval_ad(i) + &
        factor*refrac_low*integral_diff*(nr_low - impact(l))*bangle_ad(l)

        nr_low_ad = nr_low_ad + factor*refrac_low*integral_diff*kval(i)*bangle_ad(l)

        bangle_ad(l) = bangle_ad(l)

!       5.2.6 Adjoint integral
!       ----------------------

        int_up_ad  = int_up_ad + integral_diff_ad
        int_low_ad = int_low_ad - integral_diff_ad
        integral_diff_ad = 0.0_wp

        IF (i > i_bot) THEN

           part1 = EXP(-kval(i)*(nr(i)-impact(l)))
           part2 = SQRT(nr(i)-impact(l))/kval(i)
           part3 = p2 + p3* (nr(i)-impact(l)+1.5_wp/kval(i))

           part1_ad = part1_ad - part2*part3*int_low_ad
           part2_ad = part2_ad - part1*part3*int_low_ad
           part3_ad = part3_ad - part1*part2*int_low_ad
           int_low_ad = int_low_ad

           p2_ad = p2_ad + part3_ad
           p3_ad = p3_ad + (nr(i)-impact(l)+1.5_wp/kval(i))*part3_ad
           nr_ad(i) = nr_ad(i) + p3*part3_ad
           kval_ad(i) = kval_ad(i) - p3*1.5_wp/kval(i)**2*part3_ad
           part3_ad = 0.0_wp

           kval_ad(i) = kval_ad(i) -part2/kval(i)*part2_ad
           nr_ad(i) = nr_ad(i) + 0.5_wp*part2/(nr(i)-impact(l))*part2_ad
           part2_ad = 0.0_wp

           kval_ad(i) = kval_ad(i) - part1*(nr(i)-impact(l))*part1_ad
           nr_ad(i)   = nr_ad(i) - part1*kval(i)*part1_ad
           part1_ad  = 0.0_wp

           part1 = sqrt(pi/kval(i))
           part2 = p1 + 0.5_wp/kval(i)*(p2+1.5_wp*p3/kval(i))
           part3 = erf_low

           part1_ad = part1_ad + part2*part3*int_low_ad
           part2_ad = part2_ad + part1*part3*int_low_ad
           part3_ad = part3_ad + part1*part2*int_low_ad
           int_low_ad = 0.0_wp

           erf_low_ad = erf_low_ad + part3_ad
           part3_ad = 0.0_wp

          ! check

           p1_ad = p1_ad + part2_ad
           kval_ad(i) = kval_ad(i) -0.5_wp/kval(i)**2*(p2+1.5_wp*p3/kval(i))*part2_ad
           p2_ad = p2_ad + 0.5_wp/kval(i)*part2_ad
           p3_ad = p3_ad + 0.75_wp/kval(i)**2*part2_ad
           kval_ad(i) = kval_ad(i) - 0.75_wp*p3/kval(i)**3*part2_ad
           part2_ad = 0.0_wp


           kval_ad(i) = kval_ad(i) - 0.5_wp*part1/kval(i)*part1_ad
           part1_ad = 0.0_wp

        ENDIF

        int_low_ad = 0.0_wp

        part1 = EXP(-kval(i)*(nr(i+1)-impact(l)))
        part2 = SQRT(nr(i+1)-impact(l))/kval(i)
        part3 = p2 + p3* (nr(i+1)-impact(l)+1.5_wp/kval(i))

        part1_ad = part1_ad - part2*part3*int_up_ad
        part2_ad = part2_ad - part1*part3*int_up_ad
        part3_ad = part3_ad - part1*part2*int_up_ad
        int_up_ad = int_up_ad

        p2_ad = p2_ad + part3_ad
        p3_ad = p3_ad + (nr(i+1)-impact(l)+1.5_wp/kval(i))*part3_ad
        nr_ad(i+1) = nr_ad(i+1) + p3*part3_ad
        kval_ad(i) = kval_ad(i) - p3*1.5_wp/kval(i)**2*part3_ad
        part3_ad = 0.0_wp

        kval_ad(i) = kval_ad(i) -part2/kval(i)*part2_ad
        nr_ad(i+1) = nr_ad(i+1) + 0.5_wp*part2/(nr(i+1)-impact(l))*part2_ad
        part2_ad = 0.0_wp

        kval_ad(i) = kval_ad(i) - part1*(nr(i+1)-impact(l))*part1_ad
        nr_ad(i+1) = nr_ad(i+1) - part1*kval(i)*part1_ad
        part1_ad = 0.0_wp

        part1 = sqrt(pi/kval(i))
        part2 = p1 + 0.5_wp/kval(i)*(p2+1.5_wp*p3/kval(i))
        part3 = erf_up

        part1_ad = part1_ad + part2*part3*int_up_ad
        part2_ad = part2_ad + part1*part3*int_up_ad
        part3_ad = part3_ad + part1*part2*int_up_ad
        int_up_ad = 0.0_wp

        erf_up_ad = erf_up_ad + part3_ad
        part3_ad = 0.0_wp

          ! check

        p1_ad = p1_ad + part2_ad
        kval_ad(i) = kval_ad(i) -0.5_wp/kval(i)**2*(p2+1.5_wp*p3/kval(i))*part2_ad
        p2_ad = p2_ad + 0.5_wp/kval(i)*part2_ad
        p3_ad = p3_ad + 0.75_wp/kval(i)**2*part2_ad
        kval_ad(i) = kval_ad(i) - 0.75_wp*p3/kval(i)**3*part2_ad
        part2_ad = 0.0_wp

        kval_ad(i) = kval_ad(i) - 0.5_wp*part1/kval(i)*part1_ad
        part1_ad = 0.0_wp

        zed = nr(i) - roc

        IF ( i < n_lev-1 .AND. zed > imp_ht_min .AND. Tgrad_oper) THEN

! calculate P3

          uval =  beta(i)/temp_mid(i) ! convenient variable for layer.

          uval_ad = uval_ad + 0.5_wp*kval(i)**2*p3_ad
          kval_ad(i) = kval_ad(i) + uval*kval(i)*p3_ad
          p3_ad = 0.0_wp

! calculate P2

          part1 = kval(i)*uval
          part2 = kval(i)*(impact(l)-nr_mid(i))-1.0_wp

          part1_ad =  part1_ad +  part2*p2_ad
          part2_ad =  part2_ad +  part1*p2_ad
          p2_ad = 0.0_wp

          kval_ad(i) = kval_ad(i) +(impact(l)-nr_mid(i))*part2_ad
          nr_mid_ad(i) = nr_mid_ad(i) -kval(i)*part2_ad
          part2_ad = 0.0_wp

          kval_ad(i) = kval_ad(i) +uval*part1_ad
          uval_ad = uval_ad + kval(i)*part1_ad
          part1_ad = 0.0_wp

! calculate P1

          part1 = uval
          part2 = 0.5_wp*kval(i)*((impact(l)-nr_mid(i))**2 - dval(i)) -(impact(l)-nr_mid(i))
                    
          kval_ad(i) = kval_ad(i) + (1.0_wp + part1*part2)*p1_ad
          part1_ad = part1_ad + kval(i)*part2*p1_ad
          part2_ad = part2_ad + kval(i)*part1*p1_ad 
          p1_ad = 0.0_wp
          
          kval_ad(i) = kval_ad(i) + &
            0.5_wp*((impact(l)-nr_mid(i))**2 - dval(i))*part2_ad
          nr_mid_ad(i) = nr_mid_ad(i) - &
            kval(i)*(impact(l)-nr_mid(i))*part2_ad
          dval_ad(i) = dval_ad(i) - 0.5_wp*kval(i)*part2_ad
          nr_mid_ad(i) = nr_mid_ad(i)+part2_ad
          part2_ad = 0.0_wp

          uval_ad = uval_ad + part1_ad
          part1_ad = 0.0_wp

          beta_ad(i) = beta_ad(i) + uval_ad/temp_mid(i)
          temp_mid_ad(i) = temp_mid_ad(i) - uval/temp_mid(i)*uval_ad
          uval_ad = 0.0_wp

        ENDIF

        kval_ad(i) = kval_ad(i) + p1_ad
        p1_ad = 0.0_wp
        p2_ad = 0.0_wp
        p3_ad = 0.0_wp

!       5.2.4 Error function
!       --------------------

        IF (i == n_lev-1) THEN

        ! uppermost level erf_up = 1.0

            erf_up_ad = 0.0_wp

        ENDIF

        t_upper_ad = t_upper_ad + (a-(b-c*zt_up)*zt_up) * zt_up          &
                     * EXP(-(t_upper*t_upper)) * 2.0_wp * t_upper * erf_up_ad
        zt_up_ad = zt_up_ad - (a-(2.0_wp*b-3.0_wp*c*zt_up)*zt_up)        &
                   * EXP(-(t_upper*t_upper)) * erf_up_ad
        erf_up_ad = 0.0_wp

        t_lower_ad = t_lower_ad + (a-(b-c*zt_low)*zt_low) * zt_low       &
                     * EXP(-(t_lower*t_lower)) * 2.0_wp * t_lower * erf_low_ad
        zt_low_ad = zt_low_ad - (a-(2.0_wp*b-3.0_wp*c*zt_low)*zt_low)    &
                    * EXP(-(t_lower*t_lower)) * erf_low_ad
        erf_low_ad = 0.0_wp

        t_lower_ad = t_lower_ad - 0.47047_wp * zt_low * zt_low * zt_low_ad
        zt_low_ad = 0.0_wp

        t_upper_ad = t_upper_ad - 0.47047_wp * zt_up * zt_up * zt_up_ad
        zt_up_ad = 0.0_wp

!       5.2.7 Adjoint upper and lower bounds of the integral
!       ----------------------------------------------------

        IF (i == i_bot) THEN
           t_lower_ad   = 0.0_wp
        ELSE
           nr_ad(i)   = nr_ad(i) + 0.5_wp * kval(i) / t_lower * t_lower_ad
           kval_ad(i) = kval_ad(i) +    &
                          0.5_wp*(nr(i)-impact(l))/t_lower * t_lower_ad
           t_lower_ad = 0.0_wp
        ENDIF

        nr_ad(i+1) = nr_ad(i+1) + 0.5_wp * kval(i) / t_upper * t_upper_ad
        kval_ad(i) = kval_ad(i) +    &
                      0.5_wp * (nr(i+1) - impact(l)) / t_upper * t_upper_ad
        t_upper_ad = 0.0_wp

        ENDIF

!       5.2.8 Adjoint values of refractivity and impact parameter at lower level
!       ------------------------------------------------------------------------

        nr_ad(i)      = nr_ad(i) + nr_low_ad
        nr_low_ad     = 0.0_wp
        refrac_ad(i)  = refrac_ad(i) + refrac_low_ad
        refrac_low_ad = 0.0_wp

     ENDDO

     bangle_ad(l) = 0.0_wp

  ENDDO

!-------------------------------------------------------------------------------
! 6. Adjoint code - calculate exponential decay rate between levels
!-------------------------------------------------------------------------------

  DO i = n_lev - 1, 1, -1

! new dval bit

     nr_ad(i) = nr_ad(i) + 2.0_wp*(nr(i) -nr_mid(i))*dval_ad(i)
     nr_mid_ad(i) = nr_mid_ad(i) - 2.0_wp*(nr(i) -nr_mid(i))*dval_ad(i)
     dval_ad(i) = 0.0_wp

! temp gradient

     IF ((nr(i+1) - nr(i)) > 1.0_wp) THEN

       temp_ad(i+1) = temp_ad(i+1) + beta_ad(i)/(nr(i+1) - nr(i))
       temp_ad(i  ) = temp_ad(i  ) - beta_ad(i)/(nr(i+1) - nr(i))

       nr_ad(i+1) = nr_ad(i+1) - beta(i)/(nr(i+1) - nr(i))*beta_ad(i)
       nr_ad(i  ) = nr_ad(i  ) + beta(i)/(nr(i+1) - nr(i))*beta_ad(i)
       beta_ad(i) = 0.0_wp

     ELSE

       temp_ad(i+1) = temp_ad(i+1) + beta_ad(i)
       temp_ad(i  ) = temp_ad(i  ) - beta_ad(i)
       beta_ad(i) = 0.0_wp

     ENDIF

     temp_ad(i)   = temp_ad(i) + 0.5_wp*temp_mid_ad(i)
     temp_ad(i+1) = temp_ad(i+1) + 0.5_wp*temp_mid_ad(i)
     temp_mid_ad(i) = 0.0_wp

     nr_ad(i)   = nr_ad(i)   + 0.5_wp*nr_mid_ad(i)
     nr_ad(i+1) = nr_ad(i+1) + 0.5_wp*nr_mid_ad(i)
     nr_mid_ad(i) = 0.0_wp

     factor = 1.0_wp / MAX(1.0_wp, nr(i+1) - nr(i))

! limit the size of the refractivity gradient

     IF (kval_tmp(i) > 0.157_wp/refrac(i)) THEN

         refrac_ad(i) = refrac_ad(i) - 0.157_wp/refrac(i)**2*kval_ad(i)

         kval_ad(i) = 0.0_wp

     ENDIF

     refrac_ad(i+1) = refrac_ad(i+1) - factor / refrac(i+1) * kval_ad(i)
     refrac_ad(i)   = refrac_ad(i) + factor / refrac(i)   * kval_ad(i)
     nr_ad(i+1)     = nr_ad(i+1) - factor * kval(i) * kval_ad(i)
     nr_ad(i)       = nr_ad(i) + factor * kval(i) * kval_ad(i)
     kval_ad(i)     = 0.0_wp

  ENDDO

  DEALLOCATE(kval)
  DEALLOCATE(kval_tmp)
  DEALLOCATE(kval_ad)

  DEALLOCATE(beta)
  DEALLOCATE(beta_ad)
  
  DEALLOCATE(nr_mid)
  DEALLOCATE(nr_mid_ad)
  
  DEALLOCATE(temp_mid)
  DEALLOCATE(temp_mid_ad)

  DEALLOCATE(dval)
  DEALLOCATE(dval_ad)

END SUBROUTINE ropp_fm_abel_ad
