! $Id$

SUBROUTINE ropp_fm_refrac_1d(x, y)

!****s* Refractivity/ropp_fm_refrac_1d *
!
! NAME
!    ropp_fm_refrac_1d - Forward model calculating a one dimensional 
!                        refractivity profile from the state vector 
!                        assuming exponential variation of refractivity 
!                        between model levels.
!
! SYNOPSIS
!    call ropp_fm_refrac_1d(x, y)
! 
! DESCRIPTION
!    This routine is a forward model calculating a vertical profile of 
!    refractivity from profiles of temperature, humidity and pressure. 
!    Refractivity values are calculated for the geopotential height levels 
!    given in the observation vector.
!
! INPUTS
!    type(State1dFM)     :: x      ! State vector structure
!    type(Obs1dRefrac)   :: y      ! Observation vector (levels required)
!
! OUTPUT
!    type(Obs1dRefrac)   :: y      ! Obs vector with forward modelled refrac
!
! NOTES
!    The forward model assumes that the state vector structure contains 
!    temperature, humidity and pressure values on common geopotential height 
!    levels, independent of the source of those data. Model-dependent 
!    conversion routines can be used to accomplish this within the 
!    ropp_fm_roprof2state() subroutine.
! 
!    The forward model assumes that the observation vector contains 
!    geopotential height levels onto which the forward simulated
!    observations are interpolated.
!
!    The interpolation of refractivity calculated at the state vector's
!    geopotential height levels to the observation vector's geopotential height
!    levels is carried out assuming that refractivity varies exponentially
!    with geopotential height.
!
! SEE ALSO
!    ropp_fm_types
!    ropp_fm_refrac_1d_ad
!    ropp_fm_refrac_1d_tl
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
  USE ropp_utils, ONLY: ropp_MDFV
  USE ropp_fm,   not_this => ropp_fm_refrac_1d
  USE ropp_fm_types
  USE ropp_fm_constants

  IMPLICIT NONE

  TYPE(State1dFM),   INTENT(in)    :: x       ! State vector
  TYPE(Obs1dRefrac), INTENT(inout) :: y       ! Observation vector

! Local variables
  REAL(wp), DIMENSION(x%n_lev)     :: pwvp            ! Partial water vapour pressure
  REAL(wp), DIMENSION(x%n_lev)     :: pdry            ! Partial dry air pressure
  REAL(wp), DIMENSION(x%n_lev)     :: refrac          ! Refractivity
  REAL(wp), DIMENSION(x%n_lev)     :: z_geop          ! Geopotential height of model levels
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_dry_inv   ! Dry compressibility
  REAL(wp), DIMENSION(x%n_lev)     :: zcomp_wet_inv   ! Wet compressibility

  REAL(wp)                         :: kap1,kap2,kap3  ! Refractivity coefficients used in routine

  INTEGER                          :: i

  REAL(wp), DIMENSION(:,:), ALLOCATABLE     :: refrac_re          ! Refractivity

!-------------------------------------------------------------------------------
! 2. Non ideal gas options
!-------------------------------------------------------------------------------

! set inverse of compressibilities

  zcomp_dry_inv(:) = 1.0_wp
  zcomp_wet_inv(:) = 1.0_wp

! initialise geopotential heights

  z_geop(:) = x%geop(:)

  IF (x%non_ideal) THEN

! if non ideal gas calculation, use adjusted coefficients

     kap1 = kappa1_comp
     kap2 = kappa2_comp
     kap3 = kappa3_comp

!    calculate compressibilty and adjust geopotential heights in z_geop

     CALL ropp_fm_compress(x,z_geop,zcomp_dry_inv,zcomp_wet_inv)

  ELSE

     kap1 = kappa1
     kap2 = kappa2
     kap3 = kappa3

  ENDIF

!-------------------------------------------------------------------------------
! 3. Standard exponentially varying refractivity assumption
!-------------------------------------------------------------------------------

! 3.1 Calculate water vapor and dry air pressure
!-----------------------------------------------

  pwvp = x%pres * x%shum / (epsilon_water + (1.0_wp - epsilon_water)*x%shum)

  pdry = x%pres - pwvp

! 3.2 Calculate refractivity
!---------------------------

  refrac = kap1 * pdry * zcomp_dry_inv/ x%temp +  &
           kap2 * pwvp * zcomp_wet_inv/ x%temp**2 + &
           kap3 * pwvp * zcomp_wet_inv/ x%temp

! 3.3 Interpolate to measurements geopotential height levels, either using spline or linear interpolation
!--------------------------------------------------------------------------------------------------------
  ALLOCATE(refrac_re(SIZE(refrac),1))

  IF (x%spline_int) THEN
    
    refrac_re = RESHAPE(refrac, (/SIZE(refrac),1/))
    
    CALL ropp_fm_spline_log(z_geop, y%geop, refrac_re, y%refrac)

  ELSE
   
    CALL ropp_fm_interpol_log(z_geop, y%geop, refrac, y%refrac)

  ENDIF

  DEALLOCATE(refrac_re)

! 3.4 Set weight to zero if ob height is below model surface
!-----------------------------------------------------------

  DO i=1, SIZE(y%geop)
    IF ( (y%geop(i) < x%geop(1)) .OR. (y%geop(i) > x%geop(x%n_lev)) ) THEN
      y%refrac(i) = ropp_MDFV
      y%weights(i) = 0.0_wp
    END IF
  END DO

END SUBROUTINE ropp_fm_refrac_1d
