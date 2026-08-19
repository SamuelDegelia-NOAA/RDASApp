! $Id: ropp_fm_tdry.f90 1960 2010-10-02 00:00:00Z idculv $

SUBROUTINE ropp_fm_tdry(lat, alt, refrac, shum, t_dry, p_dry, Zmax, Zscale, Zstep)

!****s* Meteo/ropp_fm_tdry *
!
! NAME
!    ropp_fm_tdry - Compute temperature (and pressure profile) from
!                   refractivity assuming zero humidity.
!
! SYNOPSIS
!    CALL ropp_fm_tdry(lat, alt, refrac, shum, t_dry, p_dry, Zmax, Zscale, Zstep)
!
! DESCRIPTION
!    Calculate P(z) from numerical integration of barometric formula
!
!            d ln(P(z))                    g(z)
!            ---------- = - -------------------------------------
!               dz           Rd T(N(z),P(z),Q(z)) (1 + eps(Q(z))
!
!    given N(z) and Q(z) and known dependence T(N, P, Q).
!    Calculate T(z) = T(N(z), P(z), Q(z)). 
!
! INPUTS
!    REAL(wp)                 :: lat       ! latitude
!    REAL(wp), DIMENSION(:)   :: alt       ! altitude (z)
!    REAL(wp), DIMENSION(:)   :: refrac    ! refraction (N(z))
!    REAL(wp), DIMENSION(:)   :: shum      ! specific humidity (Q(z))
!    REAL(wp), OPTIONAL       :: Zmax      ! altitude of upper integration boundary 
!    REAL(wp), OPTIONAL       :: Zscale    ! scale height governing upper boundary conditions
!    REAL(wp), OPTIONAL       :: Zstep     ! integration step size
!
! OUTPUT
!    REAL(wp), DIMENSION(:)   :: t_dry     ! dry temperature (T(z))
!    REAL(wp), DIMENSION(:)   :: p_dry     ! dry pressure (P(z))
!
! AUTHOR
!   M Gorbunov, Russian Academy of Sciences, Russia.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   Copyright (c) 1998-2010 Michael Gorbunov <michael.gorbunov@zmaw.de>
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****


!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ropp_fm_constants

  IMPLICIT NONE

  REAL(wp),                       INTENT(in)  :: lat    ! Latitude
  REAL(wp), DIMENSION(:), TARGET, INTENT(in)  :: alt    ! Altitude (m)
  REAL(wp), DIMENSION(:),         INTENT(in)  :: refrac ! Refractivity
  REAL(wp), DIMENSION(:), TARGET, INTENT(in)  :: shum   ! Spec humidity (kg/kg)
  REAL(wp), DIMENSION(:),         INTENT(out) :: t_dry  ! Dry temperature (K)
  REAL(wp), DIMENSION(:),         INTENT(out) :: p_dry  ! Dry pressure (hPa)
  REAL(wp), OPTIONAL,             INTENT(in)  :: Zmax   ! Altitude of upper integration boundary (m)
  REAL(wp), OPTIONAL,             INTENT(in)  :: Zscale ! Scale height governing upper boundary conditions (m)
  REAL(wp), OPTIONAL,             INTENT(in)  :: Zstep  ! Integration step size (m)

  REAL(wp), TARGET, DIMENSION(:), ALLOCATABLE :: d2N    ! 2nd derivative ln(N)
  REAL(wp), TARGET, DIMENSION(:), ALLOCATABLE :: d2Q    ! 2nd derivative shum
  REAL(wp), TARGET, DIMENSION(:), ALLOCATABLE :: lnN    ! ln(N)

  REAL(wp), DIMENSION(:), ALLOCATABLE         :: ZI     ! Altitude grid
  REAL(wp), DIMENSION(:), ALLOCATABLE         :: TZ     ! Temperature on ZI
  REAL(wp), DIMENSION(:), ALLOCATABLE         :: D2T    ! 2nd derivative TZ
  REAL(wp), DIMENSION(:), ALLOCATABLE         :: lnPZ   ! ln(P) on ZI
  REAL(wp), DIMENSION(:), ALLOCATABLE         :: D2P    ! 2nd derivative LnPZ

  REAL(wp), DIMENSION(:), POINTER             :: PZ     ! Pointer to alt
  REAL(wp), DIMENSION(:), POINTER             :: PN     ! Pointer to refrac
  REAL(wp), DIMENSION(:), POINTER             :: P2N    ! Pointer to d2N
  REAL(wp), DIMENSION(:), POINTER             :: PQ     ! Pointer to shum
  REAL(wp), DIMENSION(:), POINTER             :: P2Q    ! Pointer to d2Q
      
  REAL(wp)                                    :: GCLat  ! Geocentric latitude
  REAL(wp)                                    :: LnP    ! ln(p)
  REAL(wp)                                    :: LnNZ   ! ln(NZ) on ZI
  REAL(wp)                                    :: DLnNZ  ! 1st derivative LnNZ
  REAL(wp)                                    :: QZ     ! shum on ZI
  REAL(wp)                                    :: ZP     ! Altitude value
  REAL(wp)                                    :: Z1     ! Maximum altitude
  REAL(wp)                                    :: Zmin   ! Minimum altitude = minval(alt)
  REAL(wp)                                    :: Ztop   ! Profile top = maxval(alt)
  REAL(wp)                                    :: dz     ! Integration step
  INTEGER                                     :: KZ     ! No. integration steps
  INTEGER                                     :: i      ! Index
  LOGICAL                                     :: use_Zscale

  REAL(wp), PARAMETER                         :: dzi = 15.0_wp  ! Integration step size (m)
  REAL(wp), PARAMETER                         :: Qmin = 1e-7_wp ! Minimum value for QZ

!-------------------------------------------------------------------------------
! 2. Initialization
!-------------------------------------------------------------------------------

  IF (ANY( refrac <= ropp_ZERO )) THEN

    CALL message(msg_warn,   &
         "Cannot generate Tdry from non-positive refractivities \n")
    t_dry = refrac*0.0_wp + ropp_MDFV
    p_dry = refrac*0.0_wp + ropp_MDFV

  ELSE

    ALLOCATE(d2N(SIZE(alt)))
    ALLOCATE(d2Q(SIZE(alt)))
    ALLOCATE(lnN(SIZE(alt)))

!-------------------------------------------------------------------------------
! 3. Calculate spline coefficients
!-------------------------------------------------------------------------------

    lnN = LOG(refrac)
    CALL ropp_fm_init_spline(alt(:), lnN(:), d2N)
    CALL ropp_fm_init_spline(alt(:), shum(:), d2Q)

!-------------------------------------------------------------------------------
! 4. Set up global variables for FTZP
!-------------------------------------------------------------------------------

    PZ   => alt
    PN   => lnN
    P2N  => d2N
    PQ   => shum
    P2Q  => d2Q

!-------------------------------------------------------------------------------
! 5. Define upper boundary & compute number of steps and step size
!-------------------------------------------------------------------------------

    Zmin = minval(alt(:))
    Ztop = maxval(alt(:))

    IF (PRESENT(Zmax)) THEN 
      Z1 = Zmax
    ELSE
      Z1 = Ztop
    ENDIF

    IF (PRESENT(Zscale)) THEN
      use_Zscale = .true.
    ELSE
      use_Zscale = .false.
    ENDIF

    IF (PRESENT(Zstep)) THEN
      KZ = CEILING((Z1 - Zmin)/ABS(Zstep))
    ELSE
      KZ = CEILING((Z1 - Zmin)/ABS(DZI))
    ENDIF

    dz = -(Z1 - Zmin)/KZ

!-------------------------------------------------------------------------------
! 6. Array allocation
!-------------------------------------------------------------------------------

    ALLOCATE(ZI(0:KZ))
    ALLOCATE(TZ(0:KZ))
    ALLOCATE(D2T(0:KZ))
    ALLOCATE(LnPZ(0:KZ))
    ALLOCATE(D2P(0:KZ))

!-------------------------------------------------------------------------------
! 7. Numerical integration
!-------------------------------------------------------------------------------

    ! 7.1 Geocentric latitude

    GCLat = GCLat_from_GDLat(lat)

    ! 7.2 Initial conditions

    ZP = Z1
    ZI(KZ) = ZP

    IF (use_Zscale) THEN
      CALL ropp_fm_interpol_spline(alt, lnN, d2N, Z1, LnNZ)  ! ln(N) at the upper boundary Z1.
      DLnNZ = -1.0_wp / Zscale                               ! dln(N)/dz at the upper boundary Z1.
      TZ(KZ) = - GravityGC(GCLat, ZP) / (R_dry*DLnNZ)        ! Temperature in the isothermal profile extrapolation.
      LnP = LnNZ + LOG(TZ(KZ)/kappa1)                        ! Pressure at the upper boundary Z1.
      LnPZ(KZ) = LnP
    ELSE
      CALL ropp_fm_interpol_spline(alt, lnN, d2N, Z1, LnNZ, DLnNZ)
      TZ(KZ) = - GravityGC(GCLat, ZP) / (R_dry*DLnNZ)
      LnP = LnNZ + LOG(TZ(KZ)/kappa1)
      LnPZ(KZ) = LnP
    ENDIF

    ! 7.3 Integration

    DO i= kz-1, 0, -1
      CALL ropp_fm_runge_kutta(dz, zp, lnP)                 ! Step according to barometric equation.
      CALL ropp_fm_interpol_spline(pz, pn, p2n, zp, lnNz)   ! Interpolate ln(N) from the observed profile to altitude zp.
      CALL ropp_fm_interpol_spline(pz, pq, p2q, zp, QZ)     ! Interpolate q from the observed profile to altitude zp.

      QZ = MAX(Qmin, QZ)
      ZI(i) = ZP
      TZ(i) = T_from_NPQ(EXP(LnNZ), EXP(LnP), QZ)
      LNPZ(i) = LnP
   ENDDO

!-------------------------------------------------------------------------------
! 8. Interpolation of calculated T(Z) and P(Z)
!-------------------------------------------------------------------------------

    ! 8.1 Temperature
    CALL ropp_fm_init_spline(ZI, TZ, D2T)
    DO i=1,SIZE(alt)
      CALL ropp_fm_interpol_spline(zi, tz, d2t, alt(i), t_dry(i))
    ENDDO

    ! 8.2 Pressure
    CALL ropp_fm_init_spline(ZI, LnPZ, D2P)
    DO i=1,SIZE(alt)
      CALL ropp_fm_interpol_spline(zi, lnPZ, d2p, alt(i), p_dry(i))
    ENDDO

    p_dry(:) = EXP(p_dry(:))

!-------------------------------------------------------------------------------
! 9. Clean up
!-------------------------------------------------------------------------------

    DEALLOCATE(ZI)
    DEALLOCATE(TZ)
    DEALLOCATE(D2T)
    DEALLOCATE(LnPZ)
    DEALLOCATE(D2P)

    DEALLOCATE(d2N)
    DEALLOCATE(d2Q)
    DEALLOCATE(lnN)

  ENDIF  ! Refrac > 0

CONTAINS

!-------------------------------------------------------------------------------
! 10. Conversion Geodetic to Geocentric latitude
!-------------------------------------------------------------------------------

  FUNCTION GCLat_from_GDLat(GDLat) RESULT(GCLat)

    USE typesizes, ONLY: wp => EightByteReal
    USE ropp_fm_constants, ONLY: Pi
    IMPLICIT NONE

    REAL(wp), INTENT(in) :: GDLat
    REAL(wp) :: GCLat

    REAL(wp) :: S, X(3), gd, flatfn, funsq
    REAL(wp), PARAMETER :: Re  = 6378137.0_wp
    REAL(wp), PARAMETER :: dtr = Pi/180.0_wp
    REAL(wp), PARAMETER :: f_ell = 1.0_wp / 298.257223563_wp

    ! 10.1 Convert geodetic to cartesian coordinates

    flatfn = (2.0_wp - f_ell)*f_ell
    funsq = (1.0_wp - f_ell)**2
    gd = Re / SQRT(1.0_wp - flatfn*SIN(GDLat*dtr)**2)

    X(2) = COS(GDLat*dtr)*SIN(0.0_wp*dtr)*gd
    X(1) = COS(GDLat*dtr)*COS(0.0_wp*dtr)*gd
    X(3) = SIN(GDLat*dtr)*(gd*funsq)

    ! 10.2 Convert geodetic to spherical coordinates
    
    S = ACOS(X(3)/(SQRT(SUM(X(:)**2))))

    ! 10.3 Compute geodetic latitude
    
    GCLat = Pi/2.0_wp - S

  END FUNCTION GCLat_from_GDLat

!-------------------------------------------------------------------------------
! 11. Gravity calculation
!-------------------------------------------------------------------------------

  FUNCTION GravityGC(Lat, Alt)

    USE typesizes, ONLY: wp => EightByteReal
    IMPLICIT NONE

    REAL(wp), INTENT(in) :: Lat
    REAL(wp), INTENT(in) :: Alt
    REAL(wp) :: GravityGC

    REAL(wp) :: g_sur, R0, ge, f2, f4
    REAL(wp), PARAMETER :: Re  = 6378137.0_wp
    REAL(wp), PARAMETER :: m = 0.00345_wp
    REAL(wp), PARAMETER :: f_ell = 1.0_wp / 298.257223563_wp

    f2 = -f_ell + 5.0_wp*m/2.0_wp - 17.0_wp*f_ell*m/14.0_wp +   &
            15.0_wp*m**2/4.0_wp
    f4 = -f_ell**2/2.0_wp + 5.0_wp*f_ell*m/2.0_wp

    ge = 3.986004415e14_wp / (Re**2 * &
       (1.0_wp - f_ell + 3.0_wp*m/2.0_wp - 15.0_wp*m*f_ell/14.0_wp))

    g_sur = ge*(1.0_wp + f2*(Sin(Lat))**2 - (f4*(Sin(2.0_wp*Lat))**2)/4.0_wp)

    R0 = (g_sur/ge)*Re / (1.0_wp + f_ell + m +      &
            (-3.0_wp*f_ell + 5.0_wp*m/2.0_wp)*(Sin(Lat))**2)

    GravityGC = g_sur*(R0/(R0 + Alt))**2

  END FUNCTION GravityGC

!-------------------------------------------------------------------------------
! 12. Right-hand size of barometric formula
!-------------------------------------------------------------------------------

  FUNCTION FTZP(ZP, LnP)

    USE typesizes, ONLY: wp => EightByteReal
    USE ropp_fm_constants, ONLY: epsilon_water, R_dry
    IMPLICIT NONE

    REAL(wp), INTENT(in) :: zp   ! altitude above reference ellipsoid
    REAL(wp), INTENT(in) :: lnP  ! logarithm of pressure
    REAL(wp)             :: ftzp ! right part of barometric formula

    REAL(wp)             :: lnNZ ! interpolated ln(N(z))
    REAL(wp)             :: QZ   ! interpolated Q(Z)
    REAL(wp)             :: TZP  ! T(z, P(z))
    REAL(wp)             :: GZ   ! gravity acceleration g(lat, z)

    CALL ropp_fm_interpol_spline(pz, pn, p2n, zp, lnNz)
    CALL ropp_fm_interpol_spline(pz, pq, p2q, zp, qz)

    qz = MAX(Qmin, Qz)
    TzP = T_from_NPQ(EXP(LnNZ), EXP(LnP), QZ)
    GZ = GravityGC(GClat, zp)

    ftzp = -GZ  / (R_dry * Tzp * (1.0_wp + (1.0_wp/epsilon_water-1.0_wp)*QZ))

  END FUNCTION FTZP

!-------------------------------------------------------------------------------
! 13. Calculate temperature from refractivity, pressure and humidity
!-------------------------------------------------------------------------------

  FUNCTION T_from_NPQ(N, P, Q) RESULT(T)

    USE typesizes, ONLY: wp => EightByteReal
    USE ropp_fm_constants, ONLY: R_dry, R_vap, kappa1, kappa2
    IMPLICIT NONE

    REAL(wp), INTENT(in) :: N
    REAL(wp), INTENT(in) :: P
    REAL(wp), INTENT(in) :: Q
    REAL(wp)             :: T

    REAL(wp), PARAMETER :: aq =  R_dry/R_vap
    REAL(wp), PARAMETER :: bq =  1.0_wp - aq

    ! 11.1 Analytical solution

    T = (kappa1*P + SQRT((kappa1*P)**2 + 4.0_wp*kappa2*N*P*Q/(aq + bq*Q))) /   &
         (2.0_wp*N)

  END FUNCTION T_from_NPQ

!-------------------------------------------------------------------------------
! 14. Runge_kutta numerical integration of X' = F(X,t)
!-------------------------------------------------------------------------------

  SUBROUTINE ropp_fm_runge_kutta(Dt, t, X)

    USE typesizes, ONLY: wp => EightByteReal
    
    IMPLICIT NONE

    REAL(wp),  INTENT(in)    :: dt   ! time integration step
    REAL(wp),  INTENT(inout) :: t    ! time variable
    REAL(wp),  INTENT(inout) :: X    ! dynamic variable vector

    REAL(wp) :: K1, K2, K3, K4

    K1 = FTZP(t,        X)
    K2 = FTZP(t + Dt/2, X + K1*Dt/2)
    K3 = FTZP(t + Dt/2, X + K2*Dt/2)
    K4 = FTZP(t + Dt,   X + K3*Dt)

    X  = X + (K1 + 2*K2 + 2*K3 + K4)*Dt/6
    t  = t + Dt

  END SUBROUTINE ropp_fm_runge_kutta

!-------------------------------------------------------------------------------
! 15. Splines: essentially a copy of the ropp_pp_spline module
!-------------------------------------------------------------------------------

! 15.1 Generate matrix of basic spline polynomials
! ------------------------------------------------

  SUBROUTINE ropp_fm_basic_splines(X, Xs, K)

    IMPLICIT NONE

    ! 15.1.1 Declarations

    REAL(wp), DIMENSION(:), INTENT(in)    :: X   ! Grid of argument Z
    REAL(wp), DIMENSION(:), INTENT(in)    :: Xs  ! X-grid of delta-splines
    REAL(wp), DIMENSION(:,:), INTENT(out) :: K   ! Matrix of basic polynomials

    INTEGER                               :: i   ! x index
    INTEGER                               :: j   ! Delta-spline number
    REAL(wp), DIMENSION(:), ALLOCATABLE   :: S   ! Delta-spline
    REAL(wp), DIMENSION(:), ALLOCATABLE   :: D2S ! Delta-spline 2nd derivative

    ALLOCATE(S(SIZE(xs)))
    ALLOCATE(D2S(SIZE(xs)))

    ! 15.1.2 Generate basic function matrix K

    S(:) = 0.0_wp

    DO j=1, SIZE(K,2)
       S(j) = 1.0_wp
       CALL ropp_fm_init_spline(Xs, S, D2S)
       DO i=1, SIZE(X)
          CALL ropp_fm_interpol_spline(Xs, S, D2S, X(i), K(i,j))
       ENDDO
       S(j) = 0.0_wp
    ENDDO

    DEALLOCATE(S)
    DEALLOCATE(D2S)

  END SUBROUTINE ropp_fm_basic_splines

! 15.2 Generate second derivative of spline
! -----------------------------------------

  SUBROUTINE ropp_fm_init_spline(x, f, d2)

    IMPLICIT NONE

    ! 15.2.1 Declarations

    REAL(wp), DIMENSION(:), INTENT(in)  :: x  ! Argument grid (monotonous)
    REAL(wp), DIMENSION(:), INTENT(in)  :: f  ! Gridded function
    REAL(wp), DIMENSION(:), INTENT(out) :: d2 ! 2nd derivative of spline

    REAL(wp), DIMENSION(:), ALLOCATABLE :: d1
    REAL(wp)                     :: dfl, dfr, df, a, b, c
    INTEGER                      :: i, N

    ! 15.2.2 Initialisation

    N = SIZE(x)
    ALLOCATE(d1(N))
    d2(:) = 0.0_wp

    ! 2.3 Drive-through calculation of spline coefficients

    d1(1) = 0.0_wp
    d2(N) = 0.0_wp
    d1(1) = 0.0_wp

    DO i = 2, N-1

       dfl   = (f(i) - f(i-1))/(x(i) - x(i-1))
       dfr   = (f(i+1) - f(i))/(x(i+1) - x(i))
       df    = (dfr - dfl)/(x(i+1) - x(i-1))
       a     = (x(i) - x(i-1))/(2*(x(i+1) - x(i-1)))
       b     = (x(i+1) - x(i))/(2*(x(i+1) - x(i-1)))
       c     = 1 + a*d1(i-1)
       d1(i) = -b/c
       d2(i) = (3*df - a*d2(i-1))/c
    ENDDO

    DO i = N-1, 2, -1
       d2(i) = d1(i)*d2(i+1) + d2(i)
    ENDDO

    DEALLOCATE(d1)

  END SUBROUTINE ropp_fm_init_spline

! 15.3 Return interpolated function and first two derivatives
! -----------------------------------------------------------

  SUBROUTINE ropp_fm_interpol_spline(x, f, d2, x_int, f_int, fd_int, fd2_int)

    IMPLICIT NONE

    ! 15.3.1 Declarations

    REAL(wp), DIMENSION(:), INTENT(in)  :: x  ! Argument grid (monotonous)
    REAL(wp), DIMENSION(:), INTENT(in)  :: f  ! Gridded function
    REAL(wp), DIMENSION(:), INTENT(in)  :: d2 ! 2nd derivative of spline
    REAL(wp),               INTENT(in)  :: x_int  ! Interpolation point
    REAL(wp),               INTENT(out) :: f_int  ! Interpolated function value
    REAL(wp), OPTIONAL,     INTENT(out) :: fd_int ! Interpolated 1st derivative
    REAL(wp), OPTIONAL,     INTENT(out) :: fd2_int ! Interpolated 2nd deriv

    REAL(wp) :: a1, a2, a3   ! Polynomial coefficients
    REAL(wp) :: dx           ! Grid interval
    REAL(wp) :: dx_t         ! Grid-point to interpolation-point distance
    REAL(wp) :: x_t          ! Interpolation point projected to grid extent
    REAL(wp) :: fd           ! Interpolated derivative
    INTEGER  :: i            ! Array index
    INTEGER  :: N            ! Number of data
    INTEGER  :: i_int        ! Interpolation interval index

    ! 15.3.2 Location of interpolation point inside grid

    N     = SIZE(x)

    x_t   = MIN(MAX(x_int, MIN(x(1),x(N))), MAX(x(1),x(N)))

    i_int = ropp_fm_seek_index(x, x_t)
    i     = MAX(i_int, 1)

    ! 15.3.3 Calculation of interpolation coefficients

    dx    = x(i+1) - x(i)
    a2    = d2(i)/2.0_wp
    a3    = (d2(i+1) - d2(i))/(6.0_wp*dx)
    a1    = (f(i+1) - f(i))/dx - dx*(a2 + dx*a3)

    ! 15.3.4 Calculated interpolated value

    dx_t  = x_t - x(i)
    fd    = a1 + dx_t*(2*a2 + dx_t*3*a3)
    f_int = f(i) + dx_t*(a1 + dx_t*(a2 + dx_t*a3)) + &
         fd*(x_int - x_t)

    IF (PRESENT(fd_int)) THEN
       fd_int = fd
    ENDIF

    IF (PRESENT(fd2_int)) THEN
       fd2_int  = 2.0_wp*a2 + dx_t*6.0_wp*a3
    ENDIF

  END SUBROUTINE ropp_fm_interpol_spline

! 15.4 Return index of point in grid
! ----------------------------------

  FUNCTION ropp_fm_seek_index(x, xp) RESULT(ip)

    IMPLICIT NONE

    REAL(wp), DIMENSION(:), INTENT(in) :: x   ! x-grid (homogeneous)
    REAL(wp),               INTENT(in) :: xp  ! point to locate inside grid
    INTEGER                            :: ip

    INTEGER  :: N      ! Number of grid points
    INTEGER  :: Imin   ! Upper estimate of index
    INTEGER  :: Imax   ! Lower estimate of index
    INTEGER  :: Dir    ! Direction of argument change
    INTEGER  :: It     ! Iteration count
    INTEGER  :: di     ! Index increment in iterations
    INTEGER  :: is     ! Step direction count

    ! 15.4.1 Grid size and direction calculation

    N    = SIZE(x)
    Dir  = NINT(SIGN(1.0_wp, x(N)-x(1)))

    ! 15.4.2 Checking if point is inside grid

    IF ((Dir*xp < Dir*x(1)) .OR. (Dir*xp > Dir*x(N))) THEN
       ip = 0
       RETURN
    END IF

    ! 15.4.3 Initial approximation

    Imin = 1
    Imax = N
    ip   = Imin+FLOOR(REAL(Imax - Imin, wp)*(xp - x(Imin))/(x(Imax) - x(Imin)))
    ip   = MAX(1, MIN(ip, N-1))

    ! 15.4.4 Iterative index search

    It = 0
    is = 0

    Search: DO
       IF ((Dir*x(ip) <= Dir*xp) .AND. (Dir*xp <= Dir*x(ip+1))) THEN
          EXIT Search
       END IF
       IF (ABS(is) > 1) THEN
          ip = (Imax + Imin)/2
          is = 0
       END IF
       IF (Dir*x(ip+1) < Dir*xp) THEN
          Imin = ip + 1
          di   = FLOOR(REAL(Imax - Imin, wp)*(xp - x(Imin))/(x(Imax) - x(Imin)))
          ip   = Imin + di
          is   = is + 1
       ELSE IF (Dir*xp < Dir*x(ip)) THEN
          Imax = ip
          di   = FLOOR(REAL(Imin - Imax, wp)*(xp - x(Imax))/(x(Imin) - x(Imax)))
          ip   = Imax + di
          is   = is - 1
       END IF
       ip = MAX(1, MIN(ip, N-1))
       It = It + 1
       IF (It > N) THEN
          ip = -1
          EXIT Search
       END IF
    END DO Search

  END FUNCTION ropp_fm_seek_index

END SUBROUTINE ropp_fm_tdry
