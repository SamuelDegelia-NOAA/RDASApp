! $Id: vectors.f90 2019 2009-01-14 10:20:26Z frhl $

!****f* Coordinates/rotate
!
! NAME
!    rotate - Rotate a vector in cartesian coordinates around
!             a given axis by a given angle  
!
! SYNOPSIS
!    Rotate = rotate(X, A, phi)
! 
! DESCRIPTION
!    This function rotates a vector X around a given axis A by angle phi.
!       N*(N,X) + [N,X]*Sin(Phi) + (X-N*(N,X))*Cos(Phi),   where N=A/|A|
!
! INPUTS
!    X             Vector to rotate
!    A             Rotation axis
!    Phi           Rotation angle (rad)
!
! OUTPUT
!    Rotate        Rotated vector
!
! NOTES
!
! SEE ALSO
!
! REFERENCES
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

function rotate(X, A, Phi) result(R)

! 1.1 Declarations
! ----------------

  use typesizes, only: wp => EightByteReal
  use coordinates, only: vector_product

  implicit none

  real(wp), dimension(3), intent(in) :: X        ! input vector
  real(wp), dimension(3), intent(in) :: A        ! rotation axis
  real(wp),               intent(in) :: phi      ! rotation angle
  real(wp), dimension(3)             :: R        ! rotated vector

  real(wp), dimension(3) :: norm         ! normed rotation axis

! 1.2 Frame rotation
! ------------------

!   N*(N,X) + [N,X]*Sin(Phi) + (X-N*(N,X))*Cos(Phi),   where N=A/|A|

  norm = A(:)/Sqrt(Dot_Product(A(:), A(:)))
 
  R = norm*(Dot_Product(norm, X)) + vector_product(norm, X)*sin(phi)  &
       + (X - norm*Dot_Product(norm,X))*cos(phi)

end function rotate

!****f* Coordinates/vector_product
!
! NAME
!    vector_product - Compute a vector product of two cartesian vectors
!
! SYNOPSIS
!    product = vector_product(X, Y)
!
! INPUTS
!    X             Vector 1
!    Y             Vector 2
!
! OUTPUT
!    Product       Vector product
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

  function vector_product(X, Y) result(product)
    
    use typesizes,  only: wp => EightByteReal
    real(wp), dimension(3), intent(in) :: X
    real(wp), dimension(3), intent(in) :: Y
    real(wp), dimension(3)             :: product
    
    product = (/ X(2)*Y(3) - X(3)*Y(2),  &
                 X(3)*Y(1) - X(1)*Y(3),  &
                 X(1)*Y(2) - X(2)*Y(1) /)
    
  end function vector_product

!****f* Coordinates/vector_angle
!
! NAME
!    vector_angle - Find the angle between two cartesian vectors
!
! SYNOPSIS
!    angle = vector_angle(X, Y, A)
!
! INPUTS
!    X             Vector 1
!    Y             Vector 2
!    A             Orientation axis (optional)
!
! OUTPUT
!    Angle       Angle between vectors
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
  
function vector_angle(X, Y, A) result(angle)
  
  use typesizes,  only: wp => EightByteReal
  use coordinates, only: vector_product
  
  real(wp), dimension(3), intent(in) :: X
  real(wp), dimension(3), intent(in) :: Y
  real(wp), dimension(3), optional, intent(in) :: A
  real(wp)                           :: angle
  
  real(wp), dimension(3) :: n, alpha, beta, gamma
  real(wp)               :: nn
  
  if (present(A)) then
     n = A
  else
     n = vector_product(X, Y)
  endif
  
  nn = Dot_Product(n, n)

  if (nn == 0) then
     angle = 0.0_wp
  else
     n = n/sqrt(nn)
     alpha = vector_product(n, X)
     
     beta = X - Dot_Product(n, X) * n
     gamma = Y - Dot_Product(n, Y) * n
     angle = atan2(Dot_Product(alpha,gamma), Dot_Product(beta,gamma))
  endif
      
end function vector_angle
