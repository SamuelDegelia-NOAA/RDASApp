! $Id$

module matrix_types

!****m* Matrices/matrix_types *
!
! NAME
!    matrix_types - Type declaration for positive definite matrices.
!
! SYNOPSIS
!    use matrix_types
! 
! DESCRIPTION
!    This module provides derived data types for matrices.
!
! NOTES
!    The main purpose of these matrix types is to be used with the matrix 
!    equation solving routines of this library for the respective matrix
!    types. The derived matrix types store decomposition information as
!    obtained by Lapack in addition to the actual matrix. For example, if
!    a positive definite packed matrix is used for the first time with
!    matrix_solve(), a Cholesky decomposition of the matrix is calculated
!    and used for the solving the given linear equation. The decomposition
!    is also stored in the same matrix data structure. In the next call
!    to matrix_solve, the Cholesky decomposition obtained previously is
!    reused, saving significant amounts of computations when solving
!    linear equations with many different right hand sides.
!
!    At present, only double precision matrices are supported. 
!
! SEE ALSO
!    matrix_ge
!    matrix_pp
!    matrix_pb
!    matrix_sq
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

  use typesizes, only: dp => EightByteReal

  implicit none

!--------------------------------------------------------------------------
! 1. General matrix
!--------------------------------------------------------------------------

  type matrix_ge
     real(dp), dimension(:,:), pointer :: d         => null()
     logical                           :: fact_chol = .false.
     character(len = 1)                :: equi_chol = 'N'
     real(dp), dimension(:,:), pointer :: e         => null()
     real(dp), dimension(:,:), pointer :: f         => null()
     real(dp), dimension(:),   pointer :: s         => null()
     logical                           :: fact_lu   = .false.
     character(len = 1)                :: equi_lu   = 'N'
     real(dp), dimension(:,:), pointer :: h         => null()
     integer,  dimension(:),   pointer :: i         => null()
     real(dp), dimension(:,:), pointer :: g         => null()
     real(dp), dimension(:),   pointer :: r         => null()
     real(dp), dimension(:),   pointer :: c         => null()
  end type matrix_ge

!--------------------------------------------------------------------------
! 2. Positive definite (packed) matrix
!--------------------------------------------------------------------------

  type matrix_pp
     real(dp), dimension(:), pointer :: d         => null()
     logical                         :: fact_chol = .false.
     character(len = 1)              :: equi_chol = 'N'
     real(dp), dimension(:), pointer :: e         => null()
     real(dp), dimension(:), pointer :: f         => null()
     real(dp), dimension(:), pointer :: s         => null()
  end type matrix_pp

!--------------------------------------------------------------------------
! 3. Positive definite band matrix
!--------------------------------------------------------------------------

  type matrix_pb
     real(dp), dimension(:,:), pointer :: d         => null()
     logical                           :: fact_chol = .false.
     character(len = 1)                :: equi_chol = 'N'
     real(dp), dimension(:,:), pointer :: e         => null()
     real(dp), dimension(:,:), pointer :: f         => null()
     real(dp), dimension(:),   pointer :: s         => null()
  end type matrix_pb

!--------------------------------------------------------------------------
! 4. Symmetric square root and its inverse
!--------------------------------------------------------------------------

  type matrix_sq
     real(dp), dimension(:,:), pointer :: L     => null()
     real(dp), dimension(:,:), pointer :: L_inv => null()
  end type matrix_sq

end module matrix_types
