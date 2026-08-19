!****m* ropp_utils/system
!
! NAME
!    system - System utilities
!
! SYNOPSIS
!    use system
!
! DESCRIPTION
!   The system module provides operating system for which no Fortran standard
!   exists. Most functions are ISO_C_BINDINGs into the Unix libc.
!
! FUNCTIONS
!   setenv()  -  Set an environment variable.
!
!****

MODULE system

!------------------------------------------------------------------------------
! 1. setenv()
!------------------------------------------------------------------------------

!****s* system/setenv
!
! NAME
!    setenv - Set an environment variable
!
! SYNOPSIS
!    status = setenv(name, value, overwrite)
!
! DESCRIPTION
!   The setenv() function inserts or resets the environment variable name 
!   in the current environment list.  If the variable name does not exist
!   in the list, it is inserted with the given value.  If the variable does 
!   exist, the argument overwrite is tested; if overwrite is zero, the
!   variable is not reset, otherwise it is reset to the given value.
!
! INPUTS
!   name       -  Name of the environment variable (zero-terminated string).
!   value      -  Value to be used (zero-terminated string).
!   overwrite  -  If 1, an existing environment variable will be overwritten.
!
! OUTPUT
!   status     - 0 if successful, -1 otherwise.
!
! NOTES
!   The strings passed into the setenv() function for both the environment 
!   variable name and the value it shall be set to must be zero-terminated; 
!   see the example below.
!
! EXAMPLE
!   USE system
!   INTEGER :: istat
!
!   istat = setenv('ECCODES_BUFR_SET_TO_MISSING_IF_OUT_OF_RANGE'//ACHAR(0), &
!                  '1'//ACHAR(0), 1)
!
! REFERENCE
!   The original code appeared in
!      https://software.intel.com/en-us/forums/intel-fortran-compiler/topic/594761
!
!****

   INTERFACE
      FUNCTION setenv(name, value, overwrite) bind(C, name = 'setenv')
         USE ISO_C_BINDING
         IMPLICIT NONE
         INTEGER(C_INT)                       :: setenv
         CHARACTER(KIND = C_CHAR), INTENT(in) :: name(*)
         CHARACTER(KIND = C_CHAR), INTENT(in) :: value(*)
         INTEGER(C_INT), value                :: overwrite
      END FUNCTION setenv
   END INTERFACE
   
END MODULE system
