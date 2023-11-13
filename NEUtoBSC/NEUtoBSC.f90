! ===========================================================================
!
! This program reads in a Gambit mesh file (.neu format) and
! writes out a BSC mesh file format (format used in DGSEM)
!
! Author: Zac Pyle
! Date: 11/13/2023
!
! ===========================================================================

! Declare local variables
CHARACTER(LEN=40) :: meshFileToRead    ! path to mesh file you are converting
CHARACTER(LEN=40) :: meshFileToWrite    ! path to mesh file you are creating


! Get file name from user input when calling this program
CALL getarg(1,meshFileToRead)

! Create file name for the output file


WRITE(*,*) "Converinting "meshFileToRead " to "

END