! ===========================================================================
!
! This program reads in a HOHQMesh mesh file (ISM,ISM-V2, ISM-MM, ABAQUS) and
! writes out a BSC mesh file format (format used in DGSEM). 
! This program works for 2D meshes, a separate program was written
! for 3D meshes.
!
! Author: Zac Pyle
! Date: 12/18/2023
!
! ===========================================================================

PROGRAM TranslateMeshToBSC

    USE  MeshTranslator

    IMPLICIT NONE 

    ! Declare Variables 
    CHARACTER(LEN=20) :: meshFileToRead    ! path to mesh file you are converting
    CHARACTER(LEN=20) :: meshFileToWrite   ! path to mesh file you are creating
    CHARACTER(LEN=20) :: inputMeshType
    INTEGER           :: polyOrder
    INTEGER           :: inputFileUnit  = 1
    INTEGER           :: outputFileUnit = 2
    !REAL              :: x, y, z
    REAL, DIMENSION(:,:), ALLOCATABLE      :: nodeArray
    INTEGER, DIMENSION(:,:), ALLOCATABLE   :: elArray, BCArray



    ! --------------------------------------------------------------

    ! Get file name from user input when calling this program
    CALL getarg(1,meshFileToRead)

    ! Get file name for the output file
    CALL getarg(2,meshFileToWrite)

    ! Terminal line message for user
    WRITE(*,*) "Translating ", meshFileToRead, " to ", meshFileToWrite

    ! Open input and output files
    OPEN(UNIT = inputFileUnit, FILE = meshFileToRead, STATUS = "OLD")
    OPEN(UNIT = outputFileUnit, FILE = meshFileToWrite, STATUS = "NEW")

    ! REPLACE THIS SECTION WITH A SUBROUTINE THAT DETERMINES THE INPUT FILE
    ! FORMAT, PREFERABLY FROM THE FILE EXTENSION OR A HEADER WITHIN THE FILE
    ! FOR NOW JUST TEST EACH FILE TYPE INDIVIDUALLY
    CALL DetermineInputMeshType(inputFileUnit,inputMeshType)

    inputMeshType = TRIM(inputMeshType)
    ! Debug statement 
    WRITE(*,*) inputMeshType

    ! Call appropriate translator
    SELECT CASE (inputMeshType)
        CASE ('ISM') 
            CALL ReadHOHQMeshFile_ISM(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)

        CASE ('ISM-V2') 
            CALL ReadHOHQMeshFile_ISMV2(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)

        CASE ('ISM-MM') 
            CALL ReadHOHQMeshFile_ISMMM(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)

        CASE ('ABAQUS') 
            CALL ReadHOHQMeshFile_ABAQUS(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)

        CASE ('NEU') 
            CALL ReadPointwiseMeshFile_NEU(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)

        CASE ('BSC') 
            WRITE(*,*) 'Input mesh file already in BSC format, no need to translate!'

    END SELECT

    IF (inputMeshType /= 'BSC') THEN
        CALL WriteBSCMeshFile(outputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ENDIF

    ! Close input and output files
    CLOSE(inputFileUnit)
    CLOSE(outputFileUnit)

END PROGRAM TranslateMeshToBSC
