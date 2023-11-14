! ===========================================================================
!
! This program reads in a Gambit mesh file (.neu format) and
! writes out a BSC mesh file format (format used in DGSEM). 
! This program works for 2D meshes, a separate program was written
! for 3D meshes.
!
! Author: Zac Pyle
! Date: 11/13/2023
!
! ===========================================================================

! Declare local variables
CHARACTER(LEN=20) :: meshFileToRead    ! path to mesh file you are converting
CHARACTER(LEN=20) :: meshFileToWrite    ! path to mesh file you are creating
CHARACTER(LEN=20) :: format1, format2
INTEGER           :: ios, nodeNum, elNum, totalNodeNum, totalElNum, totalBCNum, i, j
INTEGER           :: NGRPS  ! will not use this variable, but it needs to be read in
INTEGER           :: NEUUnit = 1
INTEGER           :: BSCUnit = 2
REAL              :: x, y, z
REAL, DIMENSION(:,:), ALLOCATABLE   :: nodeArray
REAL, DIMENSION(:,:), ALLOCATABLE   :: elementArray

! writing formats
format1 = "(99(2X,E14.7))"
format2 = "(4(1X,F14.7))"

z = 0.0

! Get file name from user input when calling this program
CALL getarg(1,meshFileToRead)

! Get file name for the output file
CALL getarg(2,meshFileToWrite)

! Terminal line message for user
WRITE(*,*) "Converinting ", meshFileToRead, " to ", meshFileToWrite

! Open neu and bsc files
OPEN(UNIT = NEUUnit, FILE = meshFileToRead, STATUS = "OLD")
OPEN(UNIT = BSCUnit, FILE = meshFileToWrite, STATUS = "NEW")

! Write the header for the bsc file
WRITE(BSCUnit,*) "simple mesh format version = 1.1"
WRITE(BSCUnit,*) "nodes"

! Read in the first couple of lines from the .neu file; they're just header info
ios = 0   ! if 0 the file is read successfully, of non-zero there was an error
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )

! Get the total element, node, and BC counts
READ(NEUUnit, *, IOSTAT = ios ) totalNodeNum, totalElNum, NGRPS, totalBCNum

! Allocate memory to node and element arrays
ALLOCATE( nodeArray(totalNodeNum, 4))
ALLOCATE( elementArray(totalElNum, 5))

! Skip the next two lines
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )

! Loop through all the nodes, saving them to nodeArray and writing them to the bsc file
DO i = 1,totalNodeNum
    ! Read in nodal data
    READ(NEUUnit, *, IOSTAT = ios) nodeNum, x, y

    ! Save nodal data to an array
    nodeArray(i,1) = nodeNum
    nodeArray(i,2) = x 
    nodeArray(i,3) = y 
    nodeArray(i,4) = z 

    ! Write nodal information to bsc file
    !WRITE(BSCUnit,format2) nodeArray(i,:)
    WRITE(BSCUnit,format2) nodeArray(i,:)
ENDDO

! End node section and begin element section
WRITE(BSCUnit,*) "end nodes"
WRITE(BSCUnit,*) "elements"

! Close neu and bsc files
CLOSE(NEUUnit)
CLOSE(BSCUnit)

END