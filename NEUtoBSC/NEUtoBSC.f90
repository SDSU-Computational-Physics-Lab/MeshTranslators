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
CHARACTER(LEN=20) :: formatNodes, formatEl1, formatEl2, formatBC
CHARACTER(LEN=12) :: tempRead
CHARACTER(LEN=20) :: BCNameNeu
INTEGER           :: ios, nodeNum, elNum, totalNodeNum, totalElNum, totalBCNum, i, j
INTEGER           :: elType, topLeftNode, botLeftNode, botRightNode, topRightNode
INTEGER           :: DUMMY  ! will not use this variable, but it needs to be read in
INTEGER           :: BCFaceUserInput
INTEGER           :: polyX, polyY, polyZ, NGRPS
INTEGER           :: NEUUnit = 1
INTEGER           :: BSCUnit = 2
REAL              :: x, y, z
REAL, DIMENSION(:,:), ALLOCATABLE   :: nodeArray
REAL, DIMENSION(:,:), ALLOCATABLE   :: elementArray

! writing formats
formatNodes = "(I7,3F19.14)"
formatEl1   = "(I7,A7,5(I7))"
formatEl2   = "(4(I7))"
formatBC    = "(A7,2(I12))"

! This is a 2D translator, but we mesh format includes z coordinate
z = 0.0

! Set polynomial orders for BSC mesh; although typically this is 
! overwritten by the control file. Still need it for formatting
polyX = 3
polyY = 3
polyZ = 3

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
WRITE(BSCUnit,"(32A)") "simple mesh format version = 1.1"

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

! Translate nodal information into BSC format -------------------------------------

! Indicate we are in the "nodes" section of BSC file
WRITE(BSCUnit,"(5A)") "nodes"

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
    !WRITE(BSCUnit,"(I3)",ADVANCE='NO') nodeArray(i,1)
    WRITE(BSCUnit,formatNodes) nodeNum, x, y, z

ENDDO

! End node section
WRITE(BSCUnit,"(9A)") "end nodes"

! Translate element information into BSC format -------------------------------------

! Indicate we are in the "elements" section of BSC file
WRITE(BSCUnit,"(8A)") "elements"

! Read in the next two lines of the NEU file to skip them; they're headers
READ(NEUUnit, *, IOSTAT = ios )
READ(NEUUnit, *, IOSTAT = ios )

! Loop through all elements, save to elementArray, write to BSC file
DO i = 1,totalElNum
    ! Read in element data
    READ(NEUUnit, *, IOSTAT = ios) elNum, elType, DUMMY, botLeftNode, botRightNode, topRightNode, topLeftNode

    ! Save element data to array
    elementArray(i,1) = elNum
    elementArray(i,2) = botLeftNode
    elementArray(i,3) = botRightNode
    elementArray(i,4) = topRightNode
    elementArray(i,5) = topLeftNode

    ! Write element data to BSC file
    WRITE(BSCUnit, formatEl1) elNum, "quad", 4, 0, polyX, polyY, polyZ
    WRITE(BSCUnit, formatEl2) botLeftNode, botRightNode, topRightNode, topLeftNode
ENDDO

! End element section
WRITE(BSCUnit, "(A12)") "end elements"
READ(NEUUnit, *, IOSTAT = ios )

! Keep reading lines until you reach the boundary section -------------------------------
READ(NEUUnit,*) tempRead
DO WHILE (tempRead /= 'BOUNDARY')
    READ(NEUUnit,*) tempRead
END DO

! Translate boundary information into BSC format -------------------------------------

! Indicate we are in the "boundary" section of BSC file
WRITE(BSCUnit,"(A8)") "boundary"

! Loop through each boundary condition
DO i = 1,totalBCNum
    ! Get the name of the BC from NEU file
    READ(NEUUnit,*,IOSTAT = ios) BCNameNeu

    ! Get user input on what the BC face is for this BC type; will be applied to ALL elements
    WRITE(*,*) "NEU mesh file BC name: ", BCNameNeu
    WRITE(*,*) "    What element face will this be applied to? (1-4):  "
    READ *, BCFaceUserInput

    ! Loop through each element contained in this group of BCs, write to BSC file
    READ(NEUUnit,*,IOSTAT = ios) tempRead
    DO WHILE (tempRead /= 'ENDOFSECTION')
        WRITE(BSCUnit,formatBC) tempRead, BCFaceUserInput, i  
        READ(NEUUnit,*,IOSTAT = ios) tempRead     
    ENDDO

    ! Skip line and go into next BC section
    READ(NEUUnit,*, IOSTAT = ios) tempRead
ENDDO

WRITE(BSCUnit,"(A12)") "end boundary"

! NOTE: This does NOT arrange the elements in any particular order in the output file; 
! I'm not sure if that will cause problems in the mesh reader. I don't think it will,
! but if it does I'll need to figure out some way to allocate memory for a BCArray 
! and order that according to elNum, then BCFaceUserInput. Maybe allocate it for the 
! number of elements, fill with 0's, overwrite rows that actually have values, 
! delete rows that have 0's?

! Finish and close ----------------------------------------------------
DEALLOCATE(nodeArray)
DEALLOCATE(elementArray)

! Close neu and bsc files
CLOSE(NEUUnit)
CLOSE(BSCUnit)

END