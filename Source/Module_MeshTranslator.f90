! ===========================================================================
!
! This module containes subroutines for translating reads in a HOHQMesh mesh 
! file formats (ISM,ISM-V2, ISM-MM, ABAQUS) and Gambit file format (.neu)
! into a BSC mesh file format (format used in DGSEM). 
! This program works for 2D meshes, a separate program was written
! for 3D meshes.
!
! Author: Zac Pyle
! Date: 12/18/2023
!
! ===========================================================================

MODULE MeshTranslator

    IMPLICIT NONE

    ! Declare variables common to all subroutines

    CONTAINS

    SUBROUTINE DetermineInputMeshType(inputFileUnit,inputMeshType)
    ! This subroutine reads the header of an input mesh file and determines if it is
    ! a: BSC, ISM, ISM-V2, ISM-MM, ABAQUS, or NEU format. NOTE: the default assumption is 
    ! an ISM file type.
    
        IMPLICIT NONE 

        ! Declare input variables 
        INTEGER, INTENT(IN)              :: inputFileUnit 
        CHARACTER(LEN=20), INTENT(INOUT) :: inputMeshType

        ! Declare local variables 
        INTEGER           :: ios
        CHARACTER(LEN=40) :: readLine
        INTEGER           :: NUMNP, NELEM, NGRPS, NBSETS, NDFCD, NDFVL

        ! Read in the first line of the mesh file 
        READ(inputFileUnit, *, IOSTAT=ios) readLine

        ! Debug statement
        !WRITE(*,*) readLine 

        ! compare the header to known headers of the format 
        readLine = TRIM(readLine)
        IF (readLine == 'ISM-V2') THEN
            ! Check if this is a 2D or 3D mesh file
            inputMeshType = 'ISM-V2_2D'

        ELSEIF (readLine == 'ISM-MM') THEN 
            ! Check if this is a 2D or 3D mesh file
            inputMeshType = 'ISM-MM_2D'

        ELSEIF (readLine == '*Heading') THEN
            ! Check if this is a 2D or 3D mesh file
            inputMeshType = 'ABAQUS_2D'

        ELSEIF (readLine == 'CONTROL') THEN 

            ! Check if this is a 2D or 3D mesh file
            READ(inputFileUnit, *, IOSTAT = ios) readLine
            READ(inputFileUnit, *, IOSTAT = ios) readLine
            READ(inputFileUnit, *, IOSTAT = ios) readLine
            READ(inputFileUnit, *, IOSTAT = ios) readLine
            READ(inputFileUnit, *, IOSTAT = ios) readLine
            READ(inputFileUnit, *, IOSTAT = ios) NUMNP, NELEM, NGRPS, NBSETS, NDFCD, NDFVL
            
            IF (NDFCD == 2) THEN
                inputMeshType = 'NEU_2D' 
            ELSEIF (NDFCD == 3) THEN 
                inputMeshType = 'NEU_3D' 
            ELSE
                WRITE(*,*) "ERROR: Unable to determine if this is an NEU_2D or NEU_3D file!"
                WRITE(*,*) "ERROR: Aborting mesh translation"
                RETURN
            ENDIF


            BACKSPACE(inputFileUnit)
            BACKSPACE(inputFileUnit)
            BACKSPACE(inputFileUnit)
            BACKSPACE(inputFileUnit)
            BACKSPACE(inputFileUnit)
            BACKSPACE(inputFileUnit)

        ELSEIF (readLine == 'simple') THEN 
            ! Check if this is a 2D or 3D mesh file
            inputMeshType = 'BSC'

        ELSE 
            ! Check if this is a 2D or 3D mesh file
            inputMeshType = 'ISM_2D'

        ENDIF

        ! Debug statement 
        !WRITE(*,*) inputMeshType

        ! Back up to the beginning of the file
        BACKSPACE(inputFileUnit) 


    END SUBROUTINE DetermineInputMeshType

! =====================================================================================

    SUBROUTINE WriteBSCMeshFile_2D(outputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine writes a BSC mesh file, given an array containing the nodal, 
    ! element, and boundary information. This information may be obtained using the
    ! "Read*****MeshFile_***" subroutines found below

        IMPLICIT NONE 

        ! Declare input variables 
        INTEGER,                 INTENT(IN) :: outputFileUnit, polyOrder 
        REAL,    DIMENSION(:,:), INTENT(IN) :: nodeArray
        INTEGER, DIMENSION(:,:), INTENT(IN) :: elArray 
        INTEGER, DIMENSION(:,:), INTENT(IN) :: BCArray 

        ! Declare local variables
        CHARACTER(LEN=20) :: formatNodes, formatEl1, formatEl2, formatBC
        INTEGER           :: i

        ! writing formats
        formatNodes = "(I7,3F19.14)"
        formatEl1   = "(I7,A7,5(I7))"
        formatEl2   = "(4(I7))"
        formatBC    = "(3(I6))"

        ! Write the header for the bsc file
        WRITE(outputFileUnit,"(32A)") "simple mesh format version = 1.1"

        ! Write nodal info
        WRITE(outputFileUnit,"(5A)") "nodes"
        DO i = 1,SIZE(nodeArray(:,1))
            WRITE(outputFileUnit,formatNodes) INT(nodeArray(i,1)), nodeArray(i,2:SIZE(nodeArray(i,:)))
        END DO
        WRITE(outputFileUnit,"(9A)") "end nodes"

        ! Write element info
        WRITE(outputFileUnit,"(8A)") "elements"
        DO i = 1,SIZE(elArray(:,1))
            WRITE(outputFileUnit, formatEl1) i, "quad", 4, 0, polyOrder, polyOrder, polyOrder
            WRITE(outputFileUnit, formatEl2) INT(elArray(i,2:SIZE(elArray(i,:))))
        END DO
        WRITE(outputFileUnit, "(A12)") "end elements"

        ! Write BC info
        WRITE(outputFileUnit,"(A8)") "boundary"
        DO i = 1,SIZE(BCArray(:,1))
            WRITE(outputFileUnit, formatBC) INT(BCArray(i,:))
        END DO
        WRITE(outputFileUnit,"(A12)") "end boundary"

    END SUBROUTINE WriteBSCMeshFile_2D

! ================================================================================

    SUBROUTINE WriteBSCMeshFile_3D(outputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine writes a BSC mesh file, given an array containing the nodal, 
    ! element, and boundary information. This information may be obtained using the
    ! "Read*****MeshFile_***" subroutines found below

        IMPLICIT NONE 

        ! Declare input variables 
        INTEGER,                 INTENT(IN) :: outputFileUnit, polyOrder 
        REAL,    DIMENSION(:,:), INTENT(IN) :: nodeArray
        INTEGER, DIMENSION(:,:), INTENT(IN) :: elArray 
        INTEGER, DIMENSION(:,:), INTENT(IN) :: BCArray 

        ! Declare local variables
        CHARACTER(LEN=20) :: formatNodes, formatEl1, formatEl2, formatBC
        INTEGER           :: i

        ! writing formats
        formatNodes = "(I7,3F19.14)"
        formatEl1   = "(I7,A7,5(I7))"
        formatEl2   = "(8(I7))"
        formatBC    = "(3(I6))"

        ! Write the header for the bsc file
        WRITE(outputFileUnit,"(32A)") "simple mesh format version = 1.1"

        ! Write nodal info
        WRITE(outputFileUnit,"(5A)") "nodes"
        DO i = 1,SIZE(nodeArray(:,1))
            WRITE(outputFileUnit,formatNodes) INT(nodeArray(i,1)), nodeArray(i,2:SIZE(nodeArray(i,:)))
        END DO
        WRITE(outputFileUnit,"(9A)") "end nodes"

        ! Write element info
        WRITE(outputFileUnit,"(8A)") "elements"
        DO i = 1,SIZE(elArray(:,1))
            WRITE(outputFileUnit, formatEl1) i, "hex", 8, 0, polyOrder, polyOrder, polyOrder
            WRITE(outputFileUnit, formatEl2) INT(elArray(i,2:SIZE(elArray(i,:))))
        END DO
        WRITE(outputFileUnit, "(A12)") "end elements"

        ! Write BC info
        WRITE(outputFileUnit,"(A8)") "boundary"
        DO i = 1,SIZE(BCArray(:,1))
            WRITE(outputFileUnit, formatBC) INT(BCArray(i,:))
        END DO
        WRITE(outputFileUnit,"(A12)") "end boundary"

    END SUBROUTINE WriteBSCMeshFile_3D

! =============================================================================================    

    SUBROUTINE ReadPointwiseMeshFile_NEU_2D(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine reads a NEU format mesh file and returns 3 arrays: nodeArray,
    ! elArray, and BCArray. These contain the node, element, and BC data 
    ! necessary to write a BSC format mesh file. For reference, the NEU format is used
    ! by the software GAMBIT.

        IMPLICIT NONE 

        ! Declare input variables
        ! Declare input variables
        INTEGER, INTENT(IN) :: inputFileUnit
        REAL,    DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: nodeArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: elArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: BCArray
        INTEGER,                              INTENT(OUT)   :: polyOrder

        ! Declare local variables
        REAL              :: x, y, z
        INTEGER           :: totalNodeNum, totalElNum, totalBCNum
        INTEGER           :: nodeNum, node1, node2, node3, node4
        INTEGER           :: elNum, elType, BCFace
        INTEGER           :: i, ios
        INTEGER           :: NGRPS, dummy, count
        CHARACTER(LEN=35) :: readLine
        CHARACTER(LEN=20) :: BCNameNeu

        ! NEU mesh files don't include a poly order or a z coordinate for 2D meshes,
        ! so just assign them here. polyOrder is unimportant because it is assigned
        ! in the .control file anyway
        z         = 0.0
        polyOrder = 4

        ! Terminal message for user
        WRITE(*,*) "Format: NEU_2D --> BSC_2D"

        ! Read in the first couple of lines from the .neu file; they're just header info
        ios = 0   ! if 0 the file is read successfully, of non-zero there was an error
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )

        ! Get the total element, node, and BC counts
        READ(inputFileUnit, *, IOSTAT = ios ) totalNodeNum, totalElNum, NGRPS, totalBCNum

        ! Allocate memory to node and element arrays
        ALLOCATE( nodeArray(totalNodeNum, 4))
        ALLOCATE( elArray(totalElNum, 5))

        ! Allocate the "worst case scenario" of BC's: each element face has
        ! a different BC type
        ALLOCATE( BCArray(totalElNum*4,3) )

        ! Skip the next two lines
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )

        ! Terminal message for user
        WRITE(*,*) "Nodes: ", totalNodeNum, "  Elements: ", totalElNum, "  polyOrder: ", polyOrder

        ! Loop through all the nodes, saving them to nodeArray and writing them to the bsc file
        DO i = 1,totalNodeNum
            ! Read in nodal data
            READ(inputFileUnit, *, IOSTAT = ios) nodeNum, x, y

            ! Save nodal data to an array
            nodeArray(i,1) = nodeNum
            nodeArray(i,2) = x 
            nodeArray(i,3) = y 
            nodeArray(i,4) = z 

            ! Debug statement
            !WRITE(*,*) nodeArray(i,:)
        ENDDO

        ! Read in the next two lines of the NEU file to skip them; they're headers
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )

        ! Loop through all elements, save to elArray, write to BSC file
        DO i = 1,totalElNum
            ! Read in element data
            READ(inputFileUnit, *, IOSTAT = ios) elNum, elType, dummy, node1, node2, node3, node4

            ! Save element data to array
            elArray(i,1) = elNum
            elArray(i,2) = node1
            elArray(i,3) = node2
            elArray(i,4) = node3
            elArray(i,5) = node4

            ! Debug statement
            !WRITE(*,*) elArray(i,:)
        ENDDO

        ! Keep reading lines until you reach the boundary section -------------------------------
        READ(inputFileUnit,*) readLine
        DO WHILE (readLine /= 'BOUNDARY')
            READ(inputFileUnit,*) readLine
        END DO

        ! Loop through each boundary condition
        count = 0
        DO i = 1,totalBCNum
            ! Get the name of the BC from NEU file
            READ(inputFileUnit,*,IOSTAT = ios) BCNameNeu

            ! Write out the name of the BC from the Gambit file and the BCID it's being mapped to in the BSC file
            ! for the user's convenience
            WRITE(*,*) "BC Name:  ", BCNameNeu, "-->   BCNum:  ", i

            ! Loop through each element contained in this group of BCs, write to BSC file
            READ(inputFileUnit,*,IOSTAT = ios) readLine
            DO WHILE (readLine /= 'ENDOFSECTION')
                count = count + 1

                ! re-read the line, saving data to proper variables
                BACKSPACE(inputFileUnit)
                READ(inputFileUnit, *, IOSTAT = ios) elNum, dummy, BCFace

                ! Debug statement
                !WRITE(*,*) elNum, BCFace, i  

                ! Save to BCArray
                BCArray(count,1) = elNum
                BCArray(count,2) = BCFace 
                BCArray(count,3) = i 

                ! Debug statement
                !WRITE(*,*) BCArray(count,:)

                READ(inputFileUnit,*,IOSTAT = ios) readLine     
            ENDDO

            ! Skip line and go into next BC section
            READ(inputFileUnit,*, IOSTAT = ios) readLine
        ENDDO

        ! Trim the BC array as needed
        BCArray = BCArray(1:count,:)

        ! Debug statement
        DO i = 1,size(BCArray(:,1))
            !WRITE(*,*) BCArray(i,:)
        END DO 

    END SUBROUTINE ReadPointwiseMeshFile_NEU_2D
! ================================================================================

    SUBROUTINE ReadPointwiseMeshFile_NEU_3D(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine reads a NEU format mesh file and returns 3 arrays: nodeArray,
    ! elArray, and BCArray. These contain the node, element, and BC data 
    ! necessary to write a BSC format mesh file. For reference, the NEU format is used
    ! by the software GAMBIT.

        IMPLICIT NONE 

        ! Declare input variables
        ! Declare input variables
        INTEGER, INTENT(IN) :: inputFileUnit
        REAL,    DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: nodeArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: elArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: BCArray
        INTEGER,                              INTENT(OUT)   :: polyOrder

        ! Declare local variables
        REAL              :: x, y, z
        INTEGER           :: totalNodeNum, totalElNum, totalBCNum
        INTEGER           :: nodeNum, node1, node2, node3, node4, &
                                      node5, node6, node7, node8
        INTEGER           :: elNum, elType, BCFace
        INTEGER           :: i, ios
        INTEGER           :: NGRPS, dummy, count
        CHARACTER(LEN=35) :: readLine
        CHARACTER(LEN=20) :: BCNameNeu

        ! NEU mesh files don't include a poly order so just assign here. 
        ! polyOrder is unimportant because it is assigned in the .control file anyway
        polyOrder = 4

        ! Terminal message for user
        WRITE(*,*) "Format: NEU_3D --> BSC_3D"

        ! Read in the first couple of lines from the .neu file; they're just header info
        ios = 0   ! if 0 the file is read successfully, of non-zero there was an error
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )

        ! Get the total element, node, and BC counts
        READ(inputFileUnit, *, IOSTAT = ios ) totalNodeNum, totalElNum, NGRPS, totalBCNum

        ! Allocate memory to node and element arrays
        ALLOCATE( nodeArray(totalNodeNum, 4))
        ALLOCATE( elArray(totalElNum, 9))

        ! Allocate the "worst case scenario" of BC's: each element face has
        ! a different BC type
        ALLOCATE( BCArray(totalElNum*6,3) )

        ! Skip the next two lines
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )

        ! Terminal message for user
        WRITE(*,*) "Nodes: ", totalNodeNum, "  Elements: ", totalElNum, "  polyOrder: ", polyOrder

        ! Loop through all the nodes, saving them to nodeArray and writing them to the bsc file
        DO i = 1,totalNodeNum
            ! Read in nodal data
            READ(inputFileUnit, *, IOSTAT = ios) nodeNum, x, y, z

            ! Save nodal data to an array
            nodeArray(i,1) = nodeNum
            nodeArray(i,2) = x 
            nodeArray(i,3) = y 
            nodeArray(i,4) = z 

            ! Debug statement
            !WRITE(*,*) nodeArray(i,:)
        ENDDO

        ! Read in the next two lines of the NEU file to skip them; they're headers
        READ(inputFileUnit, *, IOSTAT = ios )
        READ(inputFileUnit, *, IOSTAT = ios )

        ! Loop through all elements, save to elArray, write to BSC file
        DO i = 1,totalElNum
            ! Read in element data
            READ(inputFileUnit, *, IOSTAT = ios) elNum, elType, dummy, node1, node2, node4, node3, &
                                                                       node5, node6, node8, node7

            ! Save element data to array
            elArray(i,1) = elNum
            elArray(i,2) = node1
            elArray(i,3) = node2
            elArray(i,4) = node3
            elArray(i,5) = node4
            elArray(i,6) = node5
            elArray(i,7) = node6
            elArray(i,8) = node7
            elArray(i,9) = node8

            ! Debug statement
            !WRITE(*,*) elArray(i,:)
        ENDDO

        ! Keep reading lines until you reach the boundary section -------------------------------
        READ(inputFileUnit,*) readLine
        DO WHILE (readLine /= 'BOUNDARY')
            READ(inputFileUnit,*) readLine
        END DO

        ! Loop through each boundary condition
        count = 0
        DO i = 1,totalBCNum
            ! Get the name of the BC from NEU file
            READ(inputFileUnit,*,IOSTAT = ios) BCNameNeu

            ! Write out the name of the BC from the Gambit file and the BCID it's being mapped to in the BSC file
            ! for the user's convenience
            WRITE(*,*) "BC Name:  ", BCNameNeu, "-->   BCNum:  ", i

            ! Loop through each element contained in this group of BCs, write to BSC file
            READ(inputFileUnit,*,IOSTAT = ios) readLine
            DO WHILE (readLine /= 'ENDOFSECTION')
                count = count + 1

                ! re-read the line, saving data to proper variables
                BACKSPACE(inputFileUnit)
                READ(inputFileUnit, *, IOSTAT = ios) elNum, dummy, BCFace

                ! Remap the BCFace; NEU and BSC use different conventions for labeling the faces of
                ! hex elements
                SELECT CASE (BCFace)
                    CASE (1)
                        BCFace = 1
                    CASE (2) 
                        BCFace = 4
                    CASE (3) 
                        BCFace = 2
                    CASE (4) 
                        BCFace = 6
                    CASE (5) 
                        BCFace = 3
                    CASE (6)
                        BCFace = 5

                END SELECT

                ! Debug statement
                !WRITE(*,*) elNum, BCFace, i  

                ! Save to BCArray
                BCArray(count,1) = elNum
                BCArray(count,2) = BCFace 
                BCArray(count,3) = i 

                ! Debug statement
                !WRITE(*,*) BCArray(count,:)

                READ(inputFileUnit,*,IOSTAT = ios) readLine     
            ENDDO

            ! Skip line and go into next BC section
            READ(inputFileUnit,*, IOSTAT = ios) readLine
        ENDDO

        ! Trim the BC array as needed
        BCArray = BCArray(1:count,:)

        ! Debug statement
        DO i = 1,size(BCArray(:,1))
            !WRITE(*,*) BCArray(i,:)
        END DO 

    END SUBROUTINE ReadPointwiseMeshFile_NEU_3D

! ================================================================================

    SUBROUTINE ReadHOHQMeshFile_ISM_2D(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine reads an ISM format mesh file and returns 3 arrays: nodeArray,
    ! elArray, and BCArray. These contain the node, element, and BC data 
    ! necessary to write a BSC format mesh file.

        IMPLICIT NONE 

        ! Declare input variables
        INTEGER, INTENT(IN) :: inputFileUnit
        REAL,    DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: nodeArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: elArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: BCArray
        INTEGER,                              INTENT(OUT)   :: polyOrder

        ! Declare local variables
        REAL              :: x, y, z
        INTEGER           :: totalNodeNum, totalElNum, totalBCNum
        INTEGER           :: node1, node2, node3, node4
        INTEGER           :: edge1Type, edge2Type, edge3Type, edge4Type
        INTEGER           :: i, j, k, ios
        INTEGER           :: dummy, count
        CHARACTER(LEN=35) :: readLine
        CHARACTER(LEN=20) :: BC(4)
        CHARACTER(LEN=20), DIMENSION(:,:), ALLOCATABLE :: AllBCArray
        CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: BCNameArray
        REAL, DIMENSION(:), ALLOCATABLE      :: BCIDArray

        ! Terminal message for user
        WRITE(*,*) "Format: ISM_2D --> BSC_2D"

        ! Read in node / el / type information
        READ(inputFileUnit,*,IOSTAT=ios) totalNodeNum, totalElNum, polyOrder
        WRITE(*,*) "Nodes: ", totalNodeNum, "  Elements: ", totalElNum, "  polyOrder: ", polyOrder

        ! Allocate memory to node and element arrays
        ALLOCATE( nodeArray(totalNodeNum, 4))
        ALLOCATE( elArray(totalElNum, 5))
        ALLOCATE( AllBCArray(totalElNum, 4))

        ! Allocate the "worst case scenario" of BC's: each element face has
        ! a different BC type
        ALLOCATE( BCArray(totalElNum*4,3) )
        ALLOCATE( BCNameArray(totalElNum*4) )
        ALLOCATE( BCIDArray(totalElNum*4) )

        ! Loop through all the nodes, saving them to nodeArray
        DO i = 1, totalNodeNum
            ! Read in nodal data
            READ(inputFileUnit, *, IOSTAT=ios) x, y, z

            ! Save nodal data to an array
            nodeArray(i,1) = i
            nodeArray(i,2) = x 
            nodeArray(i,3) = y 
            nodeArray(i,4) = z 

            ! Debug statement
            !WRITE(*,*) i, x, y, z
        ENDDO

        ! Loop through all the elements, saving data into elArray and BCArray
        count = 0  ! initialize for use in BC processing
        DO i = 1, totalElNum
            ! There may be multiple lines of info if the elements have curved boundaries,
            ! keep reading until we hit the BC info but only save the first read in line.
            ! Keep track of what line was read in first with "dummy" variable
            dummy = 0
            DO
                ! Save the nodes that make up the element
                READ(inputFileUnit, *, IOSTAT=ios) node1, node2, node3, node4

                ! Save the first read in line of this element section to elArray
                IF (dummy == 0) THEN
                    ! Save the data
                    elArray(i,1) = i
                    elArray(i,2) = node1
                    elArray(i,3) = node2 
                    elArray(i,4) = node3 
                    elArray(i,5) = node4 

                    ! Debug statement 
                    !WRITE(*,*) elArray(i,:)
                ELSEIF (dummy == 1) THEN
                    ! The 2nd line in the element section dictates if there are curved
                    ! edges or not
                    edge1Type = node1 
                    edge2Type = node2 
                    edge3Type = node3 
                    edge4Type = node4 

                    ! WRITE TREATMENT OF CURVED BOUNDARIES HERE
                END IF
                
                ! Detect the boundary condition data; it's not an integer so the
                ! previous read statement will throw an error
                IF (ios .NE. 0) THEN 
                    BACKSPACE(inputFileUnit)
                    READ(inputFileUnit,'(A)',IOSTAT=ios) readLine

                    ! Extract the boundary conditions from readLine
                    READ(readLine, *) BC

                    ! Format BCs
                    AllBCArray(i,1) = TRIM(BC(1))
                    AllBCArray(i,2) = TRIM(BC(2))
                    AllBCArray(i,3) = TRIM(BC(3))
                    AllBCArray(i,4) = TRIM(BC(4))

                    ! Debug statement
                    !WRITE(*,*) BC1, " | ", BC2, " | ", BC3, " | ", BC4
                    !WRITE(*,*) AllBCArray(i,:)

                    ! Loop though BC and add any new boundary conditions to an array
                    ! containing the unique names and corresponding BCIDs
                    DO j = 1,SIZE(BC)
                        IF (TRIM(BC(j)) == "---") THEN 
                        ! Skip faces that don't have a BC

                            ! Debug statement
                            !WRITE(*,*) i, j, BC(j)
                            CYCLE
                        ELSEIF ( .NOT. ANY(BC(j) == BCNameArray(:)) ) THEN
                        ! Check if the BC name has previously been added
                            count = count + 1
                            BCNameArray(count) = TRIM(BC(j))
                            BCIDArray(count) = count 
                            

                            ! Debug statement
                            !WRITE(*,*) "BC name detected: ", BCNameArray(count), "  BC ID assigned: ", BCIDArray(count)
                        
                            
                        END IF
                    END DO
                    EXIT
                END IF

                ! Increase dummy count
                dummy = dummy + 1             
            END DO            
        END DO

        ! Trim the unused entries from BC arrays
        BCNameArray = BCNameArray(1:count)
        BCIDArray   = BCIDArray(1:count)

        ! Terminal statement for the user
        WRITE(*,*) "Unique BCs detected: ", SIZE(BCIDArray)
        DO i = 1,SIZE(BCIDArray)
            WRITE(*,*) "    ", BCNameArray(i), " ---> ", BCIDArray(i)
        END DO

        ! Loop though all elements again and write the boundary info to an array
        count = 0
        DO i = 1,totalElNum
            ! Read in the boundary info
            READ(AllBCArray(i,:),*) BC

            ! Loop though read boundary, write corresponding BC to an array
            DO j = 1,SIZE(BC)
                IF ( ANY(BC(j) == BCNameArray(:)) ) THEN 
                    count = count + 1
                    BCArray(count,1) = i 
                    BCArray(count,2) = j 
                    DO k = 1,SIZE(BCNameArray)
                        IF (BC(j) == BCNameArray(k)) THEN 
                            BCArray(count,3) = BCIDArray(k)
                        ENDIF
                    END DO
                ENDIF
            END DO
        END DO

        ! Trim the BC array as needed
        BCArray = BCArray(1:count,:)

        ! Debug statement
        DO i = 1,size(BCArray(:,1))
            !WRITE(*,*) BCArray(i,:)
        END DO           
    END SUBROUTINE ReadHOHQMeshFile_ISM_2D

    ! ================================================================================

    SUBROUTINE ReadHOHQMeshFile_ISMV2_2D(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine reads an ISM-V2 format mesh file and returns 3 arrays: nodeArray,
    ! elArray, and BCArray. These contain the node, element, and BC data 
    ! necessary to write a BSC format mesh file.

        IMPLICIT NONE 

        ! Declare input variables
        INTEGER, INTENT(IN) :: inputFileUnit
        REAL,    DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: nodeArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: elArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: BCArray
        INTEGER,                              INTENT(OUT)   :: polyOrder

        ! Declare local variables
        REAL              :: x, y, z
        INTEGER           :: totalNodeNum, totalElNum, totalBCNum
        INTEGER           :: node1, node2, node3, node4
        INTEGER           :: edge1Type, edge2Type, edge3Type, edge4Type
        INTEGER           :: i, j, k, ios
        INTEGER           :: dummy, count
        CHARACTER(LEN=35) :: readLine
        CHARACTER(LEN=20) :: BC(4)
        CHARACTER(LEN=20), DIMENSION(:,:), ALLOCATABLE :: AllBCArray
        CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: BCNameArray
        REAL, DIMENSION(:), ALLOCATABLE      :: BCIDArray

        WRITE(*,*) "ERROR: This translator has not been designed to handle ISM-V2 format yet!"
        WRITE(*,*) "ERROR: Aborting mesh translation"
        RETURN
    END SUBROUTINE ReadHOHQMeshFile_ISMV2_2D

    ! ================================================================================

    SUBROUTINE ReadHOHQMeshFile_ISMMM_2D(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine reads an ISM-MM format mesh file and returns 3 arrays: nodeArray,
    ! elArray, and BCArray. These contain the node, element, and BC data 
    ! necessary to write a BSC format mesh file.

        IMPLICIT NONE 

        ! Declare input variables
        INTEGER, INTENT(IN) :: inputFileUnit
        REAL,    DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: nodeArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: elArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: BCArray
        INTEGER,                              INTENT(OUT)   :: polyOrder

        ! Declare local variables
        REAL              :: x, y, z
        INTEGER           :: totalNodeNum, totalElNum, totalBCNum
        INTEGER           :: node1, node2, node3, node4
        INTEGER           :: edge1Type, edge2Type, edge3Type, edge4Type
        INTEGER           :: i, j, k, ios
        INTEGER           :: dummy, count
        CHARACTER(LEN=35) :: readLine
        CHARACTER(LEN=20) :: BC(4)
        CHARACTER(LEN=20), DIMENSION(:,:), ALLOCATABLE :: AllBCArray
        CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: BCNameArray
        REAL, DIMENSION(:), ALLOCATABLE      :: BCIDArray

        ! Terminal message for user
        WRITE(*,*) "WARNING: This translator has not been designed to handle muliple material information!"
        WRITE(*,*) "WARNING: All material information will be lost during this operation!"
        WRITE(*,*) "Format: ISM-MM_2D --> BSC_2D"

        ! Read in node / el / type information
        READ(inputFileUnit,*,IOSTAT=ios) readLine
        READ(inputFileUnit,*,IOSTAT=ios) totalNodeNum, dummy, totalElNum, polyOrder
        WRITE(*,*) "Nodes: ", totalNodeNum, "  Elements: ", totalElNum, "  polyOrder: ", polyOrder

        ! Allocate memory to node and element arrays
        ALLOCATE( nodeArray(totalNodeNum, 4))
        ALLOCATE( elArray(totalElNum, 5))
        ALLOCATE( AllBCArray(totalElNum, 4))

        ! Allocate the "worst case scenario" of BC's: each element face has
        ! a different BC type
        ALLOCATE( BCArray(totalElNum*4,3) )
        ALLOCATE( BCNameArray(totalElNum*4) )
        ALLOCATE( BCIDArray(totalElNum*4) )

        ! Loop through all the nodes, saving them to nodeArray
        DO i = 1, totalNodeNum
            ! Read in nodal data
            READ(inputFileUnit, *, IOSTAT=ios) x, y, z

            ! Save nodal data to an array
            nodeArray(i,1) = i
            nodeArray(i,2) = x 
            nodeArray(i,3) = y 
            nodeArray(i,4) = z 

            ! Debug statement
            !WRITE(*,*) i, x, y, z
        ENDDO

        ! Loop through all the elements, saving data into elArray and BCArray
        count = 0  ! initialize for use in BC processing
        DO i = 1, totalElNum
            ! There may be multiple lines of info if the elements have curved boundaries,
            ! keep reading until we hit the BC info but only save the first read in line.
            ! Keep track of what line was read in first with "dummy" variable
            dummy = 0
            DO
                ! Save the nodes that make up the element
                READ(inputFileUnit, *, IOSTAT=ios) node1, node2, node3, node4

                ! Save the first read in line of this element section to elArray
                IF (dummy == 0) THEN
                    ! Save the data
                    elArray(i,1) = i
                    elArray(i,2) = node1
                    elArray(i,3) = node2 
                    elArray(i,4) = node3 
                    elArray(i,5) = node4 

                    ! Debug statement 
                    !WRITE(*,*) elArray(i,:)
                ELSEIF (dummy == 1) THEN
                    ! The 2nd line in the element section dictates if there are curved
                    ! edges or not
                    edge1Type = node1 
                    edge2Type = node2 
                    edge3Type = node3 
                    edge4Type = node4 

                    ! WRITE TREATMENT OF CURVED BOUNDARIES HERE
                END IF
                
                ! Detect the boundary condition data; it's not an integer so the
                ! previous read statement will throw an error
                IF (ios .NE. 0) THEN 
                    BACKSPACE(inputFileUnit)
                    READ(inputFileUnit,'(A)',IOSTAT=ios) readLine

                    ! Extract the boundary conditions from readLine
                    READ(readLine, *) BC

                    ! Format BCs
                    AllBCArray(i,1) = TRIM(BC(1))
                    AllBCArray(i,2) = TRIM(BC(2))
                    AllBCArray(i,3) = TRIM(BC(3))
                    AllBCArray(i,4) = TRIM(BC(4))

                    ! Debug statement
                    !WRITE(*,*) BC1, " | ", BC2, " | ", BC3, " | ", BC4
                    !WRITE(*,*) AllBCArray(i,:)

                    ! Loop though BC and add any new boundary conditions to an array
                    ! containing the unique names and corresponding BCIDs
                    DO j = 1,SIZE(BC)
                        IF (TRIM(BC(j)) == "---") THEN 
                        ! Skip faces that don't have a BC

                            ! Debug statement
                            !WRITE(*,*) i, j, BC(j)
                            CYCLE
                        ELSEIF ( .NOT. ANY(BC(j) == BCNameArray(:)) ) THEN
                        ! Check if the BC name has previously been added
                            count = count + 1
                            BCNameArray(count) = TRIM(BC(j))
                            BCIDArray(count) = count 
                            
                            ! Debug statement
                            !WRITE(*,*) "BC name detected: ", BCNameArray(count), "  BC ID assigned: ", BCIDArray(count)
                                                   
                        END IF
                    END DO
                    EXIT
                END IF

                ! Increase dummy count
                dummy = dummy + 1             
            END DO            
        END DO

        ! Trim the unused entries from BC arrays
        BCNameArray = BCNameArray(1:count)
        BCIDArray   = BCIDArray(1:count)

        ! Terminal statement for the user
        WRITE(*,*) "Unique BCs detected: ", SIZE(BCIDArray)
        DO i = 1,SIZE(BCIDArray)
            WRITE(*,*) "    ", BCNameArray(i), " ---> ", BCIDArray(i)
        END DO

        ! Loop though all elements again and write the boundary info to an array
        count = 0
        DO i = 1,totalElNum
            ! Read in the boundary info
            READ(AllBCArray(i,:),*) BC

            ! Loop though read boundary, write corresponding BC to an array
            DO j = 1,SIZE(BC)
                IF ( ANY(BC(j) == BCNameArray(:)) ) THEN 
                    count = count + 1
                    BCArray(count,1) = i 
                    BCArray(count,2) = j 
                    DO k = 1,SIZE(BCNameArray)
                        IF (BC(j) == BCNameArray(k)) THEN 
                            BCArray(count,3) = BCIDArray(k)
                        ENDIF
                    END DO
                ENDIF
            END DO
        END DO

        ! Trim the BC array as needed
        BCArray = BCArray(1:count,:)

        ! Debug statement
        DO i = 1,size(BCArray(:,1))
            !WRITE(*,*) BCArray(i,:)
        END DO  
    END SUBROUTINE ReadHOHQMeshFile_ISMMM_2D

    ! ================================================================================

    SUBROUTINE ReadHOHQMeshFile_ABAQUS_2D(inputFileUnit,nodeArray,elArray,BCArray,polyOrder)
    ! This subroutine reads an ABAQUS format mesh file and returns 3 arrays: nodeArray,
    ! elArray, and BCArray. These contain the node, element, and BC data 
    ! necessary to write a BSC format mesh file.

        IMPLICIT NONE 

        ! Declare input variables
        INTEGER, INTENT(IN) :: inputFileUnit
        REAL,    DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: nodeArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: elArray
        INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: BCArray
        INTEGER,                              INTENT(OUT)   :: polyOrder

        ! Declare local variables
        REAL              :: x, y, z
        INTEGER           :: totalNodeNum, totalElNum, totalBCNum
        INTEGER           :: node1, node2, node3, node4
        INTEGER           :: edge1Type, edge2Type, edge3Type, edge4Type
        INTEGER           :: i, j, k, ios
        INTEGER           :: dummy, count
        CHARACTER(LEN=35) :: readLine
        CHARACTER(LEN=20) :: BC(4)
        CHARACTER(LEN=20), DIMENSION(:,:), ALLOCATABLE :: AllBCArray
        CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: BCNameArray
        REAL, DIMENSION(:), ALLOCATABLE      :: BCIDArray

        WRITE(*,*) "ERROR: This translator has not been designed to handle ABAQUS format yet!"
        WRITE(*,*) "ERROR: Aborting mesh translation"
        RETURN
    END SUBROUTINE ReadHOHQMeshFile_ABAQUS_2D

END MODULE MeshTranslator


