C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      SUBROUTINE BOXWALK (X,Y,NVvrContain,NNdeClosest,
     &                    NFrmNde,NghVvr,MVvr,
     &                    XNode,YNode,MNde,
     &                    LsFlgVvr,FlsFlgVvr,MaxFlgVvr,
     &                    FOutCnv)
C
C Last update:
C
C     
C Find the nearest vertex to X,Y without assuming Delaunay properties,
C i.e. when IVVALK fails. Take a pointer to a cell in the vicinity,
C initialize a minimum box with the closest vertex of that cell, and
C include all cells in the path whose bounding boxes overlap with the
C minimal box.
C
C Input:
C X:        X coordinate of the point to check for.
C Y:        Y coordinate of the point to check for.
C NVvrContain:Mesh cell to start checking.
C X/YVVr:   Vectors of x/y-coordinates of the Voronoi vertices.
C NFrmNde:  Array of forming nodes of the triangulation.
C NghVvr:   Array of neighbouring cells of the triangulation.
C MVvr:     Maximum number of cells to walk over.
C ILevel:   Vector of grid levels for all nodes.
C LsFlgVvr: List of flagged Voronoi vertices.
C FLsFlgVvr:Array of flags that show that the corresponding
C           Voronoi vertex has been flagged when set to true.
C MaxFlgVvr:Length of LsFlgVvr.
C
C Output:
C NVvrContain:Containing cell.
C NNdeClosest:  Vertex closest to the point XY.
C FOutCnv:  Flag indicating that search path left the convex hull
C           when set to true.
C NghLevelMax: Highest grid level found in all vertices connected to
C           NNdePvt.
C
      IMPLICIT NONE
      INTEGER MVvr,NVvr,NFrmNde(3,*),NghVvr(3,*), MFlgVvr, NFlgVvr,
     &        MaxFlgVvr, NNgh, KNgh, KNde, NNdeClosest,
     &        MNde, LsFlgVvr(MaxFlgVvr), IWALK, KVvr,NVvrContain
      LOGICAL*1 FlsFlgVvr(MaxFlgVvr),FOutCnv, FOverLap, OVERLAP
      DOUBLE PRECISION XNode(*),YNode(*), X,Y,DMin,
     &                 XNLL,YNLL,XNUR,YNUR,
     &                 XBLL,YBLL,XBUR,YBUR
C
C     Find the containing cell.
      NVvr = IWALK(X,Y,NVvrContain,XNode,YNode,NFrmNde,NghVVr,MVvr,
     &             FOutCnv)
      IF ( FOutCnv ) RETURN
C
C     Erase the vector of flags.
      DO KVvr = 1,MVvr
        FlsFlgVvr(KVvr) = .FALSE.
      END DO
C
C     Initialize the box.
      DMin = 1.D25
      CALL SIZEBOX( NNdeClosest, NFrmNde(1,NVvr), XNode,YNode,
     &              X,Y, DMin, XNLL, YNLL, XNUR, YNUR )
C     Initialize the list. The list contains all cells found that
C     have a box intersection and have been compared to the Mins.
      LsFlgVvr(1) = NVvr
      FlsFlgVvr(NVvr) = .TRUE.
      MFlgVvr = 1
C
C     Loop over all cells in the list.
      NFlgVvr = 0
      DO WHILE ( NFlgVvr .LT. MFlgVvr )
        NFlgVvr = NFlgVvr+1
        NVvr = LsFlgVvr(NFlgVvr)

        DO KNgh = 1,3
          NNgh = NghVvr(KNgh,NVvr)

          IF ( .NOT. FlsFlgVvr(NNgh) ) THEN
C           This cell has not been checked, yet. Find its
C           bounding box.
            CALL BNDBOX( NFrmNde(1,NNgh), XNode,YNode,
     &                   XBLL, YBLL, XBUR, YBUR )
C
C           Check whether the boxes overlap.
            FOverLap = OVERLAP( XBLL, YBLL, XBUR, YBUR,
     &                          XNLL, YNLL, XNUR, YNUR )
C
            IF ( FOverLap ) THEN
C             Update the target box.
              CALL SIZEBOX( NNdeClosest, NFrmNde(1,NNgh), XNode,YNode,
     &                      X,Y, DMin, XNLL, YNLL, XNUR, YNUR )

C             Add NNgh to the list.
              MFlgVvr = MFlgVvr+1
              IF ( MFlgVvr .GT. MaxFlgVvr ) THEN
                WRITE (*,'(A/A)')
     &          ' FATAL: out of space for MFlgVvr in boxwalk.',
     &          '        Increase MaxBndNde in nodes.cbl.'
                STOP
              END IF
              LsFlgVvr(MFlgVvr) = NNgh
              FlsFlgVvr(NNgh) = .TRUE.

            END IF
          END IF
        END DO
      END DO

      RETURN
      END
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      SUBROUTINE SIZEBOX( NNdeClosest, NFrmNde,XNode,YNode,
     &                    X,Y, DMin, XNLL, YNLL, XNUR, YNUR )
C
C Last update:
C
C     
C Calculate the smallest bounding box for a node, given an
C initial bounding box and a new node that is possibly closer
C than the one used for the calculation of the previous box.
C
C Input:
C X:        X coordinate of the point to check for.
C Y:        Y coordinate of the point to check for.
C NFrmNde:  Array of forming nodes of the triangulation.
C X/YNode:  Coordinates.
C DX/YMin:  X/Y dimension of the bounding box.
C DMinSq:   Minimum squared distance to the closest node.
C
C Changes to:
C DX/YMin:  X/Y dimension of the bounding box.
C DMinSq:   Minimum squared distance to the closest node.
C X/YNLL/UR: LowerLeft/UpperRight corner of the bounding box.
C
      IMPLICIT NONE
      INTEGER NFrmNde(3), KNde, NNde, NNdeClosest,NNdeCell
      DOUBLE PRECISION XNode(*),YNode(*),X,Y,DX,DY,DMin,DMinNew,
     &                 XNLL,YNLL,XNUR,YNUR,DMinCell
C
C     Loop over all forming nodes of this cell and compare,
C     retain the closest node of this cell.
      DMinCell = 1.D25
      DO KNde = 1,3
        NNde = NFrmNde(KNde)
        DX = XNode(NNde) - X
        DY = YNode(NNde) - Y
        DMinNew = DX**2+DY**2
C
        IF ( DMinNew .LT. DMinCell ) THEN
          DMinCell = DMinNew
          NNdeCell = NNde
        END IF
      END DO
      DMinCell = SQRT( DMinCell )
C
C     Update the box if it has shrunk.
      IF ( DMinCell .LT. DMin ) THEN
        DMin = DMinCell
        NNdeClosest = NNdeCell
        XNLL = X-DMin
        YNLL = Y-DMin
        XNUR = X+DMin
        YNUR = Y+DMin
      END IF

      RETURN
      END
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      SUBROUTINE BNDBOX( NFrmNde, XNode,YNode,
     &                   XBLL, YBLL, XBUR, YBUR )
C
C Last update:
C
C     
C Calculate the bounding box for a triangle.
C
C Input:
C NFrmNde:  Array of forming nodes of the triangulation.
C X/YNode:  Coordinates.
C
C Changes to:
C X/YNLL/UR: LowerLeft/UpperRight corner of the bounding box.
C
      IMPLICIT NONE
      INTEGER NFrmNde(3), NVvr, KNde, NNde
      DOUBLE PRECISION XBLL,YBLL,XBUR,YBUR, XNode(*),YNode(*)
C
      XBLL = XNode(NFrmNde(1))
      XBUR = XBLL
      YBLL = YNode(NFrmNde(1))
      YBUR = YBLL
C
      DO KNde = 2,3
        NNde = NFrmNde(KNde)
        XBLL = MIN( XBLL, XNode(NNde) )
        XBUR = MAX( XBUR, XNode(NNde) )
        YBLL = MIN( YBLL, YNode(NNde) )
        YBUR = MAX( YBUR, YNode(NNde) )
      END DO
C
      RETURN
      END
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      LOGICAL*1 FUNCTION OVERLAP ( XALL, YALL, XAUR, YAUR,
     &                             XBLL, YBLL, XBUR, YBUR )
C
C Last update:
C
C     
C Check whether two rectangular boxes overlap. Test whether
C     1: NLL is in B
C     2: NUR is in B
C     3: B is in N, i.e BLL in N
C
C Input:
C X:        X coordinate of the point to check for.
C Y:        Y coordinate of the point to check for.
C NFrmNde:  Array of forming nodes of the triangulation.
C
C Output:
C
      IMPLICIT NONE
      INTEGER NFrmNde(3), NVvr, KNde, NNde
      DOUBLE PRECISION XALL,YALL,XAUR,YAUR,XBLL,YBLL,XBUR,YBUR
C
C     Overlap in x?
      IF ( XAUR .LT. XBLL .OR. XBUR .LT. XALL ) THEN
C       No overlap in x.
        OVERLAP = .FALSE.
        RETURN
      ELSE IF ( YAUR .LT. YBLL .OR. YBUR .LT. YALL ) THEN
C       No overlap in y.
        OVERLAP = .FALSE.
        RETURN
      ELSE
        OVERLAP = .TRUE.
        RETURN
      END IF

      END
