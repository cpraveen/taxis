      SUBROUTINE IVVNXT (X,Y,NVvr,NNdePvt,
     &                   XVvr,YVvr,NFrmNde,NghVvr,MVvr,MBndNde,
     &                   XNode,YNode,ILevel,MNde,NLevel,
     &                   FOutCnv,NNdeNxt,NghLvMax,DNxt,
     &                   IVerbose,FLogFile,NtLog)
C
C Last update:
C 23May96; test exit status of IVVALK.
C 25Sep94;check for inclusion failure, exclude boundary vertices.
C 9May93,14:00; derived from IVVALK.
C
C Try to find a vertex that is connected to NNdePvt and has 
C neighbors of NLevel-1 only and is not a boundary vertex. Welcome
C to DOMINO software.
C
C Input:
C X/Y:      Coordinates for the distance DNxt.
C NVvr:     Voronoi vertex formed with node NNdePvt. 
C NNdePvt:  Vertex to search around.
C NFrmNde:  Array of forming nodes of the triangulation.
C NghVvr:   Array of neighbouring cells of the triangulation.
C MVvr:     Maximum number of cells to walk over.
C MBndNde:  Number of boundary nodes.
C X/YNode:  Vectors of node coordinates.
C ILevel:   Vector of grid levels for all nodes.
C MNde:     Number of nodes.
C NLevel:   Current grid level.
C
C Output:
C NVvr:     Any other Voronoi vertex formed with node NNdePvt.
C FOutCnv:  Flag indicating that search path left the convex hull
C           when set to true.
C NNdeNxt:  Vertex connected to NNdePvt with only NLevel-1 neighbors.
C NghLvMax: Highest grid level found in all vertices connected to
C           NNdeNxt.
C DNxt:     Distance from X,Y to NNdeNxt.
C Iverbose,FLogFile,NtLog...
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DIMENSION NFrmNde(3,*),NghVvr(3,*),ILevel(*)
      DOUBLE PRECISION X,Y,XVvr(*),YVvr(*),XNode(*),YNode(*)
C
C     Initialize the closest distance.
      DNxt = 1.E25

C     Find back NNdePvt in NVvr.
      Found = .FALSE.
      DO KNgh = 1,3
        IF (NNdePvt.EQ.NFrmNde(JCYCL(KNgh+2),NVvr)) THEN
          Found = .TRUE.
          NVvrNxt = NghVvr(KNgh,NVvr)
          NVvrTrgt = NVvr
C         NNdeEdge is the vertex on
C         the other end of the edge between NVvr and NVvrNxt.
          NNdeEdge = NFrmNde(JCYCL(KNgh+1),NVvr)
        END IF
      END DO
C
      IF (.NOT.Found) THEN
        WRITE (*,10) NNdePvt,NVvr
        IF (FLogFile) WRITE (NtLog,10) NNdePvt,NVvr
 10     FORMAT (' FATAL: died in IVVNXT since',I5,
     &          ' is not contained in',I5,'.'/)
        STOP
      END IF
C
C     Try the ring of vertices around NNdeEdge.
      NVvrIni = NVvrNxt
      CALL IVVALK (XNode(NNdeEdge),YNode(NNdeEdge),
     &             NVvrIni,NNdeEdge,
     &             XVvr,YVvr,NFrmNde,NghVvr,MVvr,
     &             FOutCnv,ILevel,NghLvMax)
      IF ( NNdeEdge .EQ. 0 ) THEN
C       IVVALK failed. In Multigrid, no recovery. ;-(.
        WRITE (*,'()')
     &       ' FATAL: IVVALK failed.'
        STOP
      ELSE IF (NghLvMax.LT.NLevel.AND.NNdeEdge.GT.MBndNde) THEN
        DNxt = (XNode(NNdeEdge)-X)**2+(YNode(NNdeEdge)-Y)**2
        NNdeNxt = NNdeEdge
      END IF
C
C     Walk counterclockwise around NNdePvt.
      DO WHILE (NVvrTrgt.NE.NVvrNxt)
        NVvr = NVvrNxt
        DO KNgh = 1,3
          IF (NFrmNde(KNgh,NVvr).EQ.NNdePvt) THEN
            NVvrNxt = NghVvr(JCYCL(KNgh+1),NVvr)
C           Try the ring of vertices around NNdeEdge, the vertex on
C           the other end of the edge between NVvr and NVvrNxt.
            NNdeEdge = NFrmNde(JCYCL(KNgh+2),NVvr)
            NVvrIni = NVvrNxt
            CALL IVVALK (XNode(NNdeEdge),YNode(NNdeEdge),
     &                   NVvrIni,NNdeEdge,
     &                   XVvr,YVvr,NFrmNde,NghVvr,MVvr,
     &                   FOutCnv,ILevel,NghLvMax)
            IF ( NNdeEdge .EQ. 0 ) THEN
C             IVVALK failed. In Multigrid, no recovery. ;-(.
              WRITE (*,'()')
     &             ' FATAL: IVVALK failed.'
              STOP
            END IF
            
            DEdge = (XNode(NNdeEdge)-X)**2+(YNode(NNdeEdge)-Y)**2
            IF (NghLvMax.LT.NLevel.AND.DEdge.LT.DNxt.AND.
     &          NNdeEdge.GT.MBndNde) THEN
              DNxt = DEdge
              NNdeNxt = NNdeEdge
            END IF
          END IF
        END DO
      END DO
C
      RETURN
      END




