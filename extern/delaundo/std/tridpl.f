      SUBROUTINE TRIDPL (LsOutVvr,FlsOutVv,MOutVvr,LsFlgVvr,
     &                   NtWrt,Code,Version,
     &                   MVvr,NFrmNde,NghVvr,
     &                   XVvr,YVvr,RadVvr,
     &                   MNde,XNode,YNode,
     &                   MBnd,NdxLBnNd,NmeBnd,IBndType,
     &                   NrBnd,MNrBnd,NdeCrnr,
     &                   Index,Index2)
C
C Last update:
C 25Sep94;paste background grid removal into RMBKG.
C 28Oct92,14:00;eliminate WteNdes.
C 16Oct92,19:57;recovered from QUADTRI.
C
C Writes the output of a Delaunay Triangulation in .DPL format.
C Welcome to DOMINO software.
C
C Input:
C LsOutVvr:  List of triangles outside the domain. This array is used
C            as a work array and has to be dimensioned to at least
C            the number of boundary nodes/cells, including the 
C            additional midside entries.
C FlsOutVv:  Flag array of triangles outside the domain.
C MOutVvr:   Amount of triangles outside the domain.
C NtWrt:     Number assigned to the logical unit of the output file.
C Code:      Code used to produce the grid.
C Version:   Version number of the code used.
C MVvr:      Number of Voronoi Vertices in the structure.
C NFrmNde:   Array of the three forming nodes of each Voronoi vertex.
C NghVvr:    Array of the three neighbouring Voronoi vertices to
C            each Voronoi vertex.
C XVvr:      Vector of x-coordinates of the formed Voronoi Vertices.
C YVvr:      Vector of y-coordinates of the formed Voronoi Vertices.
C RadVvr:    Vector of the Squared radii of the circumcircles around
C            each Voronoi vertex.
C MNde:      Number of nodes introduced in the structure.
C XNode:     Vector of x-coordinates of the nodes.
C YNode:     Vector of y-coordinates of the nodes.
C MBnd:      Number of boundaries.
C NdxLBnNd:  Vector of indices of the last node on each
C            boundary segment.
C NmeBnd:    Vector of names given to the boundary segments.
C IBndType:  Type of the boundary. See DELAUNDO for details.
C NrBnd:     Workspace for the boundary denomination for each boundary
C            node. Required dimension of LsOutVvr.
C MNrBnd:    Workspace for the number of nodes on each boundary.
C            Required dimension of the number of boundaries.
C NdeCrnr:   Workspace to point to nodes at junctions of boundaries.
C            Required dimension of the number of boundaries.
C Index/2:   Work array for hsort, required length as LsOutVvr.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION LsOutVvr(*),FlsOutVv(*),LsFlgVvr(*),
     &          NdeCrnr(*),NrBnd(*),MNrBnd(*),Index(*),Index2(*),
     &          NFrmNde(3,*),NghVvr(3,*),XVvr(*),YVvr(*),RadVvr(*),
     &          XNode(*),YNode(*),NdxLBnNd(0:*),NmeBnd(*),IBndType(*)
      CHARACTER Code*20,Version*5
C
C     Eliminate all pointers to cells outside the domain.
      IF (MOutVvr.GT.4) THEN
        DO NVvr = 5,MVvr
          DO KNgh = 1,3
            IF (FlsOutVv(NghVvr(KNgh,NVvr))) NghVvr(KNgh,NVvr) = -10
          END DO
        END DO
      END IF
C
C     Eliminate all cells outside the domain.
      MNwVvr = 0
      MOutVvr = MOutVvr-4
      IF (MOutVvr.GT.1) THEN
        CALL HSORT (MOutVvr,LSOutVvr,Index)
        DO Ntry = 1,MOutVvr
          LsFlgVvr(Ntry) = LsOutVvr(Index(Ntry))
        END DO
        CALL PSTPRC (MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &               LsFlgVvr,MOutVvr,FlsOutVv,MNwVvr)
      END IF
C
C     Erase counters for the cells on different boundary segments.
      DO NBnd = 1,MBnd
        MNrBnd(NBnd) = 0
      END DO
C
C     Form boundary cells.
C     Loop over the physical Voronoi vertices.
      MBndCell = 0
      MFlgCell = 0
      DO NVvr = 5,MVvr
        DO KNgh = 1,3
          IF (NghVvr(KNgh,NVvr).EQ.-10) THEN
C           This is a boundary edge. Let's form a boundary cell.
            MBndCell = MBndCell+1
            NCell = MVvr+MBndCell
C           List the forming nodes.
            NFrmNde(1,NCell) = NFrmNde(JCYCL(KNgh+2),NVvr)
            NFrmNde(2,NCell) = NFrmNde(JCYCL(KNgh+1),NVvr)
            NghVvr(3,NCell) = NVvr
            NghVvr(KNgh,NVvr) = NCell
C
C           Find the boundary numbers for the boundary cells.
C           Search through the boundary segments using the pointers
C           NdxLBnNd and bipartition.
            NNde1 = NFrmNde(1,NCell)
            NNde2 = NFrmNde(2,NCell)
            KNdxLow = 0
            KNdxHigh = MBnd
            DO WHILE (KNdxHigh-KNdxLow.GT.1)
              KNdxMed = INT(.5*(KNdxHigh+KNdxLow))
              IF (NNde1.LE.NdxLBnNd(KNdxMed)) THEN
C               NNde1 is in the lower interval.
                KNdxHigh = KNdxMed
              ELSE
C               NNde1 is in the higher interval.
                KNdxLow = KNdxMed
              END IF
            END DO
C
C           This element is given with the domain to the left.
            NrBnd(MBndCell) = KNdxHigh
            MNrBnd(KNdxHigh) = MNrBnd(KNdxHigh)+1
            IF (NNde1.EQ.NdxLBnNd(KNdxHigh-1)+1) THEN
C             This is a corner between two boundary segments. Note NNde2
C             as node with different boundary information than the cell.
C             Search for the number of the boundary NNde2 lies on using
C             NdxLBnNd and bipartition.
              KNdxLow = 0
              KNdxHigh = MBnd
              DO WHILE (KNdxHigh-KNdxLow.GT.1)
                KNdxMed = INT(.5*(KNdxHigh+KNdxLow))
                IF (NNde2.LE.NdxLBnNd(KNdxMed)) THEN
C                 NNde2 is in the lower interval.
                  KNdxHigh = KNdxMed
                ELSE
C                 NNde2 is in the higher interval.
                  KNdxLow = KNdxMed
                END IF
              END DO
              NdeCrnr(KNdxHigh) = NNde2
            END IF
C
          END IF
        END DO
      END DO
C
C     Sort the boundary cells for different boundary numbers.
      CALL HSORT (MBndCell,NrBnd,Index)
      DO NBndCell = 1,MBndCell
        NCellNew = MVvr+Index(NBndCell)
        LsOutVvr(NBndCell) = NFrmNde(1,NCellNew)
      END DO
C
C     Sort the boundary cells of each boundary for ascending nodes.
      Ndx = 1
      DO NBnd = 1,MBnd
        CALL HSORT (MNrBnd(NBnd),LsOutVvr(Ndx),Index2(Ndx))
        DO Ntry = Ndx,Ndx+MNrBnd(NBnd)-1
          Index2(Ntry) = Index2(Ntry)+Ndx-1
        END DO
        Ndx = Ndx+MNrBnd(NBnd)
      END DO
      DO NBndCell = 1,MBndCell
        NCellNew = MVvr+NBndCell
        NCellOld = MVvr+Index(Index2(NBndCell))
C       Move cells. Store the two forming nodes in NghVvr(1:2), the
C       neighbor in NFrmNde(1) and the boundary number in LsOutVvr.
        NghVvr(1,NCellNew) = NFrmNde(1,NCellOld)
        NghVvr(2,NCellNew) = NFrmNde(2,NCellOld)
        NFrmNde(3,NCellNew) = NghVvr(3,NCellOld)
        LsOutVvr(NBndCell) = NrBnd(Index(Index2(NBndCell)))
C       Correct the neighboring information in the interior cells.
        NNgh = NghVvr(3,NCellOld)
        DO KNgh = 1,3
          IF (NghVvr(KNgh,NNgh).EQ.NCellOld) THEN
            NghVvr(KNgh,NNgh) = -NCellNew
          END IF
        END DO
      END DO
C
C     Write to .DPL file:
C     Header.
      FChar = .TRUE.
      ICode = 0
      DO WHILE (FChar.AND.ICode.LT.20)
        ICode = ICode+1
        IF (Code(ICode:ICode).EQ.' ') FChar = .FALSE.
      END DO
      FChar = .TRUE.
      IVers = 0
      DO WHILE (FChar.AND.IVers.LT.5)
        IVers = IVers+1
        IF (Version(IVers:IVers).EQ.' ') FChar = .FALSE.
      END DO
      WRITE (NTWRT,'(3A)') 'unstructured grid data by ',
     &                          Code(1:ICode),Version(1:IVers)
C
C     Number of interior cells, number of previous iterations,
C     convergence check variable.
      WRITE (NtWrt,'(I7,2I2)') MVvr-4,0,1
c      WRITE (NtWrt,'(I7)') MVvr-4
C
C     Forming nodes and neighbors of these cells.
      DO NVvr = 5,MVvr
        WRITE (NtWrt,'(I1,1X,3(3I7,2X))') 3,NFrmNde(1,NVvr)-4,
     &                                    NFrmNde(2,NVvr)-4,
     &                                    NFrmNde(3,NVvr)-4,
     &                                    ABS(NghVvr(1,NVvr))-4,
     &                                    ABS(NghVvr(2,NVvr))-4,
     &                                    ABS(NghVvr(3,NVvr))-4,NVvr-4
c        WRITE (NtWrt,'(I1,1X,3I7)') 3,NFrmNde(1,NVvr)-4,
c     &                              NFrmNde(2,NVvr)-4,
c     &                              NFrmNde(3,NVvr)-4
      END DO
C
C     Number of nodes ignoring the 4 nodes of the setup hull.
      WRITE (NtWrt,'(I6)') MNde-4
C     Conditions at infinity, normalizers for Rms, ResMax.
      WRITE (NtWrt,'(6F3.0)') 1.,1.,1.,1.,0.,0.
c      WRITE (NtWrt,'(4F3.0)') 1.,1.,1.,1.
C
C     Loop over nodes.
      DO NNde = 5,MNde
        WRITE (NtWrt,'(2E15.7,4F3.0,I6)') XNode(NNde),YNode(NNde),
     &                                 1.,1.,1.,1.,NNde-4
c        WRITE (NtWrt,'(2E15.7,4F3.0)') XNode(NNde),YNode(NNde),
c     &                                 1.,1.,1.,1.
      END DO
C
C     Bodies/Elements,Corners,Cornernodes,Cornernames.
      WRITE (NtWrt,'(2I5,40I8)') MBnd,MBnd,
     &                           (NdeCrnr(NBnd)-4,NBnd=1,MBnd),
     &                           (NmeBnd(NBnd),NBnd=1,MBnd)
c      WRITE (NtWrt,'(I5)') MBnd
C
C     Loop over all boundary segments.
      DO NBnd = 1,MBnd
C
C       Number of faces.
        WRITE (NtWrt,'(2I6)') MNrBnd(NBnd),NmeBnd(NBnd)
c        WRITE (NtWrt,'(I6)') MNrBnd(NBnd)
        DO NBndCell = 1,MBndCell
          NCell = MVvr+NBndCell
          IF (LsOutVvr(NBndCell).EQ.NBnd) WRITE (NtWrt,'(4I8)')
     &                                        NghVvr(1,NCell)-4,
     &                                        NghVvr(2,NCell)-4,
     &                                        NFrmNde(3,NCell)-4,NCell-4
c          IF (LsOutVvr(NBndCell).EQ.NBnd) WRITE (NtWrt,'(2I8)')
c     &                                        NghVvr(1,NCell)-4,
c     &                                        NghVvr(2,NCell)-4
        END DO
C
      END DO
C
C     Boundary (left empty, included in bodies).
      WRITE (NtWrt,'(2I3)') 0,-1
C
      RETURN
      END
