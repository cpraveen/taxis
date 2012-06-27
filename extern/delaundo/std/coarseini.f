       SUBROUTINE COARSEINI (NLevel,ILevel,
     &                       MBnd,MUserBnd,NdxLBnNd,IBndType,NmNghFNd,
     &                       NmNghLNd,NmeBnd,NvNmBn,MinRes,
     &                       MVvr,XVvr,YVvr,RadVvr,NFrmNde,NghVvr,
     &                       RadTolMi,FlsOutVv,FlsFlgVv,LsFlgVvr,
     &                       MNde,XNode,YNode,
     &                       BkgSpc,IVerbose,FLogFile,NtLog)
C
C Last update:
C 11Aug95; proper count for boundaries below MinRes. 
C 26Sep94;new NGHBNDE.
C 24Sep94;fix bug with coarsening of segments with an even number of nodes.
C 20Apr94,15:30 conceived.
C
C Input:
C NLevel:   Current grid coarsening level.
C ILevel:   Vector of levels for each node.
C MBnd:     Total number of boundaries.
C MUserBnd: Number of user-specified boundaries.
C NdxLBnNd: Index of boundary segements.
C IBndType: Vector of boundary types.
C NmNghFNd: Vector of links to the preceding boundary segment.
C NmeBnd:   Vector of boundary names.
C NvNmBn:   Vector linking boundary names to numbers.
C MinRes:   Minimum resolution for each boundary segment.
C MVvr:     Number of cells in the grid.
C X/YVvr:   Vector of coordinates of the circumcenters.
C RadVvr:   Vector of circumradii.
C NFrmNde:  Array of forming nodes.
C NghVvr:   Array of neighboring cells.
C RadTolMi: Fraction of the circumradius that determines inclusion.
C FlsOutVv: Vector of flags for cells outside the domain.
C FlsFlgVv: Vector of flags for cells inside the cavity.
C LsFlgVvr: List of cells inside the cavity.
C MNde:     Number of nodes.
C X/YNode:  Vector of the node coordinates.
C BkgSpc:   Vector of spacing for nodes in the background grid.
C
C Output:
C ILevel:   Vector of levels for each node.
C MVvr:     Number of cells in the grid.
C X/YVvr:   Vector of coordinates of the circumcenters.
C RadVvr:   Vector of circumradii.
C NFrmNde:  Array of forming nodes.
C NghVvr:   Array of neighboring cells.
C BkgSpc:   Vector of spacing for nodes in the background grid.
C IVerbose: Verbosity.
C FLogFile:
C NtLog:
C
C Coarsen the boundary node distribution by approximately a factor of
C 2, double the background mesh spacing and create a new initial 
C triangulation with the coarse boundary discretization.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION ILevel(*),
     &          NdxLBnNd(0:*),IBndType(*),NmNghFNd(*),NmNghLNd(*),
     &          NvNmBn(-1:*),XVvr(*),YVvr(*),RadVvr(*),NFrmNde(3,*),
     &          NghVvr(3,*),FlsOutVv(*),FlsFlgVv(*),LsFlgVvr(*),
     &          XNode(*),YNode(*),BkgSpc(*),NmeBnd(*),MinRes(*)
C
C     Double the background mesh spacing.
      DO NNde = 5,NdxLBnNd(MBnd)
        BkgSpc(NNde) = 2.*BkgSpc(NNde)
      END DO
C
C     Loop over all boundary segments to establish a new coarsened
C     boundary.
      DO NBnd = 1,MUserBnd
C       Count the number of vertices on this boundary at level NLevel.
C       Note: MNdeSeg is actually not the number of segments, that is
C       boundary edges along the segment, but it is the number of
C       nodes along the current boundary segment. Anyhow: an odd
C       number of nodes means an even number of edges, and vice versa.
C       Keep in mind also, that the beginning and end are two separate
C       nodes, even if they coincide. All that matters here is the
C       count of the number of edges that can be coarsened.
        MNdeSeg = 1
        DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
          IF (ILevel(NNde).GT.NLevel-2) MNdeSeg = MNdeSeg+1
        END DO
C       First node of this boundary segment. This one is on!
        NNdePrv = NdxLBnNd(NvNmBn(NmNghFNd(NBnd)))
C
        IF ( (MNdeSeg+1)/2 .LT. MinRes(NBnd) ) THEN
C         Regular coarsening would be less than the minimum number
C         of vertices required for this segment. Carry all vertices
C         from the previous to this level, eliminate as many vertices
C         as needed, picking the ones with the smallest arc-length ratio.
          ILevel(NNdePrv) = NLevel
          DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
            IF (ILevel(NNde).EQ.NLevel-1) ILevel(NNde) = NLevel
          END DO
C
C         Cut one boundary node per loop.
          DO ICut = 1,MNdeSeg - (MinRes(NBnd)+1)
C
C           First pair of edges.
            NNde1 = NNdePrv
            CALL CALCEDGE (NNde1,NNde2,1,NLevel-1,IBnd,
     &                     NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                     ILevel,XNode,YNode,DEdge1,
     &                     MBnd,IVerbose,FLogFile,NtLog)
            CALL CALCEDGE (NNde2,NNde3,1,NLevel-1,IBnd,
     &                     NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                     ILevel,XNode,YNode,DEdge1,
     &                     MBnd,IVerbose,FLogFile,NtLog)
            DEdgeTot = DEdge1+DEdge2
            BSpcAvg = .5*(BkgSpc(NNde1)+BkgSpc(NNde3))
            DRatioMin = DEdgeTot/BSpcAvg
            NNdeCut = NNde3
C
C           Other pairs of edges.
            DO WHILE (NNde3.LT.NdxLBnNd(NBnd))
              NNde1 = NNde2
              NNde2 = NNde3
              DEdge1 = DEdge2
              CALL CALCEDGE (NNde2,NNde3,1,NLevel-1,NBnd,
     &                       NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                       ILevel,XNode,YNode,DEdge2,
     &                       MBnd,IVerbose,FLogFile,NtLog)
              DEdgeTot = DEdge1+DEdge2
              BSpcAvg = .5*(BkgSpc(NNde1)+BkgSpc(NNde3))
              DRatio = DEdgeTot/BSpcAvg
              IF (DRatio.LT.DRatioMin) THEN
C               Cut this edge
                NNdeCut = NNde3
                DRatioMin = DRatio
              END IF
            END DO
C
C           Cut NNdeCut.
            ILevel(NNdeCut) = NLevel-1
          END DO
C
        ELSE IF (2*INT(MNdeSeg/2).EQ.MNdeSeg) THEN
C         There's an even number of nodes on this segment. Find
C         a triplet of edges to form one new edge. Select that triplet
C         that has the lowest ratio of curvelength vs averaged
C         background spacing of the two outer nodes.
C
C         First triplet, including the last node on the previous
C         segment.
          NNde1 = NNdePrv
          CALL CALCEDGE (NNde1,NNde2,1,NLevel-1,IBnd,
     &                   NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                   ILevel,XNode,YNode,DEdge1,
     &                   MBnd,IVerbose,FLogFile,NtLog)
          CALL CALCEDGE (NNde2,NNde3,1,NLevel-1,IBnd,
     &                   NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                   ILevel,XNode,YNode,DEdge2,
     &                   MBnd,IVerbose,FLogFile,NtLog)
          CALL CALCEDGE (NNde3,NNde4,1,NLevel-1,IBnd,
     &                   NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                   ILevel,XNode,YNode,DEdge3,
     &                   MBnd,IVerbose,FLogFile,NtLog)
          DEdgeTot = DEdge1+DEdge2+DEdge3
          BSpcAvg = .5*(BkgSpc(NNde1)+BkgSpc(NNde4))
          DRatioMin = DEdgeTot/BSpcAvg
          NNdeCut = NNde3
C
C         Regular triplets.
          DO WHILE (NNde4.LT.NdxLBnNd(NBnd))
            NNde1 = NNde2
            NNde2 = NNde3
            NNde3 = NNde4
            DEdge1 = DEdge2
            DEdge2 = DEdge2
            CALL CALCEDGE (NNde3,NNde4,1,NLevel-1,NBnd,
     &                     NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                     ILevel,XNode,YNode,DEdge3,
     &                     MBnd,IVerbose,FLogFile,NtLog)
            DEdgeTot = DEdge1+DEdge2+DEdge3
            BSpcAvg = .5*(BkgSpc(NNde1)+BkgSpc(NNde4))
            DRatio = DEdgeTot/BSpcAvg
            IF (DRatio.LT.DRatioMin) THEN
C             Cut this edge
              NNdeCut = NNde3
              DRatioMin = DRatio
            END IF
          END DO
C
C         Mark all selected nodes as present on this level.
C         Start with the lower end of the segment up to just before
C         NNdeCut.
          NNde3 = NNdePrv
          DO WHILE (NNde3.LT.NNdeCut.OR.NNde3.EQ.NNdePrv)
            ILevel(NNde3) = NLevel
            NNde1 = NNde3
            CALL NGHBNDE (NNde1,1,NLevel-1,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNde2,IBnd)
            CALL NGHBNDE (NNde2,1,NLevel-1,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNde3,IBnd)
          END DO
C         Skip one more node.
          NNde2 = NNde3
          CALL NGHBNDE (NNde2,1,NLevel-1,ILevel,MBnd,
     &                  NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                  IVerbose,FLogFile,NtLog,
     &                  NNde3,IBnd)
C         Mark from after NNdeCut up to the end.
          DO WHILE (NNde3.LT.NdxLBnNd(NBnd))
            ILevel(NNde3) = NLevel
            NNde1 = NNde3
            CALL NGHBNDE (NNde1,1,NLevel-1,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNde2,IBnd)
            CALL NGHBNDE (NNde2,1,NLevel-1,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNde3,IBnd)
          END DO
C         Mark the end.
          ILevel(NdxLBnNd(NBnd)) = NLevel
C
        ELSE
C         This boundary has an odd number of nodes, cut every second.
          NNde3 = NNdePrv
          DO INde = 1,MNdeSeg-2,2
            ILevel(NNde3) = NLevel
            NNde1 = NNde3
            CALL NGHBNDE (NNde1,1,NLevel-1,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNde2,IBnd)
            CALL NGHBNDE (NNde2,1,NLevel-1,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNde3,IBnd)
          END DO
          ILevel(NdxLBnNd(NBnd)) = NLevel

C
        END IF
      END DO
C
C     Introduce all nodes at the current level into a new initial
C     triangulation.

      DO NBnd = 1,MUserBnd
        IF (IBndType(NBnd).NE.9) THEN
          PFVvr = MVvr
          MNdeSeg = 0
          DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
            IF (ILevel(NNde).EQ.NLevel) THEN
              MNdeSeg = MNdeSeg+1
C             Find the Voronoi vertices to be deleted.
              PFVvr = IWALK (XNode(NNde),YNode(NNde),PFVvr,
     &                       XNode,YNode,NFrmNde,NghVvr,MVvr,FOutCnv)
C             Find the constrained cavity.
              CALL FLGVVRCN (NNde,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,
     &                       NghVvr,RadVvr,FlsOutVv,LsFlgVvr,FlsFlgVv,
     &                       MFlgVvr,FOut)
C             Connect the new node with the valid structure.
              CALL CNCTNDE (NNde,MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                      LsFlgVvr,FlsFlgVv,MNwVvr)
C             Reordering the data structure, deleting obsolete elements.
              CALL PSTPRC (MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                     LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
            END IF
          END DO
C
          IF (IVerbose.GE.3) WRITE (*,10) NBnd,NmeBnd(NBnd),
     &                                    IBndType(NBnd),MNdeSeg+1
          IF (FLogFile) WRITE (NtLog,10) NBnd,NmeBnd(NBnd),
     &                                   IBndType(NBnd),MNdeSeg+1
 10       FORMAT (6X,' Boundary',I3,' named',I3,' type',I3,
     &            ' coarsened to',I5,' vertices.')
        
        END IF
      END DO
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE CALCEDGE (NNde1,NNde2,NdeInc,NLevel,IBnd,
     &                     NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                     ILevel,XNode,YNode,DEdge,
     &                     MBnd,IVerbose,FLogFile,NtLog)
C
C Last update:
C 24Sep94;cut out of COARSEINI.
C
C Find the next node on a boundary segment and calculate the edge
C length. Welcome to DOMINO software.
C
C Input:
C NNde1:   Node defining the boundary edge.
C NdeInc:  Direction of the edge [+1,-1].
C NLevel:  Level of the edge.
C NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType:
C ILevel:  Level indicator for each node.
C X/YNode:
C
C Output:
C NNde2:   Next boundary node in direction NdeInc.
C IBnd:    Boundary segment NNde1 lies on.
C DEdge:   Distance between NNde1 and NNde2.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION ILevel(*),
     &          NdxLBnNd(0:*),IBndType(*),NmNghFNd(*),NmNghLNd(*),
     &          NvNmBn(-1:*),XNode(*),YNode(*)
C
      CALL NGHBNDE (NNde1,NdeInc,NLevel,ILevel,MBnd,
     &              NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &              IVerbose,FLogFile,NtLog,
     &              NNde2,NBnd)
      DX = XNode(NNde2)-XNode(NNde1)
      DY = YNode(NNde2)-YNode(NNde1)
      DEdge = SQRT(DX*DX+DY*DY)
C
      RETURN
      END
      
