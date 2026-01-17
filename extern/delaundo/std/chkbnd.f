      SUBROUTINE CHKBND (MLmt,LmtNde,NdxLBnNd,NvNmBn,NmNghFNd,MBnd,
     &     IBndType,XNode,YNode,NLevel,ILevel,RadTolMi,
     &     MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &     FlsOutVv,FlsFlgVv,LsFlgVvr,LsNde,LsNgh,MaxLsFlg)
C
C Last update:
C 26Sep94;new NGHBNDE.
C 26Oct92,17:43;conceived.
C
C Check boundary conformatlity and swap edges when necessary. Welcome to
C DOMINO software.
C
C Input:
C MLmt:     Number of disconnected elements.
C LmtNde:   Vector of pointers to a node on each such element.
C NdxLBnNd: Vector of pointers ot the last node of each boundary.
C NvNmBn,   Vector relating boundary names to boundary numbers.
C NmNghLNd: Vector of names of boundaries following each segment.
C MBnd:     Number of boundaries.
C X/YNode:  Coordinates of the nodes.
C MVvr:     Number of cells.
C NFrmNde:  Array of the forming nodes of each triangle.
C NghVvr:   Array of the neighboring Voronoi vertices of each triangle.
C NLevel:   Current MG-level.
C ILevel:   Vector of level markers for each node.
C
C Output:
C NFrmNde:  Forming nodes for the updated connectivity.
C NghVvr:   Neighbors of the updated connectivity.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION LmtNde(*),NdxLBnNd(0:*),NvNmBn(-1:*),NmNghFNd(*),
     &          XNode(*),YNode(*),NFrmNde(3,*),NghVvr(3,*),IBndType(*),
     &          ILevel(*),RadVvr(*),FlsOutVv(*),LsFlgVvr(*),FlsFlgVv(*),
     &          LsNde(*),LsNgh(*),XVvr(*),YVvr(*)
C     Dimension a bogus NmNghLNd, since we are only travelling down
C     with NdeInc = -1, we only need NmNghFNd.
      DIMENSION NmNghLNd(1)
      REAL PrdCrPrd
      NdeInc = -1
C
C     Loop over the checking procedure until on no boundary 
C     violations have been detected.
      IOutLoop = 0
      FViol = .TRUE.
      DO WHILE (FViol.AND.IOutLoop.LT.10)
        IOutLoop = IOutLoop+1
        FViol = .FALSE.
C
C       Loop over all closed sets of boundaries.
        DO NLmt = 1,MLmt
C
C         Index of the pointer to the element.
          NNdePvt = LmtNde(NLmt)
          IF (NLevel.GT.1) THEN
C           On a coarser level, take the last node of the segment
C           as the pivot, since it has to be on the coarsened segment.
            NBnd = 1
            DO WHILE (NdxLBnNd(NBnd).LT.NNdePvt)
              NBnd = NBnd+1
            END DO
            NNdePvt = NdxLBnNd(NBnd)
          END IF
C
C         Initialize a cell for walking.
          NIniVvr = 5
C         Loop until the contour around the element is closed.
          NNdeTrgt = 0
          DO WHILE (NNdePvt.NE.NNdeTrgt)
            CALL NGHBNDE (NNdePvt,NdeInc,NLevel,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NNdeNxt,NBnd)
C
C           Calculate the position of the midpoint between two
C           successive boundary nodes on that element.
            XMid = .5*(XNode(NNdePvt)+XNode(NNdeNxt))
            YMid = .5*(YNode(NNdePvt)+YNode(NNdeNxt))
C           Find a triangle that contains the midpoint.
            NVvr = IWALK(XMid,YMid,NIniVvr,XNode,YNode,NFrmNde,NghVvr,
     &                   MVvr,FOutCnv)
C
C           Try to find NNdePvt in that triangle.
            IF (NFrmNde(1,NVvr).EQ.NNdePvt) THEN
              Nde1 = NFrmNde(2,NVvr)
              Nde2 = NFrmNde(3,NVvr)
            ELSE IF (NFrmNde(2,NVvr).EQ.NNdePvt) THEN
              Nde1 = NFrmNde(3,NVvr)
              Nde2 = NFrmNde(1,NVvr)
            ELSE IF (NFrmNde(3,NVvr).EQ.NNdePvt) THEN
              Nde1 = NFrmNde(1,NVvr)
              Nde2 = NFrmNde(2,NVvr)
            ELSE
C             NNdePvt is not contained in a triangle containing the
C             midpoint. Find a triangle that is.
              NVvr = IWALK(XNode(NNdePvt),YNode(NNdePvt),NIniVvr,
     &                     XNode,YNode,NFrmNde,NghVvr,MVvr,FOutCnv)
C             Travel through all triangles with NNdePvt to find the
C             one that intersects the vector towards NNdeNxt.
              XPvt = XNode(NNdePvt)
              YPvt = YNode(NNdePvt)
              XBndEdge = XNode(NNdeNxt)-XPvt
              YBndEdge = YNode(NNdeNxt)-YPvt
              PrdCrPrd = 1.
              NVvrNxt = NVvr
              NVvrTrgt = 0
              DO WHILE (PrdCrPrd.GE.0.AND.NVvrNxt.NE.NVvrTrgt)
                NVvr = NVvrNxt
                IF (NVvrTrgt.EQ.0) NVvrTrgt = NVvrNxt
C               Calculate the vectors along the two sides with NNdePvt.
                IF (NFrmNde(1,NVvr).EQ.NNdePvt) THEN
                  Nde1 = NFrmNde(2,NVvr)
                  Nde2 = NFrmNde(3,NVvr)
                  NVvrNxt = NghVvr(2,NVvr)
                ELSE IF (NFrmNde(2,NVvr).EQ.NNdePvt) THEN
                  Nde1 = NFrmNde(3,NVvr)
                  Nde2 = NFrmNde(1,NVvr)
                  NVvrNxt = NghVvr(3,NVvr)
                ELSE IF (NFrmNde(3,NVvr).EQ.NNdePvt) THEN
                  Nde1 = NFrmNde(1,NVvr)
                  Nde2 = NFrmNde(2,NVvr)
                  NVvrNxt = NghVvr(1,NVvr)
                ELSE
                  WRITE (*,*) 'OOPS2 in chkbnd.'
                  STOP
                END IF
                X1 = XNode(Nde1)-XPvt
                Y1 = YNode(Nde1)-YPvt
                CrPrd1 = X1*YBndEdge-Y1*XBndEdge
                X2 = XNode(Nde2)-XPvt
                Y2 = YNode(Nde2)-YPvt
                CrPrd2 = X2*YBndEdge-Y2*XBndEdge
                PrdCrPrd = CrPrd1*CrPrd2
              END DO
              IF (PrdCrPrd.GE.0) THEN
                WRITE (*,*) 'OOPS3 in chkbnd.'
                STOP
              END IF
            END IF
C
C           Test whether the next node on the boundary is another
C           forming node of this triangle.
            IF (Nde1.EQ.NNdeNxt.OR.Nde2.EQ.NNdeNxt) THEN
C             Eureka.
C
              IF (NNdeTrgt.EQ.0) THEN
C               Set the pointer NNdeTrgt.
                IF (IBndType(NBnd).EQ.1.OR.IBndType(NBnd).EQ.2) THEN
C                 This is a set of frontal boundaries connected to
C                 a loop. We're done when we get back.
                  NNdeTrgt = NNdePvt
                ELSE 
C                 This boundary is open ended. We're done when we
C                 get to the end.
                  IF (NmNghFNd(NBnd).EQ.0) THEN
C                   This boundary is open ended. 
                    NNdeTrgt = NdxLBnNd(NBnd-1)+1
                  ELSE
C                   The target is the last node of the neighboring
C                   segment at the beginning.
                    NghF = NvNmBn(NmNghFNd(NBnd))
                    NghFF = NvNmBn(NmNghFNd(NghF))
                    NNdeTrgt = NdxLBnNd(NghFF)
                  END IF
                END IF
              END IF
              NNdePvt = NNdeNxt
C
            ELSE
C             This face is missing. Find the two cavities formed
C             by all the cells cut by the missing edge and split
C             by that edge.
              CALL FACECAVS(MVvr,NFrmNde,NghVvr,XNode,YNode,
     &             LsFlgVvr,FlsFlgVv,LsNde,LsNgh,MaxLsFlg,
     &             NNdePvt,NNdeNxt,NVvr,MFlgVvr,MNdeIn,MNdeOut)

c             Problem: We don't know yet the numbering of the cells that
c             will be formed in the cavity. In order to recover the numbers
c             of the two cells formed with the edge, place a pointer to two
c             virtual cells with each cavity. The two calls to TRICAV will
c             generate no more than MNdeIn+MNdeOut new elements.
c             Create the two virtual cells that allow to join the two
c             cavities. +1 is inside, +2 is outside. Use face 1
c             in both cases.
              NVvrIn = MVvr+MNdeIn+MNdeOut+1
              LsNgh(MNdeOut) = NVvrIn
              NFrmNde(1,NVvrIn) = 0
              NFrmNde(2,NVvrIn) = NNdePvt 
              NFrmNde(3,NVvrIn) = NNdeNxt
              NVvrOut = MVvr+MNdeIn+MNdeOut+2
              LsNgh(2*MNdeOut+MNdeIn) = NVvrOut
              NFrmNde(1,NVvrOut) = 0
              NFrmNde(2,NVvrOut) = NNdeNxt
              NFrmNde(3,NVvrOut) = NNdePvt

c             Triangulate the two cavities. Note that TRICAV extends
c             the cyclic succession, thus start the second cavity
c             at 2*MndeIn.
              MNwVvr = 0
              MPNde = MNdeOut
              CALL TRICAV (MVvr,MNwVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &             RadTolMi,XNode,YNode,LsNde,LsNgh,MPNde)
              MPNde = MNdeIn
              CALL TRICAV (MVvr,MNwVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &             RadTolMi,XNode,YNode,LsNde(2*MNdeOut+1),
     &             LsNgh(2*MNdeOut+1),MPNde)

c             Fix the connectivity of the two elements bordering the
c             recovered edge.
              NghIn = NghVvr(1,NVvrOut)
              NghOut = NghVvr(1,NVvrIn)
              FoundIn = .FALSE.
              FoundOut = .FALSE.
              DO KNgh = 1,3
                IF ( NghVvr(KNgh,NghIn) .EQ. NVvrOut ) THEN
                  FoundOut = .TRUE.
                  NghVvr(KNgh,NghIn) = NghOut
                END IF
                IF ( NghVvr(KNgh,NghOut) .EQ. NVvrIn ) THEN
                  FoundIn = .TRUE.
                  NghVvr(KNgh,NghOut) = NghIn
                END IF
              END DO
              IF ( .NOT. ( FoundIn .AND. FoundOut ) ) THEN
                WRITE (*,'(2A,2(I7,A))')
     &               ' FATAL: could not recover connectivity across',
     &               ' the edge',NNdePvt,' to',NNdeNxt,' in chkbnd.f.'
                STOP
              END IF
              
C             Reorder the data structure.
              CALL PSTPRCS (LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr,FlsOutVv,
     &             MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &             XNode,YNode)
C             
            END IF
C
          END DO
C
        END DO
C
      END DO
C
      IF (FViol) THEN
        Write (*,101)
  101   FORMAT (' Error during boundary check with CHKBND. After'/
     &          ' 9 sweeps still boundary violations were found.'/)
      END IF
C
      RETURN
      END

C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C

      SUBROUTINE FACECAVS (MVvr,NFrmNde,NghVvr,XNode,YNode,
     &     LsFlgVvr,FlsFlgVv,LsNde,LsNgh,MaxLsFlg,
     &     NNdePvt,NNdeNxt,NVvrPvt,MFlgVvr,MNdeIn,MNdeOut)
C
C     Last update:
C     28Mar97: created.
C     
C     Find the cavities around a missing edge.
C     Welcome to DOMINO software.
C     
C     Input:
C     MVvr:     Number of Elements in the grid.
C     X/YVvr:   Vectors of the coordinates of the Voronoi vertices.
C     NFrmNde:  Array of the nodal connectivity.
C     NghVvr:   Array of the three neighbors of each cell.
C     RadVvr:   Vector of the squared radii of the circumcircles.
C     FlsOutVv:
C     X/YNode:
C     MNde:
C     RadTolMi:
C     LsFlgVvr: Workspaces and their sizes.
C     FlsFlgVv:
C     LsNde:
C     LsNgh:
C     MaxLsFlg: Sizes of LsNgh, LsNde, LsFlgVvr.
C     NNdePvt: The two nodes that form the missing face. Going from NNdePvt
C     NNdeNxt: to NNdeNxt, the domain is to the left.
c     NVvrPvt: The cell that contains NNdePvt
C     
C     Output:
C     LsFlgVvr:
C     FlsFlgVv:
C     LsNde:
C     LsNgh:   The Neighbor. Note Ngh(i) is between Nde(i) and (i+1)
C     MNdeIn:  Number of nodes of the cavity on the side in the domain.
C     MNdeOut: Same outside the domain.
C     
      IMPLICIT NONE
      
      DOUBLE PRECISION XNode(*),YNode(*)
      INTEGER NFrmNde(3,*),NghVvr(3,*),LsFlgVvr(*),LsNde(*),LsNgh(*)
      LOGICAL*1 FlsFlgVv(*)
      INTEGER NNdePvt, NNdeNxt, NVvrPvt,
     &     MFlgVvr, MNdeIn, MNdeOut, MaxLsFlg, MVvr

      LOGICAL*1 Found
      INTEGER NVvr, KFace, NNde, NVvrNxt, JCYCL, NNdeIn,
     &     NNdeNew, KPvt, KNde
      DOUBLE PRECISION DXEdge, DYEdge, DEdge, XEdgeMid, YEdgeMid,
     &     DNdeIn(3), SIDE
C
C     Erase the vector of flags.
      DO NVvr = 5,MVvr
        FlsFlgVv(NVvr) = .FALSE.
      END DO
C
c     Calculate a tangent direction for the missing edge.
      DXEdge = XNode(NNdeNxt)-XNode(NNdePvt)
      DYEdge = YNode(NNdeNxt)-YNode(NNdePvt)
      DEdge = SQRT(DXEdge**2+DYEdge**2)
      DXEdge = DXEdge/DEdge
      DYEdge = DYEdge/DEdge
c     Calculate a midpoint on the edge.
      XEdgeMid = .5*(XNode(NNdeNxt)+XNode(NNdePvt))
      YEdgeMid = .5*(YNode(NNdeNxt)+YNode(NNdePvt))
      
c     Find the face number in NVvrPvt that intersects the edge. The
c     face is the one that has a forming node on either side of the edge.
      Found = .FALSE.
      DO KNde = 1,3
        NNde = NFrmNde(KNde,NVvrPvt)
        IF ( NNde .EQ. NNdePvt ) THEN
          Found = .TRUE.
          KPvt = KNde
          DNdeIn(KNde) = 0.
        ELSE
          DNdeIn(KNde) = SIDE( XNode(NNde), YNode(NNde),
     &                         XEdgeMid, YEdgeMid, DXEdge, DYEdge )
        END IF

      END DO
      IF ( .NOT. Found ) THEN
        WRITE (*,'(2(A,I7),A)')
     &       ' FATAL: could not find back pivot',NNdePvt,
     &       ' in cell',NVvrPvt,' in FACECAVS.'
        STOP
      END IF

      DO KFace = 1,3
        IF ( DNdeIn( JCYCL(KFace+1) ) .LT. 0 .AND.
     &       DNdeIn( JCYCL(KFace+2) ) .GT. 0 ) THEN
c         This edge intersects.
          IF ( KPvt .NE. KFace ) THEN
c           The first cell has to have an intersection on the face
c           opposite NNdePvt.
            WRITE (*,'(A)') ' FATAL: bad first cell in FACECAVS.'
            STOP
          ELSE
C           The list of nodes outside starts at 1 with NNdePvt. Nodes
c           are listed counterclockwise around the cavity.
            FlsFlgVv(NVvrPvt) = .TRUE.
            LsFlgVvr(1) = NVvrPvt
            MFlgVvr = 1
            LsNde(1) = NNdePvt
            LsNde(2) = NFrmNde( JCYCL(KPvt+1), NVvrPvt )
            LsNgh(1) = NghVvr( JCYCL(KPvt+2), NVvrPvt )
            MNdeOut = 2
            
c           The list of nodes inside starts at MaxLsFlg with NNdePvt and
c           runs backward to NNdeNxt. It is listed clockwise.
            LsNde(MaxLsFlg) = NNdePvt
            LsNde(MaxLsFlg-1) = NFrmNde( JCYCL(KPvt+2), NVvrPvt )
            LsNgh(MaxLsFlg) = NghVvr( JCYCL(KPvt+1), NVvrPvt )
            MNdeIn = 2
            NNdeIn = MaxLsFlg-1

c           Add the initial cell to the list.
            NVvr = NVvrPvt
            LsFlgVvr(1) = NVvr
            FlsFlgVv(NVvr) = .TRUE.
            MFlgVvr = 1
c           The cell that shares the crossing edge.
            NVvrNxt = NghVvr(KFace,NVvr)

c           As long as the GOTO is equivalent to C's break, I
c           consider it OK. Exit the do loop.
            GOTO 10
          END IF
        END IF
      END DO
      WRITE (*,'(A)') ' FATAL: no initial intersection in FACECAVS.'
      STOP
 10   CONTINUE

c     Loop over all cells intersecting the edge, ie until there
c     is an intersecting element that contains NNdeNxt.
      Found = .FALSE.
      DO WHILE ( .NOT. Found )
c       Find back NVvr in NVvrNxt.
        DO KFace = 1,3
          IF ( NghVvr(KFace,NVvrNxt) .EQ. NVvr ) THEN
c           This is the one. Find which side the opposite node
c           falls into.
            NVvr = NVvrNxt
            FlsFlgVv(NVvr) = .TRUE.
            MFlgVvr = MFlgVvr+1
            LsFlgVvr(MFlgVvr) = NVvr

c           The node opposite the shared face.
            NNde = NFrmNde(KFace,NVvr)
            IF ( NNde .EQ. NNdeNxt ) THEN
c             This is the end of the missing edge.
              Found = .TRUE.
c             Fix the ends and translate the inside cavity into counter-
c             clockwise sense.
              LsNde(MNdeOut+1) = NNdeNxt
              LsNgh(MndeOut) = NghVvr(JCYCL( KFace+1 ),NVvr)
              MNdeOut = MNdeOut+1

              LsNde(NNdeIn-1) = NNdeNxt
              LsNgh(NNdeIn) = NghVvr(JCYCL( KFace+2 ),NVvr)
              MNdeIn = MNdeIn+1

            ELSE IF ( SIDE( XNode(NNde), YNode(NNde),
     &             XEdgeMid, YEdgeMid, DXEdge, DYEdge ) .GT. 0 ) THEN
c             Inside. Ie. to the left of the edge.
              MNdeIn = MNdeIn+1
              NNdeIn = NNdeIn-1
              LsNde(NNdeIn) = NNde
              LsNgh(NNdeIn+1) = NghVvr( JCYCL( KFace+2 ), NVvr )
              NVvrNxt = NghVvr( JCYCL( KFace+1 ), NVvr )
            ELSE
c             Outside. Ie. to the right of the edge.
              MNdeOut = MNdeOut+1
              LsNde(MNdeOut) = NNde
              LsNgh(MNdeOut-1) = NghVvr( JCYCL( KFace+1 ), NVvr )
              NVvrNxt = NghVvr( JCYCL( KFace+2 ), NVvr )
            END IF
            GOTO 20
          END IF
        END DO
        WRITE (*,'(A)') ' FATAL: disjoint grid found in FACECAVS.'
        STOP

 20     CONTINUE
      END DO

c     Write the inside list at the end of the outside list in
c     counterclockwise sense.
      DO KNde = 1,MNdeIn-1
        NNdeIn = MaxLsFlg-MNdeIn+KNde
        NNdeNew = 2*MNdeOut+KNde
        LsNde(NNdeNew) = LsNde(NNdeIn)
        LsNgh(NNdeNew) = LsNgh(NNdeIn+1)
      END DO
      LsNde(2*MNdeOut+MNdeIn) = LsNde(MaxLsFlg)

      RETURN
      END


      
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      DOUBLE PRECISION FUNCTION SIDE ( XNode, YNode, XEdgeMid, YEdgeMid,
     &     DXEdge, DYEdge )
      
c     Calculate which side of an edge a node is on with a cross-product.

c     INPUT:
c     X/YNode:
c     X/YEdgeMid: The midpoint of the edge.
c     DX/YEdge: The direction of the edge.

c     RETURNS
c     SIDE: The vector-product, positive if Node is to the left of
c           Edge.
      
      IMPLICIT NONE
      DOUBLE PRECISION XNode, YNode, XEdgeMid, YEdgeMid,
     &     DXEdge, DYEdge, DXNode, DYNode

      DXNode = XNode-XEdgeMid
      DYNode = YNode-YEdgeMid

      SIDE = - DXNode*DYEdge + DYNode*DXEdge
      RETURN
      END
      
      
