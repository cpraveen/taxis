      SUBROUTINE FNDVSCND (NrNde,RadVvr,XVvr,YVvr,PCellIni,FDoIntr,
     &                     XNode,YNode,NdxLBnNd,MBnd,IBndType,
     &                     NvNmBn,NmNghLNd,MNde,MNewNde,
     &                     BTol,DltStar,BndSTr,DomStr,DMax,
     &                     BkgStr,BkgSpc,NdePrio,IFront)
C
C Last update:
C 1Dec93,14:25; fix bug with connected segments by introducing IFront.
C 27Nov93,16:15; cap BkgSpc only for IBndType=1 boundaries.
C
C For a given triangle grid, find a background node to delimit a
C stretching layer. Welcome to DOMINO software.
C
C Input:
C NrNde:    Vector of forming nodes of the triangle.
C RadVvr:   Squared radius of the circumcircle of the triangle.
C X/YVvr:   Circumcenter of the triangle.
C PCellIni: Pointer to a node to start the search with.
C FDoIntr:  .TRUE. if nodes also along concave boundary segments are
C           to be produced.
C X/YNode:  Coordinates of all the nodes.
C NdxLBnNd: Vector of pointers to last nodes on boundaries.
C MBnd:     Number of boundaries.
C IBndType: Vector of boundary types.
C NvNmBn:   Vector relating boundary names to boundary numbers.
C NmNghLNd: Vector of names of the boundaries connected to the last
C           node of each segment.
C MNde:     Number of nodes in the mesh.
C MNewNde:  Number of newly constructed nodes waiting to be introduced.
C BTol:     Distance tolerance for nodes in the background grid.
C DltStar:  Thickness of the stretched layer.
C BndStr:   Maximum aspect ratio in the stretched layer.
C DomStr:   Stretching value in the isotropic region.
C DMax:     Maximum face length of any frontal boundary.
C BkgStr:   Stretching values at the nodes.
C BkgSpc:   Spacing values at the nodes.
C NdePrio:  Workspace of at least MNewNde+1.
C IFront:   Name of the element each frontal segment belongs to.
C
C Output:
C X/YNode:  Coordinates of all the nodes.
C MNewNde:  Number of newly constructed nodes waiting to be introduced.
C BkgStr:   Stretching values at the nodes.
C BkgSpc:   Spacing values at the nodes.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DIMENSION NrNde(3),NdxLBnNd(0:*),NrBnd(3),IBndType(*),
     &          FNdeBnd(3),FFceBnd(3),BkgStr(*),BkgSpc(*),
     &          NdePrio(*),NvNmBn(-1:*),NmNghLNd(*),IFront(*)
      DOUBLE PRECISION XNode(*),YNode(*),RadVvr,XVvr,YVvr,XNew,YNew
C
C     Find out how many boundary nodes it has. And set an upper 
C     limit of node spacing for a type 1 boundary.
      DM1 = 1.E25 
      MNdeBnd = 0
      DO KNde = 1,3
        IF (NrNde(KNde).LE.NdxLBnNd(MBnd)) THEN
C         This is a boundary node.
C
C         Find the boundary number of this node.
          NrBnd(KNde) = 1
          DO WHILE (NdxLBnNd(NrBnd(KNde)).LT.NrNde(KNde))
            NrBnd(KNde) = NrBnd(KNde)+1
          END DO
C
          IF (IBndType(NrBnd(KNde)).EQ.1) THEN
C           This is a physical boundary with frontal character 1.
C           Work on this one and cap BkgSpc to DMax via setting DM1.
            FNdeBnd(KNde) = .TRUE.
            MNdeBnd = MNdeBnd+1
            DM1 = DMax
          ELSE IF(IBndType(NrBnd(KNde)).EQ.4) THEN
C           This is a physical boundary with frontal character 4.
C           Work on this one.
            FNdeBnd(KNde) = .TRUE.
            MNdeBnd = MNdeBnd+1
          ELSE
C           This is a any non-frontal boundary. Ignore it.
            FNdeBnd(KNde) = .FALSE.
          END IF
C
        ELSE
C         This is an interior node.
          FNdeBnd(KNde) = .FALSE.
C
        END IF
      END DO
C
C     Find the number of boundary faces in this triangle.
      MFceBnd = 0
      DO KNde = 1,3
        IF (IBndType(NrBnd(KNde)).EQ.1.OR.
     &      IBndType(NrBnd(KNde)).EQ.4) THEN
          IF (NrNde(KNde).EQ.NdxLBnNd(NrBnd(KNde))) THEN
C           This node is the last one on its segment.
C           The next node is the first on the following segment.
            NNdeNxt = NdxLBnNd(NvNmBn(NmNghLNd(NrBnd(KNde)))-1)+1
          ELSE
C           The next node is the following on the same segment.
            NNdeNxt = NrNde(KNde)+1
          END IF
C
          IF (NrNde(JCYCL(KNde+1)).EQ.NNdeNxt) THEN
C           This is a face.
            MFceBnd = MFceBnd+1
            FFceBnd(JCYCL(KNde+2)) = .TRUE.
            FNdeBnd(KNde) = .FALSE.
            FNdeBnd(JCYCL(KNde+1)) = .FALSE.
            MNdeBnd = MNdeBnd-2
          ELSE
            FFceBnd(JCYCL(KNde+2)) = .FALSE.
          END IF
C
        ELSE 
C         This is not a frontal boundary, no face.
          FFceBnd(JCYCL(KNde+2)) = .FALSE.
C
        END IF
      END DO
C
      IF (MFceBnd.EQ.2) THEN
C       Combine these two boundaries and introduce a new node
C       at the common corner.
        MFceBnd = 0
        MNdeBnd = 1
        FNdeBnd(1) = .FALSE.
        FNdeBnd(2) = .FALSE.
        FNdeBnd(3) = .FALSE.
        IF (FFceBnd(1).AND.FFceBnd(2)) FNdeBnd(3) = .TRUE.
        IF (FFceBnd(2).AND.FFceBnd(3)) FNdeBnd(1) = .TRUE.
        IF (FFceBnd(3).AND.FFceBnd(1)) FNdeBnd(2) = .TRUE.
      END IF
C
      IF (MNdeBnd.GT.0.) THEN
C       Insert a new node at a corner.
C
C       Find out whether the distance from the three forming nodes
C       to the Voronoi vertex is smaller than the required
C       streching layer thickness DltStar or not.
        DR = SQRT(RadVvr)
        IF (DR.LT.DltStar) THEN
          IF (IFront(NrBnd(1)).NE.IFront(NrBnd(2)).OR.
     &        IFront(NrBnd(1)).NE.IFront(NrBnd(3)).OR.
     &        FDoIntr) THEN
C           Place one new node at the Voronoi vertex. This extra
C           node then covers all remaining refinement requests.
C           Set the priority according to the number of requests.
            NNde = MNde+MNewNde+1
            NdePrio(MNewNde+1) = MNdeBnd
C
C           Check whether the new node is properly spaced.
            BkgStr(NNde) = BndStr-DR/DltStar*(BndStr-DomStr)
            BkgSpc(NNde) = MIN(DM1,BSPCINT(XVvr,YVvr,PCellIni,FOutCnv))
            CALL BSPCCHK (XVvr,YVvr,BkgSpc(NNde),
     &                    XNode(MNde+1),YNode(MNde+1),
     &                    BkgSpc(MNde+1),BTol,MNewNde,PCellIni,
     &                    NLnkNde,FSpcOK)
C
            IF (FSpcOK) THEN
C             Node wellspaced, append to the list.
              MNewNde = MNewNde+1
              XNode(NNde) = XVvr
              YNode(NNde) = YVvr
C
            ELSE IF (NdePrio(NLnkNde).LT.NdePrio(MNewNde+1)) THEN
C             Node too close to node NLnkNde. Eliminate the one
C             with the lower priority.
              XNode(MNde+NLnkNde) = XVvr
              YNode(MNde+NLnkNde) = YVvr
              BkgSpc(MNde+NLnkNde) = BkgSpc(NNde)
              BkgStr(MNde+NLnkNde) = BkgStr(NNde)
              NdePrio(NLnkNde) = NdePrio(MNewNde+1)
C
            END IF
C
C           Erase the counters for the other requests.
            MNdeBnd = 0
            MFceBnd = 0
C
          END IF
C
        ELSE
C         Place a new node along the line connecting the Voronoi
C         vertex and the node with distance DltStar from the node.
          DO KNde = 1,3
            IF (FNdeBnd(KNde)) THEN
              DX = XVvr-XNode(NrNde(KNde))
              DY = YVvr-YNode(NrNde(KNde))
              DR = SQRT(DX**2+DY**2)
              XNew = XNode(NrNde(KNde))+DX/DR*DltStar
              YNew = YNode(NrNde(KNde))+DY/DR*DltStar
C  
C             Check whether the new node is properly spaced.
              BSpcNew = MIN(DM1,BSPCINT(XNew,YNew,PCellIni,FOutCnv))
              CALL BSPCCHK (XNew,YNew,BSpcNew,XNode(MNde+1),
     &                      YNode(MNde+1),BkgSpc(MNde+1),BTol,
     &                      MNewNde,PCellIni,NLnkNde,FSpcOK)
C
              IF (FSpcOK) THEN
C               Node wellspaced, append to the list.
                MNewNde = MNewNde+1
                NNde = MNde+MNewNde
                XNode(NNde) = XNew
                YNode(NNde) = YNew
                BkgSpc(NNde) = BSpcNew
                BkgStr(NNde) = DomStr
                NdePrio(MNewNde) = 2
C
              ELSE IF (NdePrio(NLnkNde).LT.2) THEN
C               Node too close to node NLnkNde. Eliminate the one
C               with the lower priority.
                XNode(MNde+NLnkNde) = XNew
                YNode(MNde+NLnkNde) = YNew
                BkgSpc(MNde+NLnkNde) = BSpcNew
                BkgStr(MNde+NLnkNde) = DomStr
                NdePrio(NLnkNde) = 2
C
              END IF
C
            END IF
          END DO
        END IF
C
      END IF
C
      IF (MFceBnd.GT.0) THEN
C       Insert a new node normal to a face. Note that there may
C       be only one boundary face. The case with two faces which
C       necessarily have to be consecutive has been treated with
C       one node assigned to the corner. The case of three 
C       boundary faces is irrelevant.
C
C       Find out whether the distance from the three forming nodes
C       to the Voronoi vertex is smaller than the required
C       streching layer thickness DltStar or not.
        DR = SQRT(RadVvr)
        IF (DR.LT.DltStar) THEN
          IF (IFront(NrBnd(1)).NE.IFront(NrBnd(2)).OR.
     &        IFront(NrBnd(1)).NE.IFront(NrBnd(3))) THEN
C           Place one new node at the Voronoi vertex. This extra
C           node then covers all remaining refinement requests.
C           Set the priority according to the number of requests.
C           Note that this case can only occur if the node oposite
C           the face is not a boundaey node, thus very unlikely.
            NNde = MNde+MNewNde+1
            NdePrio(MNewNde+1) = 3
C
C           Check whether the new node is properly spaced.
            BkgStr(NNde) = BndStr-DR/DltStar*(BndStr-DomStr)
            BkgSpc(NNde) = MIN(DM1,BSPCINT(XVvr,YVvr,PCellIni,FOutCnv))
            CALL BSPCCHK (XVvr,YVvr,BkgSpc(NNde),
     &                    XNode(MNde+1),YNode(MNde+1),
     &                    BkgSpc(MNde+1),BTol,MNewNde,
     &                    PCellIni,NLnkNde,FSpcOK)
C
            IF (FSpcOK) THEN
C             Node wellspaced, append to the list.
              MNewNde = MNewNde+1
              XNode(NNde) = XVvr
              YNode(NNde) = YVvr
C
            ELSE IF (NdePrio(NLnkNde).LT.NdePrio(MNewNde+1)) THEN
C             Node too close to node NLnkNde. Eliminate the one
C             with the lower priority.
              XNode(MNde+NLnkNde) = XVvr
              YNode(MNde+NLnkNde) = YVvr
              BkgSpc(MNde+NLnkNde) = BkgSpc(NNde)
              BkgStr(MNde+NLnkNde) = BkgStr(NNde)
              NdePrio(NLnkNde) = NdePrio(MNewNde+1)
C
            END IF
C  
          END IF
        ELSE
C         Place a new node along the line connecting the Voronoi
C         vertex and the midside with distance DltStar.
          DO KFce = 1,3
            IF (FFceBnd(KFce)) THEN
              NNdeR = NrNde(JCYCL(KFce-1))
              NNdeL = NrNde(JCYCL(KFce-2))
              XMid = .5D0*(XNode(NNdeR)+XNode(NNdeL))
              YMid = .5D0*(YNode(NNdeR)+YNode(NNdeL))
              DX = XVvr-XMid
              DY = YVvr-YMid
              DR = SQRT(DX**2+DY**2)
              XNew = XMid+DX/DR*DltStar
              YNew = YMid+DY/DR*DltStar
C
C             Check whether the new node is properly spaced.
              BSpcNew = MIN(DM1,BSPCINT(XNew,YNew,PCellIni,FOutCnv))
              CALL BSPCCHK (XNew,YNew,BSpcNew,XNode(MNde+1),
     &                      YNode(MNde+1),BkgSpc(MNde+1),BTol,
     &                      MNewNde,PCellIni,NLnkNde,FSpcOK)
C
              IF (FSpcOK) THEN
C               Node wellspaced, append to the list. Note that
C               this type of nodes has lowest priorities, hence
C               we don't have to compare with linked nodes.
                MNewNde = MNewNde+1
                NNde = MNde+MNewNde
                XNode(NNde) = XNew
                YNode(NNde) = YNew
                BkgSpc(NNde) = BSpcNew
                BkgStr(NNde) = DomStr
                NdePrio(MNewNde) = 1
              END IF
C
            END IF
          END DO
        END IF
C
      END IF
C
      RETURN
      END
 
