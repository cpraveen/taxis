      SUBROUTINE BUILDNDE (XNorm,YNorm,NNde,NNdePrv,NNdeNxt,
     &                     NNewPrv,PBNNde,PFNew,NBnd,FoundNde,MNewNde,
     &                     LsFlgVvr,FlsFlgVv,FlsOutVv,FTruncate)
C
C Last update:
C 20May96;pass cell flag lists into SPCOLCHK.
C 20Apr94,13:35;intro new SPCOLCHK w/o common blocks.
C 2Dec93,15:15;intro DM1 for wakes.
C 23Aug93,19:05;intro FTruncate.
C 11Nov92,20:55;new BSTRINT.
C 25Jun92;19:15;new SPCOLCHK. Don't allow inversely stretched cells.
C 24Jun92;13:40;Pass DMax via /BKGRD/.
C 22Jun92,20:45;Pass NNdePrv and NNdeNxt to INTROVIS.
C 18Jun92,16:56;established.
C 
C Take the normal, the distance and the node and build a box with them.
C Welcome to DOMINO software.
C
C Input:
C X/YNorm:  Normal vector pointing from the node NNde to the new node
C           NNew to be built.
C NNde:     Number of the node to build from.
C NNdePrv:  Node preceding NNde on the line.
C NNdeNxt:  Node following NNde on the line.
C NNewPrv:  Node preceding NNew on the line.
C PBNNde:   Background cell close to NNde.
C PFNew:    Foreground cell close to NNew.
C NBnd:     Number of the boundary NNew is built from.
C FoundNde: Flag that indicates that nodes from NBnd for MBnd have
C           already been found.
C LsFlgVvr: List of flagged cells in the cavity (allocated in DELAUNDO).
C FlsFlgVv: Vector of flags for the cavity (allocated in DELAUNDO).
C FTruncate:IF .T., truncate the current string after this node.
C /NODES/:  Nodes, boundaries, frontal strings and all that.
C /VVRTC/:  Connectivity.
C /BKGRD/:  Background grid and related things.
C
C Output:
C PBNNde:   Background cell close to NNde.
C PFNew:    Foreground cell close to NNew.
C FTruncate:Always .F. on exit.
C FoundNde: Flag that indicates that nodes from NBnd for MBnd have
C           already been found.
C /NODES/:  More nodes, boundaries, frontal strings and all that.
C /VVRTC/:  Updated connectivity.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      REAL XNorm,YNorm,StrLin
      INCLUDE 'nodes.cbl'
      INCLUDE 'vvrtc.cbl'
      INCLUDE 'bkgrd.cbl'
      DIMENSION LsFlgVvr(*),FlsFlgVv(*),FlsOutVv(*)
C
C     Evaluate the stretching at NNde.
C      PBNNde = IWALK(XNode(NNde),YNode(NNde),PBNNde,XNode,YNode,
C     &               NBFrmNde,NghBVvr,MBVvr,FOutCnv)
      BSpc = BSPCINT(XNode(NNde),YNode(NNde),PBNNde,FOutCnv)
      BkgStrI = BSTRINT (XNode(NNde),YNode(NNde),PBNNde,
     &                   NBFrmNde,XNode,YNode,BkgStr,
     &                   BndStr,DomStr,StrPw,StrPwInv)
C
C     Normalize the normal.
      DNorm = SQRT(XNorm**2+YNorm**2)
      DM1 = MAX(DMax,BSpc)
      Alt = DM1/DNorm/BkgStrI
C
      IF (Alt.LT.1.) THEN
C       The face to build from is longer than the altitude of the box.
C       Evaluate the stretching gradient at the approximate position
C       of the new node.
        XNew = XNode(NNde)+XNorm*Alt
        YNew = YNode(NNde)+YNorm*Alt
        PBNew = PBNNde
        PBNew = IWALK(XNew,YNew,PBNew,XNode,YNode,
     &                NBFrmNde,NghBVvr,MBVvr,FOutCnv)
C
        IF (IArea(PBNew).EQ.IFront(NBnd)) THEN
C         Forming nodes of the background cell.
          NB1 = NBFrmNde(1,PBNew)
          NB2 = NBFrmNde(2,PBNew)
          NB3 = NBFrmNde(3,PBNew)
C         Coordinates.
          X1 = XNode(NB1)
          X2 = XNode(NB2)
          X3 = XNode(NB3)
          Y1 = YNode(NB1)
          Y2 = YNode(NB2)
          Y3 = YNode(NB3)
C         Coordinate differences.
          X21 = X2-X1
          X32 = X3-X2
          X13 = X1-X3
          Y21 = Y2-Y1
          Y32 = Y3-Y2
          Y13 = Y1-Y3
C         Linear stretching values.
          VP1 = BkgStr(NB1)
          VP2 = BkgStr(NB2)
          VP3 = BkgStr(NB3)
C         Calculate the area.
          Area = .5*(X32*Y13-X13*Y32)
C         Calculate the gradient in linear stretching.
          GradSX = -.5*(Y32*VP1+Y13*VP2+Y21*VP3)/Area
          GradSY = .5*(X32*VP1+X13*VP2+X21*VP3)/Area
          GradS = SQRT(GradSX**2+GradSY**2)
C
C         Compare normal and the gradient.
          CosAlpha = -(XNorm*GradSX+YNorm*GradSY)/GradS/DNorm
          IF (CosAlpha.LT..985) THEN
C           Allow only 10 degrees misalignment between the gradient
C           and the normal.
            SinAlpha = XNorm*GradSY-YNorm*GradSX
            IF (SinAlpha.GT.0.) THEN
C             Use the gradient as normal rotated 30 degrees clockwise.
              AltProj = DM1/BkgStrI/GradS
              XNorm = -(.985*GradSX-.173*GradSY)*AltProj
              YNorm = -(.985*GradSY+.173*GradSX)*AltProj
            ELSE 
C             Rotate 10 degrees counterclockwise.
              AltProj = DM1/BkgStrI/GradS
              XNorm = -(.985*GradSX+.173*GradSY)*AltProj
              YNorm = -(.985*GradSY-.173*GradSX)*AltProj
            END IF
C
          ELSE
C           Gradient and normal properly aligned, scale the normal
C           to keep layers of constant thickness.
            AltProj = Alt/CosAlpha
            XNorm = XNorm*AltProj
            YNorm = YNorm*AltProj
C
          END IF
C
          NNew = MNde+MNewNde+1
          XNode(NNew) = XNode(NNde)+XNorm
          YNode(NNew) = YNode(NNde)+YNorm
C         Check whether the new node lies in the proper area.
          PBNew = IWALK(XNode(NNew),YNode(NNew),PBNew,XNode,YNode,
     &                NBFrmNde,NghBVvr,MBVvr,FOutCnv)
C
          IF (.NOT.FOutCnv.AND.IArea(PBNew).EQ.IFront(NBnd)) THEN
C           Check the spacing with the rest of the old nodes.
            SpcNew = DM1/BkgStrI
            CALL SPCOLCHK (NNew,PFNew,PBNew,SpcNew,
     &                     FSpcOK,NdeX,
     &                     MNde,XNode,YNode,
     &                     MVvr,NFrmNde,NghVvr,XVvr,YVvr,
     &                     MBVvr,NBFrmNde,BkgStr,FBkgStr,
     &                     BndStr,DomStr,StrPw,StrPwInv,
     &                     DTol2,DMax,
     &                     LsFlgVvr,FlsFlgVv,2*MaxBndNde)
C           Find the foreground cell containing NNew.
            PFNew = IWALK(XNode(NNew),YNode(NNew),PFNew,XNode,YNode,
     &                    NFrmNde,NghVvr,MVvr,FOutCnv)
            IF (FSpcOK.AND..NOT.FlsOutVv(PFNew)) THEN
C             Use this node. 
              FoundNde = .TRUE.
              MNewNde = MNewNde+1
              CALL INTROVIS (NNew,NNde,NNdePrv,NNdeNxt,NNewPrv,PFNew,
     &                       FlsOutVv,LsFlgVvr,FlsFlgVv)
            END IF
C
          ELSE IF (FoundNde) THEN
C           Don't use this node. Finish the previously started segment.
            FTruncate = .TRUE.
C
          END IF
C
        ELSE IF (FoundNde) THEN
C         Don't use this node. Finish the previously started segment.
          FTruncate = .TRUE.
C
        END IF
C
      ELSE IF (FoundNde) THEN
C       Don't use this node. Finish the previously started segment.
        FTruncate = .TRUE.
C
      END IF
C
      IF (FTruncate) THEN
C       Finish the previously started segment.
        FTruncate = .FALSE.
        MNde = MNde+MNewNde
        NdxLBnNd(MBnd) = MNde
        MNewNde = 0
C       Open a new frontal segment.
        FoundNde = .FALSE.
        MBnd = MBnd+1
        IFront(MBnd) = IFront(NBnd)
        IBndType(MBnd) = 11
      END IF
C
      RETURN
      END
