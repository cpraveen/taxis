      SUBROUTINE FNDNDE (N1,N2,NBVvr,MNewNde,Found,SpcNew,
     &                   MNde,XNode,YNode,
     &                   MBVvr,NBFrmNde,NghBVvr,
     &                   BkgSpc,BkgVol,DMax,SpcPw,
     &                   BkgStr,BndStr,DomStr,StrPw,StrPwInv,FBkgStr,
     &                   QTolRtHi,QTolRtLo,NLevel)
C
C Last update:
C 9Jan97; check the length of the frontal edge and cut long ones in half.
C 27Sep94;limit GradSpcUntN for positive SNrml.
C 8May94,14:30;eliminate quality limits on SNrml for MLevel>1.
C 2Dec93,15:05;intro DM1 for wakes.
C 11Nov92,21:00;new BSTRINT.
C 28Jun92,21:55;update Found only if a new node has been found.
C 25Jun92,16:25;new SPCNWCHK.
C 24Jun92,17:58;apply StrBkgI to Spcing.
C 22Jun92,11:13;SpcBnd to BkgSpc, SBVol to BkgVol and SpcInt to BSpcInt.
C 17Jul91, 12:33. (Remove bug in call to BSpcInt.)
C 12Jul91, 17:35. (Found set to .T. only if new node constructed.)
C 5Jul91, 19:35. (Eliminating NghNde.)
C 19:09, 9Apr91.
C
C Find a face of a triangle with an unsufficient side length ratio
C to refine on. Find a node for each such face and check spacing
C among the nodes
C
C Input: 
C N1,N2:    Numbers of the two nodes of the base in counter clock-
C           wise order.
C NBVvr:    Background cell to start the search with.
C MNewNde:  Amount of new nodes to be introduced.
C SpcNew:
C MNde:
C XNode:
C YNode:
C MBVvr:
C NBFrmNde:
C NghBVvr:
C BkgSpc:
C BkgVol:
C DMax:
C SpcPw:
C BkgStr:
C BndStr:
C DomStr:
C StrPw:
C StrPwInv:
C FBkgStr:
C QTolRtHi:
C QTolRtLo:
C MLevel:
C
C Output:
C MNewNde:  Updated amount of new nodes to be introduced.
C Found:    Flag to indicate that a bad cell has been detected.
C NBVvr:    Background cell that contains the new node.
C XNode:
C YNode:
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION XNode(*),YNode(*),NBFrmNde(3,*),NghBVvr(3,*),
     &          BkgSpc(*),BkgVol(*),FBkgStr(*),BkgStr(*)
      REAL SpcPw,StrPw,StrPwInv,SNrml
      Found = .FALSE.
      F_DO_BKG_STR = .FALSE.
C
C     Midside of the shortest face:
      XMid = .5D0*(XNode(N1)+XNode(N2))
      YMid = .5D0*(YNode(N1)+YNode(N2))
C     Normal on the shortest face:
      XNrml = YNode(N1)-YNode(N2)
      YNrml = XNode(N2)-XNode(N1)
      RNrml = DSQRT(XNrml**2+YNrml**2)
C
C     Find the background cell that contains the node.
C      XEqu = XMid+.866D0*XNrml
C      YEqu = YMid+.866D0*YNrml
C     Use extrapolation from the point xM+n.
      XEqu = XMid+XNrml
      YEqu = YMid+YNrml
      PBEqu = IWALK(XEqu,YEqu,NBVvr,XNode,YNode,NBFrmNde,NghBVVr,
     &              MBVvr,FOutCnv)
C     Return if the proposed node is outside the convex hull.
      IF (FOutCnv) RETURN
C
C     Three forming nodes of the background cell.
      NB1 = NBFrmNde(1,PBEqu)
      NB2 = NBFrmNde(2,PBEqu)
      NB3 = NBFrmNde(3,PBEqu)
C     Spacing gradient of the background cell.
      XGradSpc = (-BkgSpc(NB1)*(YNode(NB3)-YNode(NB2))-
     &            BkgSpc(NB2)*(YNode(NB1)-YNode(NB3))-
     &            BkgSpc(NB3)*(YNode(NB2)-YNode(NB1)))/
     &           2.D0/BkgVol(PBEqu)
      YGradSpc = (-BkgSpc(NB1)*(XNode(NB2)-XNode(NB3))-
     &            BkgSpc(NB2)*(XNode(NB3)-XNode(NB1))-
     &            BkgSpc(NB3)*(XNode(NB1)-XNode(NB2)))/
     &           2.D0/BkgVol(PBEqu)
C
C     Interpolated spacing on the background mesh.
c      PBMid = PBEqu
c      BSpc = BSPCINT(XMid,YMid,PBMid,FOutCnv)
C     Extrapolate from x5=xEqu=xM+n.
      BSpc = BSPCINT(XEqu,YEqu,PBEqu,FOutCnv)
C
C     Compare the length of the edge to the background spacing at
C     the midedge.
      REdge = SQRT((XNode(N1)-XNode(N2))**2 +
     &             (YNode(N1)-YNode(N2))**2 )
      GradHN = XNrml*XGradSpc+YNrml*YGradSpc
      BSpcMidEdge = BSpc - GradHN
      IF ( 2.*BSpcMidEdge .LT. REdge ) THEN
C       The Edge is more than twice as long than the required spacing.
C       Check exactly at the midedge.
        BSpcMidEdge = BSPCINT(XMid,YMid,PBEqu,FOutCnv)
        IF ( 2.*BSpcMidEdge .LT. REdge ) THEN
C         Place the new vertex at the midedge, and append it to the list.
          Found = .TRUE.
          MNewNde = MNewNde+1
          NNew = Mnde+MNewNde
          XNode(NNew) = XMid
          YNode(NNew) = YMid
          SpcNew = BSpcMidEdge
C         
          RETURN
        END IF
      END IF
C
C     Find out whether PCell is a cell in the viscous layer and
C     evaluate the background lengthscale. 
      IF (FBkgStr(PBEqu) .AND. F_DO_BKG_STR ) THEN
        BkgStrI = BSTRINT(XEqu,YEqu,PBEqu,NBFrmNde,XNode,YNode,BkgStr,
     &                    BndStr,DomStr,StrPw,StrPwInv)
        DM1 = MAX(DMax,BSpc)
        BSpc = MIN(BSpc,DM1/BkgStrI)
        GradFac = MAX(0.,MIN(BkgStrI-1.,1.))
        XGradSpc = GradFac*XGradSpc
        YGradSpc = GradFac*YGradSpc
      END IF
C
C     Length of the normal for the counterclockwise triangle
C     as extrapolated from xM.
C      SNrml = BSpc/RNrml/
C     &        (1.1547D0-.5D0*(XUntNrml*XGradSpc+YUntNrml*YGradSpc))
C     Extrapolate from x5=xEqu=xM+n.
      GradHUntN = GradHN/RNrml
C     Limit GradHUntN for SNrml to exclude negative SNrml.
      BSpcByRNrml = BSpc/RNrml
      GradHUntNLim = MIN(GradHUntN,BSpcByRNrml,2.3094)
      SNrml = 2.*(BSpcByRNrml-GradHUntNLim)/(2.3094-GradHUntN)
C     Limit this length to inhibit skewed triangles.
      SNrml = MIN(QTolRtHi,SNrml)
      SNrml = MAX(SNrml,QTolRtLo)
C
C     Append the new node to the list.
      Found = .TRUE.
      MNewNde = MNewNde+1
      NNew = Mnde+MNewNde
      XNode(NNew) = XMid+SNrml*XNrml
      YNode(NNew) = YMid+SNrml*YNrml
      SpcNew = BSpcInt(XNode(NNew),YNode(NNew),NBVvr,FOutCnv)
C
      RETURN
      END
