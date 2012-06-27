      SUBROUTINE SPCNWCHK (NNew,MNewNde,SpcNew,PBVvr,FSpcOK)
C
C Last update:
C 11Nov92,21:00; new BSTRINT.
C 25Jun92,16:20;eliminate INew.
C 24Jun92,14:55;use DMax/BkgStrI isotropically.
C 28May92,14:26:Get rid of integers in LOGICAL.
C 23Mar92, 18:00. (fill SpcNew(INew) in SPCNWCHK.)
C 18Mar92, 16:45. (extracted from FNDNDE.)
C
C Check whether the new nodes are sufficiently spaced among each other
C considering non-Euclidean distance. Welcome to DOMINO software.
C
C Input:
C NNew:      Number of the new node to check in the full list of nodes.
C MNde:      Number of old nodes.
C MNewNde:   Number of new nodes.
C SpcNew:    Approximation for the spacing at the new node.
C PBVvr:     Startpoint for the walk towards the parent background cell.
C /NODES/:   Coordinates of the nodes. See main program DELAUNDO for
C            details.
C
C Output:
C SpcNew:   Approximation for the spacing at the new node.
C PBVvr:     Parent background cell.
C FSpcOK:    .TRUE. if the new node is properly spaced.
C /NODES/:   Coordinates of the nodes. See main program DELAUNDO for
C            details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      INCLUDE 'vvrtc.cbl'
      INCLUDE 'bkgrd.cbl'
      DIMENSION SpcNew(*)
C
C     Check whether the first new node is sufficiently spaced with
C     the other new ones. Loop backwards to join preferably nodes
C     constructed on the same triangle.
      DO NNde = MNde+MNewNde,MNde+1,-1
C
C       Euclidean distance between the nodes.
        DX = XNode(NNew)-XNode(NNde)
        DY = YNode(NNew)-YNode(NNde)
        Dist = DX**2+DY**2
C
        IF (Dist.LT..25*(SpcNew(NNew-MNde)+SpcNew(NNde-MNde))**2)
     &  THEN
C         These nodes are close. Check exactly.
          XMid = .5D0*(XNode(NNew)+XNode(NNde))
          YMid = .5D0*(YNode(NNew)+YNode(NNde))
          BSpc = BSPCINT(XMid,YMid,PBVvr,FOutCnv)
          IF (FBkgStr(PBVvr)) THEN
            BkgStrI = BSTRINT(XMid,YMid,PBVvr,NBFrmNde,XNode,YNode,
     &                        BkgStr,BndStr,DomStr,StrPw,StrPwInv)
C           Transformed distance to the node.
            Bspc = MIN(BSpc,DMax/BkgStrI)
          END IF
          IF (Dist.LT.DTol2*BSpc**2) THEN
C           The new node is too close to this new node. Merge.
            FSpcOK = .FALSE.
            XNode(NNde) = .5D0*(XNode(NNde)+XNode(NNew))
            YNode(NNde) = .5D0*(YNode(NNde)+YNode(NNew))
            RETURN
          END IF
        END IF
C         
      END DO
C
      FSpcOK = .TRUE.
      RETURN
      END 
