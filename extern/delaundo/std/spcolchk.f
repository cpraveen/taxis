      SUBROUTINE SPCOLCHK (NNew,PFNew,PBNew,SpcNew,
     &                     FSpcOK,NodeX,
     &                     MNde,XNode,YNode,
     &                     MVvr,NFrmNde,NghVvr,XVvr,YVvr,
     &                     MBVvr,NBFrmNde,BkgStr,FBkgStr,
     &                     BndStr,DomStr,StrPw,StrPwInv,DTol2,DMax,
     &                     LsFlgVvr,FlsFlgVvr,MaxFlgVvr)
C
C Last update:
C 20May96; intro BOXWALK
C 27Apr94,13:33; new IVVALK.
C 11.Jan.94,14:05; concieved, using the Dirichlet tesselation.
C
C Test the new node for proper spacing with all old nodes already
C introduced into the structure. Welcome to DOMINO software.
C
C Input: 
C NNew:     Number of the node to check.
C PFNew:    Pointer to the foreground cell containing NNde.
C PBNew:    Pointer to the background cell containing NNde.
C SpcNew:   Approximate spacing at the new node.
C FlsFlgVv: Flag vector, dimensioned in DELAUNDO.
C /NODES/:  Coordinates of the nodes. See main program
C           DELAUNDO for details.
C /VVRTC/:  Old connectivity. See main program DELAUNDO for 
C           details.
C Output:
C FSpcOK:   .TRUE. if the node is properly spaced.
C NodeX:    A node that is too close to NNew on FSpcOK = .T.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION XNode(*),YNode(*),NFrmNde(3,*),NghVvr(3,*),XVvr(*),
     &          YVvr(*),FBkgStr(*),NBFrmNde(3,*),BkgStr(*),
     &          LsFlgVvr(MaxFlgVvr),FlsFlgVvr(MaxFlgVvr)
      REAL StrPw,StrPwInv

C     Find the closest Voronoi vertex.
      XNew = XNode(NNew)
      YNew = YNode(NNew)
C     Disregard the level information and pass NFrmNde as bogus ILevel.
      CALL IVVALK(XNew,YNew,PFNew,NodeX,
     &            XVvr,YVvr,NFrmNde,NghVvr,MVvr,
     &            FOutCnv,NFrmNde,NghLevelMax)
      IF ( NodeX .LT. 0 ) THEN
C       Unrecoverable error in IVVALK.
        STOP
      ELSE IF ( NodeX .EQ. 0 ) THEN
C       IVVALK failed. Try boxwalk.
        CALL BOXWALK ( XNew,YNew,PFNew,NodeX,NFrmNde,NghVvr,MVvr,
     &                 XNode,YNode,MNde,
     &                 LsFlgVvr,FlsFlgVvr,MaxFlgVvr,FOutCnv)
      END IF
C
      Dist = (XNew-XNode(NodeX))**2+(YNew-YNode(NodeX))**2
      DSpcSqNw = DTol2*SpcNew**2
      IF (Dist.LT.DSpcSqNw) THEN
C       NNew might be too close to NodeX. Check exactly.
        XMid = .5D0*(XNode(NNew)+XNode(NodeX))
        YMid = .5D0*(YNode(NNew)+YNode(NodeX))
        BSpcMid = BSPCINT(XMid,YMid,PBNew,FOutCnv)
        IF (FBkgStr(PBNew)) THEN
          BkgStrI = BSTRINT(XMid,YMid,PBNew,NBFrmNde,XNode,YNode,
     &                      BkgStr,BndStr,DomStr,StrPw,StrPwInv)
C         Transformed distance.
          BSpcMid = MIN(BSpcMid,DMax/BkgStrI)
        END IF
        IF (Dist.LT.DTol2*BSpcMid**2) THEN
C         These two are indeed too close.
          FSpcOK = .FALSE.
          RETURN
        END IF
      END IF
C
      FSpcOK = .TRUE.
      RETURN
      END




