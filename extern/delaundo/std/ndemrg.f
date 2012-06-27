      SUBROUTINE NDEMRG (NNdeX,NNew,PFVvr,PBVvr,XNode,YNode,MNde,
     &                   MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                   FlsOutVv,LsFlgVvr,FlsFlgVv,
     &                   LsNde,LsNgh,SpcNew,RadTolMi,
     &                   MBVvr,NBFrmNde,BkgStr,FBkgStr,
     &                   BndStr,DomStr,StrPw,StrPwInv,DTol2,DMax,
     &                   MaxLsFlg)
C
C Last update:
C 20May96; new SPCOLCHK.
C 
C Extract a node from the DT, merge it with a given node, check
C the spacing condition of the new node and in case of proper
C spacing, reintroduce the merged node. Welcome to DOMINO software.
C
C Input: 
C NNdeX:    Node in the existing DT.
C NNew:     Node that conflicts with NNdeX, not introduced.
C PFVvr:    Pointer to a foreground cell in the vicinity of NNdeX.
C PBVvr:    Pointer to a background cell in the vicinity of NNdeX.
C X/YNode:  Coordinates of the nodes.
C MNde:     Total number of vertices in the mesh.
C MVvr:     Total number of Voronoi vertices in the mesh.
C X/YVvr:   Coordinates of the voronoi vertices.
C NFrmNde:  Forming nodes of the triangles.
C NghVvr:   Neighbors of the triangles.
C RadVvr:   circumradii of the cells.
C FlsOutVv: Vector of flags marking cells outside the domain.
C LsFlgVvr: Workspace for the introduction and extraction routines.
C FlsFlgVv: Workspace for the introduction and extraction routines.
C LsNde:    Workspace for NODEX.
C LsNgh:    Workspace for NODEX.
C SpcNew:   Spacing at NNew
C RadTolMi: Inner rim of the circumcircle.
C MaxLsFlg: size of LsNde,LsNgh,LsFlgVvr
C
C Output:
C X/YNode:  Coordinates of the nodes.
C MVvr:     Total number of Voronoi vertices in the mesh.
C X/YVvr:   Coordinates of the voronoi vertices.
C NFrmNde:  Forming nodes of the triangles.
C NghVvr:   Neighbors of the triangles.
C RadVvr:   circumradii of the cells.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      REAL SpcPw,StrPw,StrPwInv
      DIMENSION LsFlgVvr(*),FlsFlgVv(*),FlsOutVv(*),XNode(*),YNode(*),
     &          XVvr(*),YVvr(*),NFrmNde(3,*),NghVvr(3,*),RadVvr(*),
     &          LSNde(*),LsNgh(*),NBFrmNde(3,*),BkgStr(*),FBkgStr(*)
C
C     Extract NNdeX from the DT.
      CALL NDEX(NNdeX,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,
     &          NghVvr,RadVvr,FlsOutVv,XNode,YNode,MNde,
     &          LsFlgVvr,FlsFlgVv,LsNde,LsNgh)
C
C     Merge it with NNew.
      XNode(NNew) = .5D0*(XNode(NNdeX)+XNode(NNew))
      YNode(NNew) = .5D0*(YNode(NNdeX)+YNode(NNew))
C
C     Check the spacing of the merged node. Note that the determination
C     of the conflicting vertex NdeXX is irrelevant, since in case of
C     improper spacing we retain NNdeX in the mesh.
      CALL SPCOLCHK (NNew,PFVvr,PBVvr,SpcNew,FSpcOK,NdeXX,
     &               MNde,XNode,YNode,
     &               MVvr,NFrmNde,NghVvr,XVvr,YVvr,
     &               MBVvr,NBFrmNde,BkgStr,FBkgStr,
     &               BndStr,DomStr,StrPw,StrPwInv,
     &               DTol2,DMax,
     &               LsFlgVvr,FlsFlgVv,MaxLsFlg)
C
      IF (FSpcOK) THEN
C       Introduce NNew.
        XNode(NNdeX) = XNode(NNew)
        YNode(NNdeX) = YNode(NNew)
      ELSE
C       Reintroduce NNdeX.
      END IF
C
C     Introduce the chosen node at NNdeX.
C     Find the Voronoi vertices to be deleted.
      PFVvr = IWALK(XNode(NNdeX),YNode(NNdeX),PFVvr,XNode,YNode,
     &              NFrmNde,NghVvr,MVvr,FOutCnv)
C
C     Find the constrained cavity.
      CALL FLGVVRCN (NNdeX,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,
     &               NghVvr,RadVvr,FlsOutVv,LsFlgVvr,FlsFlgVv,
     &               MFlgVvr,FOut)
C
C     Connect the new node with the valid structure.
      CALL CNCTNDE (NNdeX,MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &              LsFlgVvr,FlsFlgVv,MNwVvr)
C
C     Reordering the data structure, deleting obsolete elements.
      CALL PSTPRC (MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &             LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
      RETURN
      END
