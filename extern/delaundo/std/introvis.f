      SUBROUTINE INTROVIS (NNew,NNde,NNdePrv,NNdeNxt,NNewPrv,PFVvr,
     &                     FlsOutVv,LsFlgVvr,FlsFlgVv)
C
C Last update:
C 23Apr94,18:45;new PSTPRCS.
C 22Jun92,20:50;intro NNdePrv/Nxt, NNewPrv.
C 3Jun92,19:19;established.
C
C Take a node and introduce it into a constrained triangulation given
C the cell that contains the new node. Welcome to DOMINO software.
C
C Input:
C NNew:     Number of the node to introduce.
C NNde:     Number of the node just below on the previous level.
C NNdePrv:  Node preceding NNde on the line.
C NNdeNxt:  Node following NNde on the line.
C NNdeNxt:  Node preceding NNew on the line.
C PFVvr:    Number of the element that contains NNew.
C FlsOutVv: Vector of flags for protected triangles.
C LsFlgVvr: List of flagged cells (allocated in DELAUNDO).
C FlsFlgVv: Vector of flags for triangles to be retriangulated (allo-
C           cated in DELAUNDO).
C /NODES/:  Nodes and their connectivity in form of frontal strings.
C /VVRTC/:  Connectivity of the nodes.
C
C Output:
C /VVRTC/:  Updated connectivity of the nodes.
C 
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      INCLUDE 'vvrtc.cbl'
      DIMENSION LsFlgVvr(*),FlsFlgVv(*),FlsOutVv(*)
C
C     Find the constrained cavity.
      CALL FLGVVRCN (NNew,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,NghVvr,
     &               RadVvr,FlsOutVv,LsFlgVvr,FlsFlgVv,MFlgVvr,FOut)
C
C     Connect
      CALL CNCTNDE (NNew,MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &              LsFlgVvr,FlsFlgVv,MNwVvr)
C
C     Protect the structured triangles by setting FlsOutVv.
      DO NVvr = MVvr+1,MVvr+MNwVvr
        Nde2 = NFrmNde(2,NVvr)
        Nde3 = NFrmNde(3,NVvr)
        IF (Nde2.EQ.NNde.AND.Nde3.EQ.NNdeNxt) THEN
          FlsOutVv(NVvr) = .TRUE.
        ELSE IF (Nde2.EQ.NNdePrv.AND.Nde3.EQ.NNde) THEN
           FlsOutVv(NVvr) = .TRUE.
        ELSE IF (Nde2.EQ.NNewPrv.AND.Nde3.EQ.NNde) THEN
           FlsOutVv(NVvr) = .TRUE.
        ELSE
           FlsOutVv(NVvr) = .FALSE.
        END IF      
      END DO
C
C     Update NNewPrv to the node just built.
      NNewPrv = NNew
C
C     Clean up
      CALL PSTPRCS (LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr,FlsOutVv,
     &              MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &              XNode,YNode)
C
      RETURN
      END
