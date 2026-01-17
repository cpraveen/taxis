      SUBROUTINE NDEX (NNde,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,
     &                 NghVvr,RadVvr,FlsOutVv,XNode,YNode,MNde,
     &                 LsFlgVvr,FlsFlgVv,LsNde,LsNgh)
C
C Last update:
C 23Apr94,18:45;new PSTPRCS.
C 14Mar94,19:20; make Dist DOUBLE.
C
C Extract a node from the triangulation and close the mesh.
C Welcome to DOMINO software.
C
C Input:
C NNde:     Number of the new point to be extracted.
C PFVvr:    Number of the element in the triangulation that contains
C           NNde.
C MVvr:     Number of Elements in the grid.
C X/YVvr:   Vectors of the coordinates of the Voronoi vertices.
C NFrmNde:  Array of the nodal connectivity.
C NghVvr:   Array of the three neighbors of each cell.
C RadVvr:   Vector of the squared radii of the circumcircles.
C
C Output:
C MVvr:     Number of Elements in the grid.
C X/YVvr:   Vectors of the coordinates of the Voronoi vertices.
C NFrmNde:  Array of the nodal connectivity.
C NghVvr:   Array of the three neighbors of each cell.
C RadVvr:   Vector of the squared radii of the circumcircles.
C Ls,Fls:   Work space.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DOUBLE PRECISION Dist
      DIMENSION XVvr(*),YVvr(*),NFrmNde(3,*),NghVvr(3,*),RadVvr(*),
     &          FlsOutVv(*),LsFlgVvr(*),FlsFlgVv(*),XNode(*),YNode(*),
     &          LsNde(*),LsNgh(*)
C
C     Erase the vector of flags.
      DO NVvr = 5,MVvr+100
        FlsFlgVv(NVvr) = .FALSE.
      END DO
C
C     Find a foreground cell that is formed with NNde.
      NVvr = IWALK(XNode(NNde),YNode(NNde),PFVvr,XNode,YNode,
     &             NFrmNde,NghVvr,MVvr,FOutCnv)
C
C     Walk around NNde and find all cells and the outer perimeter of
C     the cavity.
      NNdeTrgt = 0
      MFlgVvr = 0
      NNdeNxt = -1
      DO WHILE (NNdeNxt.NE.NNdeTrgt)
C       Put NVvr into the list, Find back NNde in NVvr, list the
C       nodes on the perimeter.
        MFlgVvr = MFlgVvr+1
        LsFlgVvr(MFlgVvr) = NVvr
        FlsFlgVv(NVvr) = .TRUE.
        DO KK = 1,3
          IF (NFrmNde(KK,NVvr).EQ.NNde) KNde = KK
        END DO
        NNdeCurr = NFrmNde(JCYCL(KNde+1),NVvr)
        NNdeNxt = NFrmNde(JCYCL(KNde+2),NVvr)
        LsNde(MFlgVvr) = NNdeCurr
        LsNgh(MFlgVvr) = NghVvr(KNde,NVvr)
        NVvr = NghVvr(JCYCL(KNde+1),NVvr)
        IF (NNdeTrgt.EQ.0) NNdeTrgt = NNdeCurr
      END DO
      MPNde = MFlgVvr

C     All of the following should be replaced by TRICAV, the next
c     time we debug around ndex. 1Apr97, and this is not an April
c     fool's joke. Jens.
      
C     Extend the cyclic succession.
      DO NPNde = 1,MPNde
        LsNde(MPNde+NPNde) = LsNde(NPNde)
        LsNgh(MPNde+NPNde) = LsNgh(NPNde)
      END DO
C
C     Shave off triangles until the cavity has vanished.
      MNwVvr = 0
      DO WHILE (MPNde.GT.2)
        Found = .FALSE.
        KNde = 1
C       Walk around the perimeter of the cavity to find a valid
C       Delaunay triangle thet connects three consecutive nodes.
C       Such a triangle must exist. Then eliminate the node at the
C       center from the list, form the new cell and reorder nodes
C       and cells around the perimeter.
        Found = .TRUE.
        DO WHILE (Found)
          Found = .FALSE.
C         Take the next three nodes to possibly form a cell.
          NNwVvr = MVvr+MNwVvr+1
          NFrmNde(1,NNwVvr) = LsNde(KNde)
          NFrmNde(2,NNwVvr) = LsNde(KNde+1)
          NFrmNde(3,NNwVvr) = LsNde(KNde+2)
          
C         Calculate its circumcenter and radius.
          CALL CALCVVR (NFrmNde(1,NNwVvr),XVvr(NNwVvr),YVvr(NNwVvr),
     &                  RadVvr(NNwVvr),FLinDeg)
          IF (FLinDeg) THEN
C           This triangle is linearly degenerate. Try the next cell.
            KNde = KNde+1
            Found = .TRUE.
C
          ELSE
C           Check whether this triangle is counterclockwise. I.e.,
C           make sure it is not formed at a reflex edge of the
C           cavity on the wrong side. Use the cross-product.
            Nde1 = NFrmNde(1,NNwVvr)
            Nde2 = NFrmNde(2,NNwVvr)
            Nde3 = NFrmNde(3,NNwVvr)
            DX1 = XNode(Nde3)-XNode(Nde2)
            DY1 = YNode(Nde3)-YNode(Nde2)
            DX3 = XNode(Nde1)-XNode(Nde2)
            DY3 = YNode(Nde1)-YNode(Nde2)
            CrPrd = DX1*DY3-DY1*DX3
            IF (CrPrd.LE.0.) THEN
              Found = .TRUE.
            ELSE
C             Prepare for the loop over thr rmaining nodes in the
C             cavity.
              KKNde = KNde+2
              RadVvMin = RadVvr(NNwVvr)*RadTolMi
            END IF
C
C           Check whether this circumcircle is free of nodes from
            DO WHILE (.NOT.Found.AND.KKNde.LT.KNde+MPNde-1)
              KKNde = KKNde+1
              NxtNde = LsNde(KKNde)
              Dist = (XNode(NxtNde)-XVvr(NNwVvr))**2+
     &               (YNode(NxtNde)-YVvr(NNwVvr))**2
              IF (Dist.LT.RadVvMin) Found = .TRUE.
            END DO
C
            IF (Found) THEN
C             Try the next cell.
              KNde = KNde+1
            ELSE
C             Form this cell, update the neighboring connectivity.
              MNwVvr = MNwVvr+1

              NghVvr(3,NNwVvr) = LsNgh(KNde)
              NNgh = LsNgh(KNde)
              NdeShare = NFrmNde(1,NNwVvr)
              DO KNgh = 1,3
                IF (NFrmNde(KNgh,NNgh).EQ.NdeShare)
     &            NghVvr(JCYCL(KNgh+1),NNgh) = NNwVvr
              END DO

              NghVvr(1,NNwVvr) = LsNgh(KNde+1)
              NNgh = LsNgh(KNde+1)
              NdeShare = NFrmNde(2,NNwVvr)
              DO KNgh = 1,3
                IF (NFrmNde(KNgh,NNgh).EQ.NdeShare)
     &            NghVvr(JCYCL(KNgh+1),NNgh) = NNwVvr
              END DO

C             Reorder the perimeter.
              MPNde = MPNde-1
              LsNde(1) = LsNde(KNde+2)
              DO KKNde = 2,MPNde
                LsNde(KKNde) = LsNde(KNde+KKNde+1)
                LsNgh(KKNde-1) = LsNgh(KNde+KKNde)
              END DO
              LsNgh(MPNde) = NNwVvr
              DO KKNde = 1,MPNde
                LsNde(KKNde+MPNde) = LsNde(KKNde)
                LsNgh(KKNde+MPNde) = LsNgh(KKNde)
              END DO
            END IF
C
          END IF
        END DO
      END DO
C
C     Fix the neighbors across the last edge.
      NghVvr(2,NNwVvr) = LsNgh(1)
      NghVvr(2,LsNgh(1)) = NNwVvr
C
C     Update PFVvr. Use as a cell in the vicinity the one copied over
C     LsFlgVvr(1) or LsFlgVvr(2). Only one of them can possibly be at
C     the end of the list and thus possibly eliminated. Pick the one
C     in the field.
      PFVvr = MIN(LsFlgVvr(1),LsFlgVvr(2))
C
C     Reorder the data structure.
      CALL PSTPRCS (LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr,FlsOutVv,
     &              MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &              XNode,YNode)
C
      RETURN
      END

