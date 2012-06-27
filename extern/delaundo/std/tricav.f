      SUBROUTINE TRICAV (MVvr,MNwVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &     RadTolMi,XNode,YNode,LsNde,LsNgh,MPNde)
C
C Last update:
c     1Apr97; cut out of ndex.f
C
C Triangulate a cavity.
C Welcome to DOMINO software.
C
C Input:
C MVvr:     Number of Elements in the grid.
C MNwVvr:   Number of new Elements in the grid, waiting to be pstprcs'd.
C X/YVvr:   Vectors of the coordinates of the Voronoi vertices.
C NFrmNde:  Array of the nodal connectivity.
C NghVvr:   Array of the three neighbors of each cell.
C RadVvr:   Vector of the squared radii of the circumcircles.
c LsNde:    The nodes around the perimeter.
C LsNgh:    The neighbors around the perimeter.
c MPNde:    The number of nodes/edges/neighbors around the perimeter.
C
C Output:
C MVvr:     Number of Elements in the grid.
C X/YVvr:   Vectors of the coordinates of the Voronoi vertices.
C NFrmNde:  Array of the nodal connectivity.
C NghVvr:   Array of the three neighbors of each cell.
C RadVvr:   Vector of the squared radii of the circumcircles.
C LsNde/LsNgh: The list is cyclically extended and the used elements
C              are removed as the triangulation proceeds.
C 
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      INTEGER MNwVvr, MVvr, NFrmNde(3,*), NghVvr(3,*),
     &     LsNde(*), LsNgh(*), MPNde
      DOUBLE PRECISION RadTolMi, XVvr(*), YVvr(*), RadVvr(*),
     &     XNode(*), YNode(*)

      DOUBLE PRECISION Dist, DX1, DY1, DX3, DY3, CrPrd, RadVvMin
      INTEGER KNde, Nde1, Nde2, Nde3, KNgh, KKNde, NNgh, NNwVvr,
     &     NxtNde, NdeShare, JCYCL, NPNde
      LOGICAL*1 Found, FLinDeg

      IF ( MPNde .EQ. 3 ) THEN
c       There are three nodes, i.e. one triangle.
        MNwVvr = MNwVvr+1
        NNwVvr = MVvr+MNwVvr
c       Make the cell and fix the pointers to the formed cell with its neighbors.
        DO KNde = 1,3
          NFrmNde(KNde,NNwVvr) = LsNde(KNde)
          NghVvr(KNde,NNwVvr) = LsNgh( JCYCL(KNde+1) )
c         The neighbor ccw after this node.
          NNgh =  LsNgh(KNde)
          DO KNgh = 1,3
            IF ( NFrmNde(KNgh,NNgh) .EQ. LsNde(KNde) )
     &           NghVvr(JCYCL(KNgh+1),NNgh) = NNwVvr
          END DO
        END DO
        
        RETURN
      END IF

C     Extend the cyclic succession.
      DO NPNde = 1,MPNde
        LsNde(MPNde+NPNde) = LsNde(NPNde)
        LsNgh(MPNde+NPNde) = LsNgh(NPNde)
      END DO

C     Shave off triangles until the cavity has vanished.
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
c             Extend the cyclic succession.
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
      RETURN
      END

