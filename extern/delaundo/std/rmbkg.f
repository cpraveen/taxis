      SUBROUTINE RMBKG (MAllVvr,NFrmNde,MNde,XNode,YNode,
     &                  MBnd,NdxLBnNd,IBndType)
C
C Last update:
C 25Sep94;cut out from tridpl.
C
C Eliminate the additional background vertices. Repack the list
C of vertices, adjust NFrmNde in all levels. Welcome to DOMINO software.
C
C Input:
C MAllVvr:   Number of all Voronoi Vertices in all structures.
C NFrmNde:   Array of the three forming nodes of each Voronoi vertex.
C MNde:      Number of nodes introduced in the structure.
C XNode:     Vector of x-coordinates of the nodes.
C YNode:     Vector of y-coordinates of the nodes.
C MBnd:      Number of boundaries.
C NdxLBnNd:  Vector of indices of the last node on each
C            boundary segment.
C IBndType:  Type of the boundary. See DELAUNDO for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION NFrmNde(3,*),XNode(*),YNode(*),NdxLBnNd(0:*),IBndType(*)
C
C     Remove boundaries of the background grid.
      NBnd = 0
      DO WHILE (NBnd.LT.MBnd)
        NBnd = NBnd+1
        IF (IBndType(NBnd).EQ.9.OR.IBndType(NBnd).EQ.10) THEN
C         Erase the additional nodes in the background triangulation
C         from the list.
          NLoNde = NdxLBnNd(NBnd-1)+1
          NHiNde = NdxLBnNd(NBnd)
          MDelNde = NHiNde-NLoNde+1
          DO NVvr = 5,MAllVvr
            DO KNde = 1,3
              NNde = NFrmNde(KNde,NVvr)
              IF (NNde.GT.NLoNde) NFrmNde(KNde,NVvr) = NNde-MDelNde
            END DO
          END DO
          DO NNde = NLoNde,MNde-MDelNde
            XNode(NNde) = XNode(NNde+MDelNde)
            YNode(NNde) = YNode(NNde+MDelNde)
          END DO
          DO NNBnd = NBnd+1,MBnd
            NdxLBnNd(NNBnd) = NdxLBnNd(NNBnd)-MDelNde
          END DO
          MNde = MNde-MDelNde
C
C         Erase the background boundary from the list of boundaries.
          DO NNBnd = NBnd,MBnd-1
            NdxLBnNd(NNBnd) = NdxLBnNd(NNBnd+1)
          END DO
          MBnd = MBnd-1
          NBnd = NBnd-1
        END IF
      END DO
C
      RETURN
      END
