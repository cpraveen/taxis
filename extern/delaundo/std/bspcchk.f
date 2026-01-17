      SUBROUTINE BSPCCHK (XNew,YNew,BSpcNew,XNode,YNode,BSpcVec,
     &                    BTol,MNewNde,PCellIni,NLnkNde,FSpcOK)
C
C Last update:
C 24Nov92,18:10; fix bug with MNde.
C     21Feb92, 3:06. (Simplify distance test.)
C     2Feb92, 0:35. (Eliminate use of nodes.cbl, single precision.)
C     28Jan92, 15:55. (use new RBTol rather than RDTol.)
C     27Jan92, 16:15. (remove updating of SpcNew.)
C
C Check whether the new nodes are sufficiently spaced amongst
C each other. Welcome to DOMINO software.
C
C Input:
C XNew,YNew: Coordinates of the new node to check.
C BSpcNew:   Approximation for the spacing at the new node.
C X-,YNode:  Coordinate vectors of the nodes to check with. 
C BSpcVec:   Vector of approximate spacings at the other new nodes.
C MNewNde:   Number of new nodes past MNde.
C PCellIni:  Startpoint for the walk towards the parent background cell.
C
C Output:
C PCellIni:  Parent background cell.
C NLnkNde:   Number of the first node that is too close.
C FSpcOK:    .TRUE. if the new node is properly spaced.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DOUBLE PRECISION XNode(*),YNode(*),XNew,YNew
      DIMENSION BSpcVec(*)
C
C     Loop backwards to link preferably nodes constructed
C     on the same triangle.
      DO KNde = MNewNde,1,-1
C       Calculate the distance. Note that the X/YNode are passed as
C       XNode(MNde+1).
        DX = XNew-XNode(KNde)
        DY = YNew-YNode(KNde)
        Dist = DX**2+DY**2
        IF (Dist.LT.(BTol*.5*(BSpcNew+BSpcVec(KNde)))**2) THEN
C         The new node is too close to this old node.
          FSpcOK = .FALSE.
          NLnkNde = KNde
          RETURN
        END IF
      END DO
C
      FSpcOK = .TRUE.
      RETURN
      END 
