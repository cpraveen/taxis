      SUBROUTINE FLGVVRCN (NNde,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,
     &                     NghVvr,RadVvr,FlsOutVv,
     &                     LsFlgVvr,FlsFlgVv,MFlgVvr,FOutside)
C
C Last update:
C 2Nov92,22:05;intro FOutside.
C 3Jun92,19:25;established.
C
C Find the constrained cavity of the point NNde to be introduced.
C Welcome to DOMINO software.
C
C Input:
C NNde:     Number of the new point to be introduced.
C PFVvr:    Number of the element in the triangulation that contains
C           NNde.
C RadTolMi: Tolerance to apply to the circumcircle to ensure that the
C           NNde lies inside.
C MVvr:     Number of Elements in the grid.
C X/YVvr:   Vectors of the coordinates of the Voronoi vertices.
C NFrmNde:  Array of the nodal connectivity.
C NghVvr:   Array of the three neighbors of each cell.
C RadVvr:   Vector of the squared radii of the circumcircles.
C FlsOutVv: Vector of flags indicating that the corresponding cell 
C           shall not be retriangulated.
C
C Output:
C LsFlgVvr: List of flagged Voronoi vertices.
C FLsFlgVv: Array of flags that show that the corresponding
C           Voronoi vertex has been flagged when set to true.
C MFlgVvr:  Number of flagged Voronoi vertices.
C FOutside: .T. if NNde is not contained in the circumcircle of PFVvr.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      DIMENSION XVvr(*),YVvr(*),NFrmNde(3,*),NghVvr(3,*),RadVvr(*),
     &          FlsOutVv(*),LsFlgVvr(*),FlsFlgVv(*)
C
C     Erase the vector of flags.
      DO NVvr = 1,MVvr+100
        FlsFlgVv(NVvr) = .FALSE.
      END DO
C
C     Distance from new node to center of circumcircle.
      RDst = (XVvr(PFVvr)-XNode(NNde))**2+
     &       (YVvr(PFVvr)-YNode(NNde))**2
      IF (RDst.LE.RadVvr(PFVvr)*RadTolMi.AND..NOT.FlsOutVv(PFVvr)) THEN
C
C       The new node lies within this circle and the triangle can
C       be retriangulated.
C       Place the found Voronoi vertex as first entry in the list
C       of flagged Voronoi vertices LsFlgVvr, set the amount of
C       found flagged Voronoi vertices MFlgVvr to 1.
        LsFlgVvr(1) = PFVvr
        FlsFlgVv(PFVvr) = .TRUE.
        MFlgVvr = 1
        IWrkVvr = 1
C
      ELSE
        FOutside = .TRUE.
        RETURN
      END IF
C
C     Loop over all the entries in the flagged list.
      DO WHILE (IWrkVvr.LE.MFlgVvr)
C
C       Take the next flagged Voronoi vertex.
        NVvr = LsFlgVvr(IWrkVvr)
C
C       Loop over the neighbours of each entry in the flagged list.
        DO KNghVvr = 1,3
C         Number of the neighbouring Voronoi vertex to check.
          NNghVvr = NghVvr(KNghVvr,NVvr)
C
          IF (.NOT.(FlsFlgVv(NNghVvr).OR.FlsOutVv(NNghVvr))) THEN
C           This Voronoi vertex can get flagged.
C           Calculate the distance.
            RDst = (XVvr(NNghVvr)-XNode(NNde))**2+
     &             (YVvr(NNghVvr)-YNode(NNde))**2
            IF (RDst.LT.RadVvr(NNghVvr)*RadTolMi) THEN
C             NNde lies inside. Add it to the list. 
              MFlgVvr = MFlgVvr+1
              LsFlgVvr(MFlgVvr) = NNghVvr
              FlsFlgVv(NNghVvr) = .TRUE.
            END IF
          END IF
C
        END DO
C
C       Increase the amount of checked Voronoi vertices by one.
        IWrkVvr = IWrkVvr+1
      END DO
C
      FOutside = .FALSE.
      RETURN
      END
