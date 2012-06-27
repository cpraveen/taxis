      SUBROUTINE PSTPRC (MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                   LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
C     Last update:
C 7Aug92,16:44;accept also NghVvr=0 as boundary indicator.
C     3Jun91,. (pass grid as arguments.)
C     4.10.90, 17:43. (running in Brussel.)
C
C     Overwrite flagged entries in the triangulation lists with
C     the last entries. Welcome to DOMINO software.
C     
C     Input:
C     LsFlgVvr: List of flagged triangles to be overwritten.
C     MFlgVvr:  Amount of flagged triangles.
C     FlsFlgVv: Flag array for the flagged triangles.
C     MNwVvr:   Number of new Voronoi vertices to include in
C               the structure.
C     /NODES/:  Information about the nodes, here especially
C               the dimension MAXNODE of the arrays.
C     /VVRTC/:  Connectivity with isolated flagged elements. See
C               main program DELAUNDO for details.
C
C     Output:
C     /VVRTC/:  Purged connectivity. See main program DELAUNDO
C               for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
C
      INCLUDE 'nodes.cbl'
C
      DIMENSION XVvr(1),YVvr(1),NFrmNde(3,1),NghVvr(3,1),RadVvr(1),
     &          LsFlgVvr(1),FlsFlgVv(1)
C
C     Take a flagged Voronoi vertex.
      DO IDlVvr = 1,MFlgVvr
C
C       Number of the entry to overwrite.
        NDl = LsFlgVvr(IDlVvr)
C       Number of the entry to copy.
        NNw = MVvr+MNwVvr
C
        Found = .FALSE.
        DO WHILE (NNw.GE.Ndl.AND..NOT.Found)
C
          IF (.NOT.FlsFlgVv(NNw)) THEN
C           This Voronoi vertex is valid.
            Found = .TRUE.
C
C           Copy data:
C           Loop over forming nodes and neighbouring Voronoi vertices.
            DO K = 1,3
              NFrmNde(K,NDl) = NFrmNde(K,NNw)
              NghVvr(K,NDl) = NghVvr(K,NNw)
            END DO          
            XVvr(NDl) = XVvr(NNw)
            YVvr(NDl) = YVvR(NNw)
            RadVvr(NDl) = RadVvr(NNw)
            FlsFlgVv(NDl) = .FALSE.
C
C           Change the pointers at the neighbouring Voronoi 
C           vertices to point to the new storage location:
C           Take a neighbouring Voronoi vertex to the copied one.
            DO KNghVvr = 1,3
C
C             Neighbouring Voronoi vertex to the copied one.
              NghNwVvr = NghVvr(KNghVvr,NDl)
C
              IF (NghNwVvr.GT.0) THEN
C               If this neighbour is a physical one, then
C               check its pointers to neighbouring Voronoi vertices.
C               Take the first neighbouring Voronoi vertex.
                KPntr = 1
                Found = .FALSE.
                DO WHILE (.NOT.Found)
                  IF (NghVvr(KPntr,NghNwVvr).EQ.NNw) THEN
C                   This is the entry to be updated.
                    NghVvr(KPntr,NghNwVvr) = NDl
                    Found = .TRUE.
                  ELSE
C                   Take next neighbouring Voronoi vertex.
                    KPntr = KPntr+1
                  END IF
                END DO
              END IF
C
            END DO
C
          END IF
C
C         Reduce counter of new Voronoi vertices.
          MNwVvr = MNwVvr-1
          NNw = NNw-1
C
        END DO
C
      END DO
C
C     Update counter for Voronoi vertices.
      MVvr = MVvr+MNwVvr
C
      RETURN
      END
