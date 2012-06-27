      SUBROUTINE PSTPRCS (LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr,FlsOutVv,
     &                    MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &                    XNode,YNode)
C
C Last update:
C 23Apr94,18:45;drop common blocks.
C 13Jan94,15:10; Fix bug occuring when MFlgVvr > MNwVvr.
C 3Jun92,19:15;Extended from PSTPRC to carry over FlsOutVv.
C
C     Overwrite flagged entries in the triangulation lists with
C     the last entries. Welcome to DOMINO software.
C     
C     Input:
C     LsFlgVvr: List of flagged triangles to be overwritten.
C     MFlgVvr:  Amount of flagged triangles.
C     FlsFlgVv: Flag vector for the flagged triangles.
C     JOutVv:   Status indicator vector for the flagged triangles.
C     MNwVvr:   Number of new Voronoi vertices to include in
C               the structure.
C     /NODES/:  Information about the nodes, here especially
C               the dimension MAXNODE of the arrays.
C     /VVRTC/:  Connectivity with isolated flagged elements. See
C               main program DELAUNDO for details.
C
C     Output:
C     JOutVv: Reordered quality flag vector for the flagged triangles.
C     /VVRTC/:  Purged connectivity. See main program DELAUNDO
C               for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION LsFlgVvr(*),FlsFlgVv(*),FlsOutVv(*),
     &          NFrmNde(3,*),NghVvr(3,*),XVvr(*),YVvr(*),RadVvr(*),
     &          XNode(*),YNode(*)
C
C     Loop over all flagged Voronoi vertices.
      DO IDlVvr = 1,MFlgVvr
C
C       Search backwards from the top of the list of new entries for
C       a non-flagged Vvr.
C       Number of the entry to overwrite.
        NDl = LsFlgVvr(IDlVvr)
C       Latest entry in the list.
        NNw = MVvr+MNwVvr
        Found = .FALSE.
        DO WHILE (NNw.GE.NDl.AND..NOT.Found)
          IF (.NOT.FlsFlgVv(NNw)) THEN
            Found = .TRUE.
C
C           This Voronoi vertex is valid. Copy its data over NDl.
            DO K = 1,3
              NFrmNde(K,NDl) = NFrmNde(K,NNw)
              NghVvr(K,NDl) = NghVvr(K,NNw)
            END DO          
            XVvr(NDl) = XVvr(NNw)
            YVvr(NDl) = YVvR(NNw)
            RadVvr(NDl) = RadVvr(NNw)
            FlsFlgVv(NDl) = .FALSE.
            FlsOutVv(NDl) = FlsOutVv(NNw)
C
C           Change the pointers at the neighboring Voronoi 
C           vertices to point to the new storage location:
            DO KNghVvr = 1,3
C             Number of the neighbor to the copied one.
              NghNwVvr = NghVvr(KNghVvr,NDl)
              IF (NghNwVvr.NE.-10) THEN
C               This neighbour is a physical one. Find back the
C               neighboring entry pointing toward NNw and update.
                Found = .FALSE.
                DO KVvr = 1,3
                  IF (NghVvr(KVvr,NghNwVvr).EQ.NNw) THEN
C                   This is the entry to be updated.
                    NghVvr(KVvr,NghNwVvr) = NDl
                    Found = .TRUE.
                  END IF
                END DO
                IF (.NOT.Found) THEN
                  WRITE (*,99) 
     &             'FATAL: DELAUNDO died in PSTPRCS trying to find',
     &             NNw,' in the neighbors of',NghNwVvr,
     &             ' which are',(NghVvr(KVvr,NghNwVvr),KVvr=1,3),'.',
     &             '       The coordinates of the cell are:',
     &             (NFrmNde(K,NNw),XNode(NFrmNde(K,NNw)),
     &              YNode(NFrmNde(K,NNw)),K=1,3)
 99               FORMAT (/A,I7,A,I7/7X,A,3I7,A/A,3(I7,2E15.7/39X))
                  STOP
                END IF
              END IF
            END DO
C
          END IF
C
C         Reduce counter of new Voronoi vertices because either we
C         have copied NNw over NDl or NNw was invalid anyhow.
          MNwVvr = MNwVvr-1
          NNw = NNw-1
        END DO
      END DO
C
C     Update counter for Voronoi vertices.
      MVvr = MVvr+MNwVvr
      RETURN
      END
