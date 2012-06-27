      SUBROUTINE CNCTNDE (NNde,
     &                    MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                    LsFlgVvr,FlsFlgVv,ICntFce)
C
C Last update:
C 18Feb94,16:00; intro FAbort.
C 25Sep93,22:10; new CALCVVR.
C 9Feb93,23:55; Welcome to 4.0.
C
C Connect the new point to its contiguous points. Welcome to DOMINO
C software.
C
C     Input:
C     NNde:     Number of the new node to be introduced.
C     LsFlgVvr: List of flagged Voronoi vertices, thus
C               the domain to be retriangulated.
C     FlsFlgVv: Array of flags indicating that the corresponding
C               Voronoi vertex has been flagged when set to true.
C     /VVRTC/:  Old connectivity. See main program DELAUNDO for 
C               details.
C     /NODES/:  Coordinates of the nodes. See main program
C               DELAUNDO for details.
C
C     Output:
C     ICntFce:  Counter of the faces created along the outer edge
C               of the cut out domain, thus the number of new
C               Voronoi vertices.
C     /VVRTC/:  New structure. See main program DELAUNDO for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      DIMENSION XVvr(1),YVvr(1),NFrmNde(3,1),NghVvr(3,1),RadVvr(1),
     &          LsFlgVvr(2*MaxNode),FlsFlgVv(2*MaxNode),
     &          PNghVvr(2*MaxBndNde),PFlgVvr(2*MaxBndNde),
     &          PNwVvr(2*MaxBndNde)
C
      IF (ICntFce.EQ.-99) THEN
C       Encoded: do not abort ungracefully on linear degeneracy, but
C       return with a negative ICntFce.
        FAbort = .FALSE.
      ELSE
        FAbort = .TRUE.
      END IF
C
C     Initialize the first edge.
      ICntFce = 0
      NFlgVvr = LsFlgVvr(1)
      KNdePvt = 1
      KNdeNxt = 2
      NNdeTrgt = 0
      NNghVvr = NghVvr(3,NFlgVvr)
C
C     Loop as long set of edges is not closed.
      DO WHILE (NNdeTrgt.NE.NFrmNde(KNdePvt,NFlgVvr))
C
        KNgh = JVVR(KNdePvt,KNdeNxt)
        IF (FlsFlgVv(NghVvr(KNgh,NFlgVvr))) THEN
C         This neighbour is also flagged.
C
C         Find back the common edge in this neighbour.
          Found = .FALSE.
          KNghVvr = 1
          DO WHILE (.NOT.Found)
C
            IF (NghVvr(KNghVvr,NNghVvr).EQ.NFlgVvr) THEN
C             This is the common face.
C
              IF (NFrmNde(JCYCL(KNghVvr+1),NNghVvr).EQ.
     &            NFrmNde(KNdePvt,NFlgVvr)) THEN
C               This lower node of the face is the new pivot.
                KNdePvt = JCYCL(KNghVvr+1)
              ELSE
C               The higher node is the new pivot.
                KNdePvt = JCYCL(KNghVvr+2)
              END IF
C
C             Rest of the update.
C             The new "next" node is opposite to the common face.
              KNdeNxt = KNghVvr
C             The new Voronoi vertex to look at is the former
C             neighbouring Voronoi vertex.
              NFlgVvr = NNghVvr
C             The new neighbouring Voronoi vertex is the one
C             on the edge with the pivot and the "next" node.
              NNghVvr = NghVvr(JVVR(KNdePvt,KNdeNxt),NFlgVvr)
              Found = .TRUE.
C
            END IF
C
            KNghVvr = KNghVvr+1
          END DO
C
        ELSE
C         This neighbouring Voronoi vertex is not flagged. Thus this is 
C         an outside face. Let's form a triangle.
C
C         Increase counter.
          ICntFce = ICntFce+1
          I = MVvr+ICntFce
C
C         List forming nodes.
          NFrmNde(1,I) = NNde
          NFrmNde(2,I) = NFrmNde(KNdePvt,NFlgVvr)
          NFrmNde(3,I) = NFrmNde(KNdeNxt,NFlgVvr)
C
C         List neighbouring Voronoi vertices.
          NghVvr(1,I) = NNghVvr
          NghVvr(2,I) = I+1
          NghVvr(3,I) = I-1
C
C         Calculate the location of the new Voronoi vertex.
          CALL CALCVVR(NFrmNde(1,I),XVvr(I),YVvr(I),RadVvr(I),FWteNde)
C
          IF (FWteNde.AND.FAbort) THEN
C           The new node lies on the same line as the other
C           two forming nodes.
            WRITE (*,'(A/A,3I7/A,3F13.5/A,3F13.5)')
     &      ' FATAL: linear degeneracy in cnctnde.',
     &      '        trying to connect',(NFrmNde(ii,i),ii=1,3),
     &      '        x:',(xnode(nfrmnde(ii,i)),ii=1,3),
     &      '        y:',(ynode(nfrmnde(ii,i)),ii=1,3)
            STOP
          ELSE IF (FWteNde) THEN
            ICntFce = -1
            RETURN
          END IF
C
C         Virtual update of the connectivity in the old neighbour.
          PNghVvr(ICntFce) = NNghVvr
          PFlgVvr(ICntFce) = NFlgVvr
          PNwVvr(ICntFce) = I
C
C         Set target to first pivot - in case not done yet.
          IF (NNdeTrgt.EQ.0) NNdeTrgt = NFrmNde(KNdePvt,NFlgVvr)
C
C         Update for the next face in the same triangle.
C         Keep in mind the old pivot.
          KOldPvt = KNdePvt
C         The new pivot is the old "next" node.
          KNdePvt = KNdeNxt
C         The new "next" node is the third, so far untouched node of
C         the triangle in NFlgVvr.
          KNdeNxt = JVVR(KOldPvt,KNdeNxt)
C         The new neighbour to look at is opposite to the old pivot.
          NNghVvr = NghVvr(KOldPvt,NFlgVvr)
C
        END IF
      END DO
C   
C     Correction of the pointers to neighbours 
C     for the first and the last new Voronoi vertex.
      NghVvr(3,MVvr+1) = MVvr+ICntFce
      NghVvr(2,MVvr+ICntFce) = MVvr+1
C
C     Update of the connectivity in the old neighbour, only
C     executed if no degeneracy occurred.
      DO I = 1,ICntFce
        DO KNgh = 1,3
          IF (NghVvr(KNgh,PNghVvr(I)).EQ.PFlgVvr(I)) THEN
C           This is the entry to be updated.
C           Change neighbouring pointer from flagged Vvr to new Vvr:
            NghVvr(KNgh,PNghVvr(I)) = PNwVvr(I)
            Found = .TRUE.
          END IF
        END DO
        IF (.NOT.Found) THEN
          WRITE (*,'(2(A,I7),A)') ' Neighbor',PFlgVvr(I),
     &                ' not listed in',PNghVvr(I),' during CNCTNDE.'
          STOP
        END IF
      END DO
C 
      RETURN
      END
