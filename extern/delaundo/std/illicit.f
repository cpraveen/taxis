      SUBROUTINE ILLICIT (NB,NdxLBnNd,IBndType,MBnd,FAllDisc,LsNotCon,
     &                    NdxNotCon,NmNghLNd,NvNmBn,NrBnd,
     &                    FIllCon23,FIllCon31,FIllCon12)
C
C Last update:
C
C Check whether a backgound triangle connects illicitly.
C
C Input:
C NB:        Vector of three forming nodes of the triangle.
C NdxLBnNd:  Vector of last nodes on each segment.
C MBnd:      Number of boundaries.
C FAllDisc:  IF .T., all non-consecutive faces betwen frontal boundaries
C            will be illicit.
C IBndType:  Vector of boundary types.
C MNotCon:   List of boundaries that are not permissible for connections.
C NdxNotCon: Index for MNotCon.
C NmNghLNd:  Name of the segment connected to each segment at the end.
C NvNmBn:    Relates names to numbers for each boundary.
C
C Output:
C NrBnd:     Number of the boundary each node lies on.
C FIllCon:   .T. if the face is illicitly connected.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION NB(3),NdxLBnNd(0:*),FIllCon(3),IBndType(*),NrBnd(3),
     &          LsNotCon(*),NdxNotCon(0:*),FFront(3),NmNghLNd(*),
     &          NvNmBn(-1:*)
C
C     Find the boundary names of all forming nodes.
      DO K=1,3
        NrBnd(K) = 1
        DO WHILE (NdxLBnNd(NrBnd(K)).LT.NB(K).AND.NrBnd(K).LE.MBnd)
          NrBnd(K) = NrBnd(K)+1
        END DO
      END DO
C
      FIllCon(1) = .FALSE.
      FIllCon(2) = .FALSE.
      FIllCon(3) = .FALSE.
C
      IF (FAllDisc) THEN
C       Disconnect all non-consecutive connections on any frontal
C       boundary.
        DO K=1,3
          FFront(K) = .FALSE.
          IF (IBndType(NrBnd(K)).EQ.1.OR.
     &        IBndType(NrBnd(K)).EQ.4) THEN
            FFront(K) = .TRUE.
          ELSE
            FFront(K) = .FALSE.
          END IF
        END DO
C
        DO K=1,3
          IF (FFront(K).AND.FFront(JCYCL(K+1))) THEN
C           Find the consecutive node to NB(K).
            IF(NB(K).EQ.NdxLBnNd(NrBnd(K))) THEN
              NNdeNxt = NdxLBnNd(NvNmBn(NmNghLNd(NrBnd(K)))-1)+1
            ELSE
              NNdeNxt = NB(K)+1
            END IF
            IF (NB(JCYCL(K+1)).NE.NNdeNxt) FIllCon(JCYCL(K+2)) = .TRUE.
          END IF
        END DO
C
      ELSE
C       Find out whether this connectivity is illicit.
        DO K=1,3
          DO INotCon = NdxNotCon(NrBnd(K)-1)+1,NdxNotCon(NrBnd(K))
            NNotCon = LsNotCon(INotCon)
            IF (NrBnd(JCYCL(K+1)).EQ.NNotCon) THEN
C             Nodes NB1 and NB2 must not be connected.
              FIllCon(JCYCL(K+2)) = .TRUE.
            END IF
          END DO
        END DO
C
      END IF
C
      FIllCon23 = FILLCon(1)
      FIllCon31 = FILLCon(2)
      FIllCon12 = FILLCon(3)
      RETURN
      END
