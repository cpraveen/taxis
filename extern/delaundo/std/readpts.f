      SUBROUTINE READPTS (NtRead,NdxNotCon,LsNotCon)
C
C     Last update:
C     17Jul91, 10:46. (Allow clockwise boundaries with negative
C                      boundary types through inverse reading.)
C     15Jul91, 15:34. (Read anti-connectivity information.)
C     6Jul91, 15:32. (Introducing IBndType.)
C     5Jul91, 19:45. (Eliminating NghNde.)
C     4.10.90, 18:06
C
C     READPTS reads the input data for DELAUNDO from an unformatted
C     dpl.pts file. Welcome to DOMINO software.
C
C     Input:
C
C     NtRead:   Number of the logical unit the input file is.
C               assigned to.
C     MBnd:     Number of boundaries.
C     NmeBnd:   Name of current boundary. Names may be arbitrarily
C               chosen between 1 and 100 and must not be ordered.
C     MBndNde:  Number of boundary nodes on the boundary, included
C               first and last node.
C     NmNghFnd: Name of the boundary having as last node 
C               the first node of the current boundary.
C     NmNghLnd: Name of the boundary having as first node
C               the last node of the current boundary.
C     IBndType: Type of the boundary given:
C               1:  regular, enforced, in foreground triangulation.
C               2:  enforced, but non-frontal, in foreground.
C               3:  non-enforced by CHKBND, non-frontal in foreground.
C               9:  in background.
C               >0: counterclockwise, domain to the left.
C               <0: clockwise, domain to the  right.
C     NdxNotCon:Number of the boundaries that may not be connected to
C               this boundary in the background mesh.
C     LsNotCon: Names of the boundaries that may not be connected to
C               this boundary in the background mesh. Note that on 
C               output LsNotCon contains the numbers of these
C               boundaries.
C     XNode:    X-coordinate of the boundary node.
C     YNode:    Y-coordinate of the boundary node.
C               Boundary nodes are contiguous in such a way that
C               proceeding to the following boundary node the
C               interior field is on the right hand side.
C     MNtNde:   Number of interior nodes.
C     XNode:    X-coordinate of the interior node.
C     YNode:    Y-coordinate of the interior node.
C     /NODES/:  Maximum amount of boundaries, MaxBound.
C
C     Output:
C     /NODES/   Coordinates, type and amount of boundaries and
C               nodes, pointers to a node on each row of boundary
C               elements, Index of last points of the boundary 
C               segments. See main program DELAUNDO for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      DIMENSION FlChBnd(20),NdxNotCon(0:*),LsNotCon(*)
C
C     Reading input data from the .pts file.
C     Read number of boundary segments.
      READ(NtRead) MBnd
C
C     Read parameters for the first boundary segments.
      NdxNotConZ = NdxNotCon(NBnd-1)+1
      READ(NtRead) NmeBnd(1),MBndNde,NmNghFNd(1),NmNghLNd(1),
     &             IBndType(1),MNotCon,
     &             (LsNotCon(I),I=NdxNotConZ,NdxNotConZ+MNotCon)
      NdxNotCon(NBnd) = NdxNotConZ+MNotCon
C
C     Create an index of the last node of the first segment, NdxLBnNd,
C     keeping in mind, that the first 4 nodes are the corners of
C     the convex hull and the first node of each segment is
C     considered to belong to the previous boundary segment.
      NdxLBnNd(0) = 4
      NdxLBnNd(1) = MBndNde+3
      IF (IBndType(1).GE.0) THEN
C       This is a boundary given in counterclockwise sense with the
C       domain to the left.
        READ(NtRead) XFrst,YFrst,(XNode(NNde),YNode(NNde),
     &                            NNde=5,NdxLBnNd(1))
      ELSE
C       This is a boundary with the domain to the right.
        READ(NtRead) (XNode(NNde),YNode(NNde),NNde=NdxLBnNd(1),5,-1),
     &               XFrst,YFrst
      END IF
C
C     Create inverse of the list of names of the boundaries NvNmBn.
      NvNmBn(NmeBnd(1)) = 1
C
C     Reset check-flag.
      FlChBnd(1) = .FALSE.
C
      DO NBnd=2,MBnd
C
C       Read parameters for the following boundary segments.
        NdxNotConZ = NdxNotCon(NBnd-1)+1
        READ(NtRead) NmeBnd(NBnd),MBndNde,NmNghFNd(NBnd),
     &               NmNghLNd(NBnd),IBndType(NBnd),
     &               MNotCon,
     &               (LsNotCon(I),I=NdxNotConZ,NdxNotConZ+MNotCon)
        NdxNotCon(NBnd) = NdxNotConZ+MNotCon
C       Create an index of the last node of this segment, NdxLBnNd.
        NdxLBnNd(NBnd) = NdxLBnNd(NBnd-1)+MBndNde-1
C       Read coordinates of the boundary segment.
        IF (IBndType(1).GE.0) THEN
C         This is a boundary given in counterclockwise sense with the
C         domain to the left.
          READ(NtRead) XFrst,YFrst,
     &                 (XNode(NNde),YNode(NNde),
     &                  NNde=NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd))
        ELSE
C         This is a boundary with the domain to the right.
          READ(NtRead) (XNode(NNde),YNode(NNde),
     &                  NNde=NdxLBnNd(NBnd),NdxLBnNd(NBnd-1)+1,-1),
     &                 XFrst,YFrst
        END IF
C
C       Create inverse of the list of names of the boundaries NvNmBn.
        NvNmBn(NmeBnd(NBnd)) = NBnd
C
C       Reset check-flag.
        FlChBnd(NBnd) = .FALSE.
C
      END DO
C
C     Read interior nodes.
      READ(NtRead) MNtNde
C     Number of Nodes.
      MNde = NdxLBnNd(MBnd)+MNtNde
      READ(NtRead) (XNode(NNtNde),YNode(NNtNde),
     &              NNtNde=NdxLBnNd(MBnd)+1,MNde)
C
C     Specify one boundary node on each element and count the
C     number of elements, correct information of adjacent elements.
      MLmt = 0
C
C     Pick a boundary segment.
      DO NBnd = 1,MBnd
C
        IF (.NOT.FlChBnd(NBnd)) THEN
C         Segment hasn't been used so far.
          NCurrBnd = NBnd
          FlChBnd(NBnd) = .TRUE.
          NTrgtBnd = 0
C
C         Walk through its adjacent segments.
          Found = .FALSE.
          DO WHILE (.NOT.Found)
C
            IF (NCurrBnd.EQ.NTrgtBnd) THEN
C             This is the end of the row of boundary segments.
C             Increase sum of elements.
              MLmt = MLmt+1
C             Set pointer to second point of first segment of the row.
              LmtNde(MLmt) = NdxLBnNd(NBnd)-MBndNde+2
              Found = .TRUE.
C
            ELSE
C             Take the following segment.
              NCurrBnd = NvNmBn(NmNghLNd(NCurrBnd))
              FlChBnd(NCurrBnd) = .TRUE.
C
C             If not happened yet initialize the target segment.
              IF (NTrgtBnd.EQ.0) NTrgtBnd = NBnd
C
            END IF
C
          END DO
C
        END IF
      END DO
C
C     Convert the anti-connectivity information from boundary
C     names to boundary numbers.
      DO KBnd = 1,NdxNotCon(MBnd)
        LsNotCon(KBnd) = NvNmBn(LsNotCon(KBnd))
      END DO
C
C     Erase clockwise boundary types.
      DO NBnd = 1,MBnd
        IBndType(NBnd) = ABS(IBndType(NBnd))
      END DO
C
      RETURN
      END
