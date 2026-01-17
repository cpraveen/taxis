      SUBROUTINE LPLC (IVerbose,NtLog,FLogFile,
     &                 MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,FlsOutVv,
     &                 MNde,XNode,YNode,
     &                 MBnd,NdxLBnNd,NvNmBn,NmNghFNd,NmNghLNd,IBndType,
     &                 Beta,RelFac,MSweep,MVisNode)
C
C Last update:
C 27Aug96; don't smooth viscous nodes.
C 9Jan96; fix typo in format statement 11.
C July94; derived from LPLFILT.
C
C Apply a Laplacian filter to a given triangular grid. Richter's
C method of barycentrage pond'er'e is applied that reduces the
C weight of neighboring nodes that form many edges. The effect
C is controlled by setting a parameter beta [0,1]. Welcome to 
C DOMINO software.
C
C Input:
C IVerbose: Level of verbosity.
C NtLog:    Logical unit of the logfile.
C FLogFile: .T. if a logfile is to be written.
C MVvr:     Number of cells.
C NFrmNde:  Forming nodes of all cells.
C NghVvr:   Neighbors of all cells.
C X/YVvr:   Vector of coordinates of the circumcenter.
C RadVvr:   Vector of circumradii.
C FlsOutVv: Flag indicating that the cell is outside the domain.
C MNde:     Number of nodes.
C X/YNode:  Coordinates of the nodes.
C MBnd:     Number of boundaries.
C NdxLBnNd: Last boundary node of each boundary segment.
C NvNmBn:   Relates boundary names to boundary numbers.
C NmNghF/LNd: Names of the next boundary segment at first/last node. 
C Beta:     Modifier for the weight function. See Richter.
C RelFac:   Relaxation Factor for the update.
C MSweep:   Number of iterations to be performed.
C MVisNode: Number of the viscous nodes.
C 
C Output:
C X/YNode:  Coordinates of the nodes.
C X/YVvr:   Erased.
C RadVvr:   Erased.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DOUBLE PRECISION XVvr,YVvr,RadVvr,XNode,YNode
      DIMENSION NFrmNde(3,*),NghVvr(3,*),XVvr(*),YVvr(*),RadVvr(*),
     &          XNode(*),YNode(*),NdxLBnNd(0:*),NvNmBn(-1:*),
     &          NmNghFNd(*),NmNghLNd(*),FlsOutVv(*),IBndType(*)
      DOUBLE PRECISION TwoPi,DXHi,DXLo,DYHi,DYLo,DHi,
     &                 X1,Y1,X2,Y2,X3,Y3,RDX1,RDY1,RDX2,RDY2,RDX3,RDY3
      TwoPi = 4.*ACOS(0.)
      MBndNde = NdxLBnNd(MBnd)

      IF (FLogFile) WRITE (NtLog,37)
      IF (IVerbose.GE.3) WRITE (*,37)
 37   FORMAT (/3X,' Applying a Laplacian smoother.')

C     Don't smooth fields without interior nodes.
      IF ( MNde-MBndNde .EQ. 0 ) THEN
        IF (IVerbose.GT.3) WRITE (*,12) 
        IF (FLogFile) WRITE (NtLog,12) 
 12     FORMAT (6X,' WARNING: no interior nodes, no smoothing.')
        RETURN
      END IF
      
C     Find the number of twice the neighboring nodes for each node.
C     Store the counter temporarily in RadVvr.
      DO NNde = 5,MNde
        RadVvr(NNde) = 0.
      END DO     
C     Loop over all cells to find the number of neighboring nodes.
      DO NVvr = 5,MVvr
        IF (.NOT.FlsOutVv(NVvr)) THEN
          N1 = NFrmNde(1,NVvr)
          N2 = NFrmNde(2,NVvr)
          N3 = NFrmNde(3,NVvr)
          RadVvr(N1) = RadVvr(N1)+1.
          RadVvr(N2) = RadVvr(N2)+1.
          RadVvr(N3) = RadVvr(N3)+1.
        END IF
      END DO
C
C     Correct the counter with respect to the angular fraction
C     that the segment around the boundary node spans.
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).EQ.1.OR.IBndType(NBnd).EQ.2) THEN
C         This is an enforced boundary with physical nodes.
          NNde = NdxLBnNd(NvNmBn(NmNghFNd(NBnd)))
          NHi = NdxLBnNd(NBnd-1)+1
          DXHi = XNode(NHi)-XNode(NNde)
          DYHi = YNode(NHi)-YNode(NNde)
          DHi = SQRT(DXHi**2+DYHi**2)
          DXHi = DXHi/DHi
          DYHi = DYHi/DHi
          DO NNde = NHi,NdxLBnNd(NBnd)
            DXLo = DXHi
            DYLo = DYHi
            IF (NNde.LT.NdxLBnNd(NBnd)) THEN
              NHi = NNde+1
            ELSE
              NHi = NdxLBnNd(NvNmBn(NmNghLNd(NBnd))-1)+1
            END IF
C
            DXHi = XNode(NHi)-XNode(NNde)
            DYHi = YNode(NHi)-YNode(NNde)
            DHi = SQRT(DXHi**2+DYHi**2)
            DXHi = DXHi/DHi
            DYHi = DYHi/DHi
            CosNNde = -(DXHi*DXLo+DYHi*DYLo)
            SinNNde = DXHi*DYLo-DYHi*DXLo
            AngHiLo = ACOS(CosNNde)
C
            IF (SinNNde.LT.0.) THEN
C             0 <= AngFrac <= 180.
              AngFrac = AngHiLo/TwoPi
            ELSE
C             180 <= AngFrac <= 360.
              AngFrac = 1.-AngHiLo/TwoPi
            END IF
C           Correct for the spanned angular sector.
            RadVvr(NNde) = RadVvr(NNde)/AngFrac
C
          END DO
        END IF
      END DO
C
C     Convert the counter of neighbors into the weight factor
C     alpha according to Richter's formula.
      DO NNde = 5,MNde
        RadVvr(NNde) = MAX(1.D0,5.D0+Beta*(RadVvr(NNde)-6.D0))
      END DO
C
C     Perform MSweep iterations.
      DO NSweep = 1,MSweep
C
C       Erase the old update vector. The update is accumulated
C       temporarily into X/YVvr.
        DO NNde = 5,MNde
          XVvr(NNde) = 0.
          YVvr(NNde) = 0.
        END DO
C
C       Loop over all interior cells to accumulate the distances.
        DO NVvr = 5,MVvr
          IF (.NOT.FlsOutVv(NVvr)) THEN
            N1 = NFrmNde(1,NVvr)
            N2 = NFrmNde(2,NVvr)
            N3 = NFrmNde(3,NVvr)

C           Multiply the coordinates with the weight alpha.
            X1 = XNode(N1)
            Y1 = YNode(N1)
            X2 = XNode(N2)
            Y2 = YNode(N2)
            X3 = XNode(N3)
            Y3 = YNode(N3)
            RDX1 = RelFac*(X3-X2)
            RDY1 = RelFac*(Y3-Y2)
            RDX2 = RelFac*(X1-X3)
            RDY2 = RelFac*(Y1-Y3)
            RDX3 = RelFac*(X2-X1)
            RDY3 = RelFac*(Y2-Y1)
C
C           Accumulate the updates. Relaxation Factor and weight
C           are all wrapped in one formula such that XNode does not
C           appear on the RHS of the update formula.
            XVvr(N1) = XVvr(N1)+RadVvr(N2)*(RDX3+X1)+
     &                          RadVvr(N3)*(-RDX2+X1)
            YVvr(N1) = YVvr(N1)+RadVvr(N2)*(RDY3+Y1)+
     &                          RadVvr(N3)*(-RDY2+Y1)
            XVvr(N2) = XVvr(N2)+RadVvr(N3)*(RDX1+X2)+
     &                          RadVvr(N1)*(-RDX3+X2)
            YVvr(N2) = YVvr(N2)+RadVvr(N3)*(RDY1+Y2)+
     &                          RadVvr(N1)*(-RDY3+Y2)
            XVvr(N3) = XVvr(N3)+RadVvr(N1)*(RDX2+X3)+
     &                          RadVvr(N2)*(-RDX1+X3)
            YVvr(N3) = YVvr(N3)+RadVvr(N1)*(RDY2+Y3)+
     &                          RadVvr(N2)*(-RDY1+Y3)
C
          END IF
        END DO
C
C       Loop over all interior nodes and collect the sum of the
C       weights. The sum of the weights is temporarily stored in XNode.
C       (If only Fortran77 could allocate memory!)
        DO NNde = MBndNde+MVisNode+1,MNde
          XNode(NNde) = 0.
        END DO
        DO NVvr = 5,MVvr
          N1 = NFrmNde(1,NVvr)
          N2 = NFrmNde(2,NVvr)
          N3 = NFrmNde(3,NVvr)
          IF (N1.GT.MBndNde+MVisNode)
     &      XNode(N1) = XNode(N1)+RadVvr(N2)+RadVvr(N3)
          IF (N2.GT.MBndNde+MVisNode)
     &      XNode(N2) = XNode(N2)+RadVvr(N3)+RadVvr(N1)
          IF (N3.GT.MBndNde+MVisNode)
     &      XNode(N3) = XNode(N3)+RadVvr(N1)+RadVvr(N2)
        END DO
C
C       Update the coordinates. Refill XNode with the coordinate.
C       Calculate an L2 norm of the Y updates.
        YDiffMax = 0.
        YDiffSum = 0.
        NNdeDiffMax = 0
        DO NNde = MBndNde+MVisNode+1,MNde
          YNew = YVvr(NNde)/XNode(NNde)
          YDiff = (YNew-YNode(NNde))**2
          YDiffSum = YDiffSum+YDiff
          IF (YDiff.GT.YDiffMax) THEN
            YDiffMax = YDiff
            NNdeDiffMax = NNde
          END IF
          YNode(NNde) = YNew
          XNode(NNde) = XVvr(NNde)/XNode(NNde)
        END DO
C
C       Convergence. Take the number of samples out of the root
C       since YDiffSum scales with n*YDiff**2, and YDiff scales with
C       sqrt(n). Thus, YDiffL2 should be independent of the mesh size.
        YDiffL2 = LOG10(SQRT(YDiffSum)/(MNde-MBndNde-MVisNode)+1.E-25)
        YDiffMaxL2 = LOG10(SQRT(YDiffMax)/
     &               (MNde-MBndNde-MVisNode)+1.E-25)
        IF (IVerbose.GT.3)
     &    WRITE (*,11) NSweep,YDiffL2,YDiffMaxL2,NNdeDiffMax,
     &                 XNode(NNdeDiffMax),YNode(NNdeDiffMax)
        IF (FLogFile)
     &    WRITE (NtLog,11) NSweep,YDiffL2,YDiffMaxL2,NNdeDiffMax,
     &                 XNode(NNdeDiffMax),YNode(NNdeDiffMax)
 11     FORMAT (6X,I3,'. rel.sweep, L2:',F6.2,'  (',F6.2,
     &          ' at',I7,':',E11.4,'/',E11.4,'),')
C
      END DO
C
      RETURN
      END
