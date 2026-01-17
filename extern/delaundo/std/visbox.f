      SUBROUTINE VISBOX (LsFlgVvr,FlsFlgVv,FlsOutVv,FAskRow,MVisRow,
     &                   IVerbose,NtLog,FLogFile)
C
C Last update:
C 7Dec93,16:00;fix confusion with connectivity of wakes. NmNfhFnd of
C              the wake is to the left of the juncture, as viewed
C              from the wake.
C 22Aug93,16:05;remove premature exit in element loop.
C 10Dec92,20:00;intro FAskRow and MVisRow
C 25Jun92,20:44;Unprotect the outermost layer of cells.
C 25Jun92,13:40;Pass DMax via /BKGRD/.
C 22Jun92,21:38;Pass NNdePrv, NNdeNxt and NNewPrv to BUILDNDE.
C 21Jun92,18:13;use the same DMax for all segments.
C 18Jun92,17:26;established.
C 
C Emulate structured gridding in form of flat cells aligned with the
C surface in the areas that belong to uniquely one surface. Welcome to
C DOMINO software.
C
C Input:
C LsFlgVvr: List of flagged cells in the cavity (allocated in DELAUNDO).
C FlsFlgVv: Vector of flags for the cavity (allocated in DELAUNDO).
C FlsOutVv: Vector of flags, .TRUE. for cells outside the domain and
C           protected cells.
C FAskRow:  .TRUE. if more rows are to be prompted after MVisRow>MRow.
C MVisRow:  No. of rows to produce.
C /NODES/:  Nodes, boundaries, frontal strings and all that.
C /VVRTC/:  Connectivity.
C /BKGRD/:  Background grid and related things.
C
C Output:
C /NODES/:  More nodes, boundaries, frontal strings and all that.
C /VVRTC/:  Updated connectivity.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      INCLUDE 'vvrtc.cbl'
      INCLUDE 'bkgrd.cbl'
      DIMENSION LsFlgVvr(*),FlsFlgVv(*),FlsOutVv(*),NghFWk(MaxBound),
     &          NghLWk(MaxBound)
C
      IF (IVerbose.GE.3) THEN
        WRITE (*,'(/3X,A)')
     &  ' Creating stretched triangles for viscous layers.'
      END IF
      IF (FLogFile) THEN
        WRITE (NtLog,'(/3X,A)')
     &  ' Creating stretched triangles for viscous layers.'
      END IF
C
C     Reset the number of nodes to be introduced.
      MNewNde = 0
      MBndNde = NdxLBnNd(MBnd)
      MIniBnd = MBnd
      MRow = 0
C
C     Establish pointers between regular and wake segments.
      DO NBnd = 1,MBnd
        NghFWk(NBnd) = 0
        NghLWk(NBnd) = 0
      END DO
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).EQ.4) THEN
          IF (NmNghFNd(NBnd).NE.0) THEN
C           This wake is connected at the beginning. Note that wakes can
C           be either left open at the beginning or can be connected. They
C           may not be connected at the end.
            NghF = NvNmBn(NmNghFNd(NBnd))
            NghFWk(NghF) = NBnd
            NghLWk(NvNmBn(NmNghFNd(NghF))) = NBnd
          END IF
        END IF
      END DO
C
C     Put up initial loops around each element. Pick any regular segment
C     and walk until an impinging wake or its own beginning is found.
C     The string to be formed stretches from there to the open end of 
C     the next impinging wake or the wraparound.
      NNBnd = 0
      DO WHILE (NNBnd.LT.MIniBnd.AND.MRow.LT.MVisRow)
        NNBnd = NNBnd+1
        MBndBefore = MBnd
C
        IF (IFront(NNBnd).EQ.NNBnd.AND.
     &      (IBndType(NNBnd).EQ.1.OR.IBndType(NNBnd).EQ.4)) THEN
          FOpen = .TRUE.
          NBnd = NNBnd
          DO WHILE (FOpen)
            NPrvBnd = NghFWk(NBnd)
            IF (NPrvBnd.EQ.0) NPrvBnd = NvNmBn(NmNghFNd(NBnd))
            IF (IBndType(NPrvBnd).EQ.4.OR.
     &          NPrvBnd.EQ.NNBnd) FOpen = .FALSE.
            NBnd = NPrvBnd
          END DO
C
          NBndTrgt = 0
          DO WHILE (NBndTrgt.NE.NBnd)
C
C           Open a new frontal segment.
            FoundNde = .FALSE.
            MBnd = MBnd+1
            MRow = MRow+1
            IFront(MBnd) = IFront(NBnd)
            IBndType(MBnd) = 11
            MNewNde = 0
            NNewPrv = 0
            FOpen = .TRUE.
C
C           Initialize the target.
            IF (NBndTrgt.EQ.0) NBndTrgt = NBnd
C
            IF (IBndType(NBnd).EQ.4) THEN
C             Build backwards on this wake. Note that the reversion of
C             the connecting nodes and the normals is done within 
C             BUILDNDE. Hence, the first and last node are given in
C             forward sense.
              NdeNxtBnd = 0
              FNxtBnd = .FALSE.
              NmPrvBnd = NvNmBn(NmNghLNd(NBnd))
              IF (NmPrvBnd.EQ.0) THEN
                FPrvBnd = .FALSE.
              ELSE
                NdePrvBnd = NdxLBnNd(NPrvBnd)
                FPrvBnd = .TRUE.
              END IF
              NdeLo = NdxLBnNd(NBnd-1)+1
              NdeHi = NdxLBnNd(NBnd)
              NdeInc = -1
C             Prevent a splitting of the string in scanbnd.
              NBndType = 0
              CALL SCANBND (NBnd,MBnd,MRow,IFront,NBndType,
     &                      NdeLo,NdeHi,NdeInc,
     &                      NdePrvBnd,NdeNxtBnd,FPrvBnd,FNxtBnd,NNewPrv,
     &                      XNode,YNode,FoundNde,MNde,
     &                      MNewNde,LsFlgVvr,FlsFlgVv,FlsOutVv)
C
C             Take the next boundary.
              IF (NmNghFNd(NBnd).EQ.0) THEN
C               This is an open-ended wake. I.e. keep NBnd and have
C               it treated from the other side the next time.
C
              ELSE
C               Build on the corner formed with the three end nodes
C               from the two regular boundaries and the end node of the
C               impinging wake. Note that NBnd carries the following
C               regular boundary.
                NPrvBnd = NBnd
                FPrvBnd = .TRUE.
                NxtBnd = NvNmBn(NmNghFNd(NPrvBnd))
                NBnd = NvNmBn(NmNghFNd(NxtBnd))
                FNxtBnd = .TRUE.
                NdeNxtBnd = NdxLBnNd(NxtBnd-1)+1
                NdePrvBnd = NdxLBnNd(NPrvBnd-1)+1
                NdeLo = NdxLBnNd(NBnd)
                NdeHi = NdeLo
                NdeInc = 1
C               Prevent a splitting of the string in scanbnd.
                NBndType = 0
                CALL SCANBND (NBnd,MBnd,MRow,IFront,NBndType,
     &                        NdeLo,NdeHi,NdeInc,
     &                        NdePrvBnd,NdeNxtBnd,FPrvBnd,FNxtBnd,
     &                        NNewPrv,XNode,YNode,FoundNde,MNde,
     &                        MNewNde,LsFlgVvr,FlsFlgVv,FlsOutVv)
C
C               Increment to the next boundary segment.
                NPrvBnd = NBnd
                NdePrvBnd = NdeLo
                NBnd = NxtBnd
C
              END IF

            ELSE
C             Initialize the previous boundary.
              NPrvBnd = NvNmBn(NmNghFNd(NBnd))
              NdePrvBnd = NdxLBnNd(NPrvBnd)
              FPrvBnd = .TRUE.
C
            END IF
C
            DO WHILE (FOpen)
C             Build forwards along segments.
              IF (IBndType(NBnd).EQ.4) THEN
C               This is an open-ended wake.
                FNxtBnd = .FALSE.
                NdeNxtBnd = 0
                FOpen = .FALSE.
              ELSE
C               This is a boundary in a closed loop.
                NxtBnd = NghLWk(NBnd)
                IF (NxtBnd.EQ.0) NxtBnd = NvNmBn(NmNghLNd(NBnd))
                NdeNxtBnd = NdxLBnNd(NxtBnd-1)+1
                FNxtBnd = .TRUE.
              END IF
              NdeLo = NdxLBnNd(NBnd-1)+1
              NdeHi = NdxLBnNd(NBnd)
              NdeInc = 1
C             Prevent 'cornering' at wakes.
              NBndType = IBndType(NxtBnd)
              CALL SCANBND (NBnd,MBnd,MRow,IFront,NBndType,
     &                      NdeLo,NdeHi,NdeInc,
     &                      NdePrvBnd,NdeNxtBnd,FPrvBnd,FNxtBnd,NNewPrv,
     &                      XNode,YNode,FoundNde,MNde,
     &                      MNewNde,LsFlgVvr,FlsFlgVv,FlsOutVv)
C
C             Increment to the next boundary segment.
              NdePrvBnd = NdxLBnNd(NBnd)
              NPrvBnd = NBnd
              FPrvBnd = .TRUE.
              IF (IBndType(NBnd).EQ.4) THEN
                FOpen = .FALSE.
              ELSE
                NBnd = NxtBnd
              END IF
              IF (NBnd.EQ.NBndTrgt.AND.
     &            IBndType(NBnd).NE.4) FOpen = .FALSE.
            END DO
C
            IF (FoundNde) THEN
C             Close this segment.
              MNde = MNde+MNewNde
              NdxLBnNd(MBnd) = MNde
            ELSE
C             Remove this unfilled segment.
              MBnd = MBnd-1
            END IF
C
          END DO
C
          IF (MRow.GE.MVisRow.AND.FAskRow) THEN
            MBuiltNode = NdxLBnNd(MBnd)-NdxLBnNd(MBndBefore)
            WRITE (*,'(6X,I7,A,I3,A,I5,A,$)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                          NmeBnd(IFront(NBnd)),' at',Mbnd,',row',
     &                            MRow,'. How many more rows?'
            IF (FLogFile)
     &      WRITE (NtLog,'(6X,I7,A,I3,A,I5,A,$)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                          NmeBnd(IFront(NBnd)),' at',Mbnd,',row',
     &                            MRow,'. How many more rows?'
            READ (*,*) NXtraRow
            MVisRow = MVisRow+NXtraRow
C
          ELSE IF (MBnd-MBndBefore.NE.0) THEN
            MBuiltNode = NdxLBnNd(MBnd)-NdxLBnNd(MBndBefore)
            IF (IVerbose.GE.3)
     &      WRITE (*,'(6X,I7,A,I3,A,I5,A)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                            NmeBnd(IFront(NBnd)),',row',
     &                            MRow,'.'
            IF (FLogFile)
     &      WRITE (NtLog,'(6X,I7,A,I3,A,I5,A)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                            NmeBnd(IFront(NBnd)),',row',
     &                            MRow,'.'
C
          END IF
C
        END IF
      END DO
C
C     Loop over all type 11 frontal segments.
      NBnd = MIniBnd
      DO WHILE (NBnd.LT.MBnd.AND.MRow.LT.MVisRow)
        NBnd = NBnd+1
        MBndBefore = MBnd
C
        IF (IBndType(NBnd).EQ.11) THEN
C         A subsequently built internal string of nodes.
C         Find first and last node in this segment.
          NdeLo = NdxLBnNd(NBnd-1)+1
          NdeHi = NdxLBnNd(NBnd)
          NdeInc = 1
          NdePrvBnd = 0
          FPrvBnd = .FALSE.
          NdeNxtBnd = 0
          FNxtBnd = .FALSE.
C
          IF (NdeHi-NdeLo.GE.1) THEN
C           There is at least one face in this segment. We can build.
C           Loop over all nodes to build new nodes.
            NdeInc = 1
C           Open a new frontal segment.
            FoundNde = .FALSE.
            MBnd = MBnd+1
            MRow = MRow+1
            IFront(MBnd) = IFront(NBnd)
            IBndType(MBnd) = 11
            MNewNde = 0
            NNewPrv = 0
            CALL SCANBND (NBnd,MBnd,MRow,IFront,IBndType(NBnd),
     &                    NdeLo,NdeHi,NdeInc,
     &                    NdePrvBnd,NdeNxtBnd,FPrvBnd,FNxtBnd,NNewPrv,
     &                    XNode,YNode,FoundNde,MNde,
     &                    MNewNde,LsFlgVvr,FlsFlgVv,FlsOutVv)
C
            IF (FoundNde) THEN
C             Close this segment.
              MNde = MNde+MNewNde
              NdxLBnNd(MBnd) = MNde
            ELSE
C             Remove this unfilled segment.
              MBnd = MBnd-1
            END IF
C
          END IF
C
        END IF
C
        IF (MRow.GE.MVisRow.AND.FAskRow) THEN
          MBuiltNode = NdxLBnNd(MBnd)-NdxLBnNd(MBndBefore)
          WRITE (*,'(6X,I7,A,I3,A,I5,A,$)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                            NmeBnd(IFront(NBnd)),',row',
     &                            MRow,'. How many more rows?'
            IF (FLogFile)
     &      WRITE (NtLog,'(6X,I7,A,I3,A,I5,A,$)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                            NmeBnd(IFront(NBnd)),',row',
     &                            MRow,'. How many more rows?'
          READ (*,*) NXtraRow
          MVisRow = MVisRow+NXtraRow
C
        ELSE IF (MBnd-MBndBefore.NE.0) THEN
          MBuiltNode = NdxLBnNd(MBnd)-NdxLBnNd(MBndBefore)
          IF (IVerbose.GE.3)
     &    WRITE (*,'(6X,I7,A,I3,A,I5,A)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                    NmeBnd(IFront(NBnd)),',row',MRow,'.'
          IF (FLogFile)
     &    WRITE (NtLog,'(6X,I7,A,I3,A,I5,A)') MBuiltNode,
     &                            ' nodes around boundary named',
     &                    NmeBnd(IFront(NBnd)),',row',MRow,'.'
C
        END IF
C
C       Shift the counters of strings one position down, but leave
C       the string that was just processed to retain a lower
C       index to the string that will be processed next.
        IF (NBnd.EQ.MIniBnd+2) THEN
          DO NNBnd = NBnd,MBnd
            NdxLBnNd(NNBnd-1) = NdxLBnNd(NNBnd)
            IBndType(NNBnd-1) = IBndType(NNBnd)
            IFront(NNBnd-1) = IFront(NNBnd)
          END DO
          NBnd = NBnd-1
          MBnd = MBnd-1
        END IF
C
      END DO
C
C     Protect all cells that have only protected neighbors.
      DO NVvr = 5,MVvr
        Ngh1 = NghVvr(1,NVvr)
        Ngh2 = NghVvr(2,NVvr)
        Ngh3 = NghVvr(3,NVvr)
        IF (FlsOutVv(Ngh1).AND.FlsOutVv(Ngh2).AND.FlsOutVv(Ngh3)) THEN
          FlsOutVv(NVvr) = .TRUE.
        END IF
      END DO
C
      RETURN
      END
