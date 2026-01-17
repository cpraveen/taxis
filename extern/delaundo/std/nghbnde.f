      SUBROUTINE NGHBNDE (NNde,NdeInc,NLevel,ILevel,MBnd,
     &                    NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                    IVerbose,FLogFile,NtLog,
     &                    NghNde,NBnd)
C
C Last update:
C 15Jan95; fix bug with boundary number 0 on return from open
C          ended wake.
C 26Sep94;conceived.
C
C Find the next node up or down on the element.
C
C Input:
C NNde:     Node that needs a neighbor.
C NdeInc:   Direction of the neighbor.
C NLevel:   Gridlevel.
C NdxLBnNd: Vector of indices of the last node on each
C ILevel:   Level indicator for each node.
C           boundary segment.
C NmNghFNd: Vector of the names of the boundary neighbouring 
C           to the first node of the segment.
C NmNghLNd: Vector of the names of the boundary neighbouring 
C           to the last node of the segment.
C NvNmBnd:  Vector of numbers of the boundaries given the name.
C IBndType: Type of boundary. See READFIP/READPTS for details.
C IVerbose,FLogFile,NtLog...
C
C Output:
C NghNde:   Neighbor to node NNde on a possibly coarsened segment
C           in direction NdeInc.
C NBnd:     Number of the boundary segment of NNde.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION NdxLBnNd(0:*),NmNghFNd(*),NmNghLNd(*),NvNmBn(-1:*),
     &          IBndType(*),ILevel(*)
C
C     Find the boundary segment of NNde
      NBnd = 1
      DO WHILE (NdxLBnNd(NBnd).LT.NNde)
        NBnd = NBnd+1
      END DO
      NBndNNde = NBnd
      IF (IBndType(NBnd).GE.9.OR.IBndType(NBnd).EQ.3) THEN
C       This is a set of background boundary nodes or a set of
C       disconnected interior nodes.
        NghNde = -1
        RETURN
      END IF
C
      FNotFound = .TRUE.
      NghNde = NNde
      MInc = 0
      MaxInc = NdxLBnNd(MBnd)
C
      IF (NdeInc.GE.0) THEN
C       Search forward.
        DO WHILE (FNotFound.AND.MInc.LE.MaxInc)
C         Increment NghNde.
          IF (NghNde.LT.NdxLBnNd(NBnd)) THEN
            NghNde = NghNde+1
            MInc = MInc+1
          ELSE
            NBnd = NvNmBn(NmNghLNd(NBnd))
            IF (NBnd.EQ.0) THEN
C             This is an open end of a wake. Return and list
C             the boundary number.
              NBnd = NBndNNde
            END IF
            NghNde = NdxLBnNd(NBnd-1)+1
            MInc = MInc+1
          END IF
C         Check whether this is the neighbor on the level.
          IF (ILevel(NghNde).GE.NLevel.OR.NLevel.EQ.1)
     &      FNotFound = .FALSE.
        END DO
C
      ELSE
C       Search backward.
        DO WHILE (FNotFound.AND.MInc.LE.MaxInc)
C         Decrement NghNde.
          IF (NghNde.GT.NdxLBnNd(NBnd-1)+1) THEN
            NghNde = NghNde-1
            MInc = MInc+1
          ELSE
            NBnd = NvNmBn(NmNghFNd(NBnd))
            IF (NBnd.EQ.0) THEN
C             This is an open end of a wake. Return and list
C             the boundary number.
              NBnd = NBndNNde
            END IF
            NghNde = NdxLBnNd(NBnd)
            MInc = MInc+1
          END IF
C         Check whether this is the neighbor on the level.
          IF (ILevel(NghNde).GE.NLevel.OR.NLevel.EQ.1)
     &       FNotFound = .FALSE.
        END DO
C
      END IF
C
C     Check whether the search was successful.
      IF (MInc.GT.MaxInc) THEN
        WRITE (*,10) NNde,NLevel
        IF (FLogFile) WRITE (NtLog,10) NNde,NLevel
 10     FORMAT (' FATAL: No neighboring boundary vertex found for'/
     &          '        vertex',I7,' on level',I5/)
        STOP
      END IF
C
      RETURN
      END
