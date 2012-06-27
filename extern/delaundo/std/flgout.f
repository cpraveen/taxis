      SUBROUTINE FLGOUT (LsOutVvr,FlsOutVv,MOutVvr,
     &                   MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &                   NvNmBn,IBndType,MNde,XNode,YNode,
     &                   MVvr,NFrmNde,NghVvr,
     &                   NLevel,ILevel)
C
C Last update:

C 5Mar95; must have been stone-drunk or working for 48 hours straight
C         when I did the change below. That must be utter garbage. Or
C         am I stone-drunk now? Have only been working for 12 hours!
C         Anyhow, undo the change below. While we're at it, trying to
C         understand, loop over all the neighbors of the cell outside
C         the domain formed with NLmtNde only, if that cell is not
C         yet in the list. Otherwise we've done that already, e.g. as
C         in interior flows with many connected segments.
C 15Jan95; set IWrkVvr to last value of MOutVvr.
C 21Apr94,16:45; intro NGHBNDE.
C
C Flag the triangles outside the domain.
C
C Input:
C LsOutVvr: Vector of at least MVvr.
C FLsOutVv: Vector of at least MVvr.
C MLmt:     Number of elements of the configuration.
C LmtNde:   Vector of one node for each element.
C MBnd:     Number of boundary segments.
C NdxLBnNd: Vector of indices of the last node on each
C           boundary segment.
C NmNghFNd: Vector of the names of the boundary neighbouring 
C           to the first node of the segment.
C NmNghLNd: Vector of the names of the boundary neighbouring 
C           to the last node of the segment.
C NvNmBnd:  Vector of numbers of the boundaries given the name.
C IBndType: Type of boundary. See READFIP/READPTS for details.
C MNde:     Number of nodes.
C X/YNode:  Node coordinates.
C MVvr:     Number of cells.
C NFrmNde:  Forming nodes of the cells.
C NghVvr:   Neighboring cells.
C NLevel:   Gridlevel.
C ILevel:   Vector of grid level indicators of each node.
C
C Output:
C LsOutVvr:  Sequential list of flagged Voronoi vertices.
C FLsOutVv:  Full array of flags stating that the corresponding
C            Voronoi vertex has been flagged, when set to true.
C MOutVvr:   Amount of flagged Voronoi vertices.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DOUBLE PRECISION ScPrd
      DIMENSION LsOutVvr(*),FlsOutVv(*),LmtNde(*),NdxLBnNd(0:*),
     &          NmNghFNd(*),NmNghLNd(*),NvNmBn(-1:*),IBndType(*),
     &          XNode(*),YNode(*),NFrmNde(3,*),NghVvr(3,*),ILevel(*)
C     Search upward.
      NdeInc = 1
C
C     Erase the vector of flags.
      DO NVvr = 1,MVvr
        FlsOutVv(NVvr) = .FALSE.
      END DO
C     Reset counter of flagged Voronoi vertices.
      MOutVvr = 0
C
C     Take the first set of boundaries, i.e. the first element.
      DO NLmt = 1,MLmt
C       Index of the pointer to the element.
        NLmtNde = LmtNde(NLmt)
        IF (NLevel.GT.1) THEN
C         On a coarser level, take the last node of the segment
C         as the pivot, since it has to be on the coarsened segment.
          NBnd = 1
          DO WHILE (NdxLBnNd(NBnd).LT.NLmtNde)
            NBnd = NBnd+1
          END DO
          NLmtNde = NdxLBnNd(NBnd)
        END IF
        CALL NGHBNDE (NLmtNde,NdeInc,NLevel,ILevel,MBnd,
     &                NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,IBndType,
     &                IVerbose,FLogFile,NtLog,
     &                NNghNde,NBnd)
C
C       Work only on one-sided frontal boundaries, DON'T touch
C       the wakes or you will lose it all.
        IF (IBndType(NBnd).NE.4) THEN
C
C         Calculate the position of the midpoint between two
C         successive boundary nodes on that element.
          XMid = .5*(XNode(NLmtNde)+XNode(NNghNde))
          YMid = .5*(YNode(NLmtNde)+YNode(NNghNde))
C         Take an initial Voronoi vertex to start the walk.
          NVvrIni = 5
C         Walk through the neighbouring Voronoi vertices.
          NVvr = IWALK(XMid,YMid,NVvrIni,XNode,YNode,NFrmNde,NghVvr,
     &                 MVvr,FOutCnv)
          IF (FOutCnv) THEN
            WRITE (*,'(A/A,I7)') 
     &      'FATAL: Search leading outside of convex hull in FLGOUT.',
     &      '       NVvr:',NVvr
            STOP
          END IF
C
C         Find the third node in the current triangle.
          DO K3 = 1,3
            N3 = NFrmNde(K3,NVvr)
            IF (N3.NE.NLmtNde.AND.N3.NE.NNghNde) THEN
              N3Nde = N3
              K3Nde = K3
            END IF
          END DO
C         Calculate the scalar product outward boundary normal - 
C         vector from the midpoint of the edge to the third point.
          DXNrml = YNode(NLmtNde)-YNode(NNghNde)
          DYNrml = XNode(NNghNde)-XNode(NLmtNde)
          DXVec = XNode(N3Nde)-XMid
          DYVec = YNode(N3Nde)-YMid
          ScPrd = DXNrml*DXVec+DYNrml*DYVec
C         IF the scalar product is positive,
C         this Voronoi vertex is within the domain. Switch to the
C         neighbouring Voronoi vertex on the other side of the
C         boundary.
          IF (ScPrd.GT.0) NVvr = NghVvr(K3Nde,NVvr) 
C
          IF ( .NOT. FlsOutVv(NVvr) ) THEN
C           Append the found Voronoi vertex to the
C           list for deletion, LsOutVvr, and flag it in FlsOutVv.
            MOutVvr = MOutVvr+1
            LsOutVvr(MOutVvr) = NVvr
            FlsOutVv(NVvr) = .TRUE.
C           
C           Loop over the neighbours of each new entry in the flagged list.
            IWrkVvr = MOutVvr
            DO WHILE ( IWrkVvr .LE. MOutVvr )
              DO KNghVvr = 1,3
C               
C               Number of the current neighbouring Voronoi vertex.
                NNghVvr = NghVvr(KNghVvr,LsOutVvr(IWrkVvr))
C               Number of the lower node on the common face.
                NLoNde = NFrmNde(JCYCL(KNghVvr+1),LsOutVvr(IWrkVvr))
C               Number of the higher node on the common face.
                NHiNde = NFrmNde(JCYCL(KNghVvr+2),LsOutVvr(IWrkVvr))
C               Neighboring node on a boundary.
                IF (NHiNde.GT.NdxLBnNd(MBnd)) THEN
C                 This is an interior node.
                  NNghNde = NHiNde
                ELSE
C                 This is some boundary node.
C                 Find the neighboring node to that node on the boundary.
                  CALL NGHBNDE (NHiNde,NdeInc,NLevel,ILevel,MBnd,
     &                          NdxLBnNd,NmNghFNd,NmNghLNd,NvNmBn,
     &                          IBndType,IVerbose,FLogFile,NtLog,
     &                          NNghNde,NBnd)
C                 Invalidate NNghNde for non-physical boundaries.
                  IF (NNghNde.LT.0) NNghNde = NHiNde
                END IF
C               
                IF (.NOT.(FlsOutVv(NNghVvr)
C                      Voronoi vertex already flagged.
     &                    .OR. NNghNde.EQ.NLoNde
C                            This is a boundary not to be crossed.
     &                    .OR. NNghVvr.LE.4))
C                            Voronoi vertex is non physical.
     &             THEN
C                 The Voronoi vertex found is outside the
C                 domain and will be flagged.
                  MOutVvr = MOutVvr+1
                  LsOutVvr(MOutVvr) = NNghVvr
                  FlsOutVv(NNghVvr) = .TRUE.
                END IF
C
              END DO
C             
              IWrkVvr = IWrkVvr+1
            END DO

          END IF
C
        END IF
C
      END DO
C
C     Flag the non-physical Voronoi vertices #1 to #4.
      DO NVvr = 1,4
        LsOutVvr(MOutVvr+NVvr) = NVvr
        FlsOutVv(NVvr) = .TRUE.
      END DO
      MOutVvr = MOutVvr+4
C
      RETURN
      END
