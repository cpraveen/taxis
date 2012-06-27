      SUBROUTINE SCANBND (NBnd,MBnd,MRow,IFront,NBndType,
     &                    NdeLo,NdeHi,NdeInc,
     &                    NdePrvBnd,NdeNxtBnd,FPrvBnd,FNxtBnd,NNewPrv,
     &                    XNode,YNode,FoundNde,MNde,
     &                    MNewNde,LsFlgVvr,FlsFlgVv,FlsOutVv)
C
C Last update:
C 23Aug93,19:10; pass FTruncate to BUILDNDE to truncate a string at
C                a corner.
C 22Aug93,16:02; fix bug with wrong sin-sign in corner test.
C 
C Take the given boundary between NdeLo and NdeHi and scan it in
C the given direction NdeInc to construct viscous nodes. Welcome to
C DOMINO software.
C
C Input:
C NBnd:     Number of the current boundary.
C NdeLo:    First node to loop over.
C NdeHi:    Last node to loop over.
C NdeInc:   Loop increment for the loop over the nodes.
C NdePrvBnd: Node on the string prior to NdeLo.
C NdeNxtBnd: Node on the string after NdeHi.
C FPrvBnd:  .T. if there is a NdePrvBnd.
C FNxtBnd:  .T. if there is a NdeNxtBnd.
C NNewPrv:  Most recent node on the string that is being created.
C MBnd:     Amount of boundaries.
C MRow:     Amount of rows.
C IFront:   Vector relating a string of nodes to the solid boundary
C           it was built from.
C X/YNode:  Vectors of the coordinates of the nodes.
C PBNNde:   Pointer to a background cell near NBnd.
C PFNew:    Pointer to a foreground cell near NBnd.
C MNde:     Number of old nodes.
C MNewNde:  Number of new nodes.
C LsFlgVvr: Workspace for the introduction routines.
C FlsFlgVv: Workspace for the introduction routines.
C FlsOutVv: Vector of flags for cells outside the domain.
C
C Output:
C FoundNde: .T. if a node has been found.
C MNewNde:  Number of new nodes.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      DOUBLE PRECISION XNode(*),YNode(1)
      DIMENSION LsFlgVvr(*),FlsFlgVv(*),FlsOutVv(*),IFront(*)
C
C     Initialize pointers for the search.
      PBNNde = 5
      PFNew = 5
C
C     Loop over all nodes to build new nodes.
      IF (NdeInc.EQ.1) THEN
        NStart = NdeLo
        NEnd = NdeHi
      ELSE
        NStart = NdeHi
        NEnd = NdeLo
      END IF
C
      DO NNde = NStart,NEnd,NdeInc
C
        IF (NNde.EQ.NdeLo) THEN
C         This node is the first in its row. The segment is
C         either open or linked to another boundary, possibly
C         itself.
          IF (FPrvBnd) THEN
C           A frontal boundary in a closed loop.
            NNdePrv = NdePrvBnd
            DXLo = XNode(NNde)-XNode(NNdePrv)
            DYLo = YNode(NNde)-YNode(NNdePrv)
            FLo = .TRUE.
            FrstNde =  .TRUE.
          ELSE
C           The segment is open at this end.
            FLo = .FALSE.
            FrstNde =  .FALSE.
            NNdePrv = 0
          END IF
C
        ELSE
C         This is an interior face.
          DXLo = XNode(NNde)-XNode(NNde-1)
          DYLo = YNode(NNde)-YNode(NNde-1)
          FLo = .TRUE.
          NNdePrv = NNde-1
          FrstNde =  .FALSE.
C
        END IF
C
        IF (NNde.EQ.NdeHi) THEN
C         This node is the last in its row. The segment is
C         either open or linked to another boundary, possibly
C         itself.
          IF (FNxtBnd) THEN
C           A frontal boundary in a closed loop.
            NNdeNxt = NdeNxtBnd
            DXHi = XNode(NNdeNxt)-XNode(NNde)
            DYHi = YNode(NNdeNxt)-YNode(NNde)
            FHi = .TRUE.
            FLstNde =  .TRUE.
          ELSE
C           The segment is open at this end.
            FHi = .FALSE.
            FLstNde =  .FALSE.
            NNdeNxt = 0
          END IF
C
        ELSE
C         This is an interior face in a single segment.
          FLstNde =  .FALSE.
          DXHi = XNode(NNde+1)-XNode(NNde)
          DYHi = YNode(NNde+1)-YNode(NNde)
          FHi = .TRUE.
          NNdeNxt = NNde+1
c
        END IF
C
C       Calculate the normal(s).
        IF (FLo.AND.FHi) THEN
C         There are two faces. 
          IF (FrstNde) THEN
C           There is a first node on a segment, possibly 
C           this segment begins just after a corner.
            IF (NNdePrv.NE.0) THEN
C             Calculate the scalar product to detect the corner.
              DXPrv = XNode(NNdePrv)-XNode(NNdePrv-1)
              DYPrv = YNode(NNdePrv)-YNode(NNdePrv-1)
              DPrv = DYPrv**2+DXPrv**2
              DLo = DYLo**2+DXLo**2
              DHi = DYHi**2+DXHi**2
              CosDlt = (DYLo*DYPrv+DXLo*DXPrv)/SQRT(DLo)/SQRT(DPrv)
              SinDlt = DXLo*DYPrv-DYLo*DXPrv
              IF (CosDlt.LT..5.AND.SinDlt.GT.0.
     &            .AND.NBndType.EQ.1) THEN
C               This is a convex corner, like e.g. a trailing edge.
C               Build two nodes, one with a direction normal
C               to the lower face from NNdePrv, one normal to the
C               bisector from NNde. Start with the lower face.
                XNormX = -.5D0*(DYHi+DYLo)
                YNormX = .5D0*(DXHi+DXLo)
                NNdeX = NNde
                NNdeXPrv = NNdePrv
                NNdeXNxt = NNdeNxt
                FCrnr = .TRUE.
                XNorm = -DYLo
                YNorm = DXLo
                NNdeBuild = NNdePrv
                NNdePrv = 0
                NNdeNxt = NNde
              ELSE
C               Build along the bisector.
                FCrnr = .FALSE.
                DHi = SQRT(DXHi**2+DYHi**2)
                DLo = SQRT(DXLo**2+DYLo**2)
                XNorm = -DYHi/DHi-DYLo/DLo
                YNorm = DXHi/DHi+DXLo/DLo
                DNorm = SQRT(XNorm**2+YNorm**2)
                XNorm = XNorm/DNorm*(DHi+DLo)*.5
                YNorm = YNorm/DNorm*(DHi+DLo)*.5
                NNdeBuild = NNde
              END IF
C
            ELSE
C             Build along the bisector.
              FCrnr = .FALSE.
              DHi = SQRT(DXHi**2+DYHi**2)
              DLo = SQRT(DXLo**2+DYLo**2)
              XNorm = -DYHi/DHi-DYLo/DLo
              YNorm = DXHi/DHi+DXLo/DLo
              DNorm = SQRT(XNorm**2+YNorm**2)
              XNorm = XNorm/DNorm*(DHi+DLo)*.5
              YNorm = YNorm/DNorm*(DHi+DLo)*.5
              NNdeBuild = NNde
            END IF
C
          ELSE IF (FLstNde) THEN
C           There is a last node on a segment, possibly a corner.
C           Calculate the scalar product to detect the corner.
            DLo = DYLo**2+DXLo**2
            DHi = DYHi**2+DXHi**2
            CosDlt = (DYLo*DYHi+DXLo*DXHi)/SQRT(DLo)/SQRT(DHi)
            SinDlt = DXHi*DYLo-DYHi*DXLo
            IF (CosDlt.LT..5.AND.NBndType.EQ.1.AND.
     &          SinDlt.GT.0.) THEN
C             Build a node with a direction normal to the lower face
C             face from NNde. Truncate the string after this node.
              FTruncate = .TRUE.
              FCrnr = .FALSE.
              XNorm = -DYLo
              YNorm = DXLo
              NNdeBuild = NNde
            ELSE
C             Build along the bisector.
              FCrnr = .FALSE.
              DHi = SQRT(DXHi**2+DYHi**2)
              DLo = SQRT(DXLo**2+DYLo**2)
              XNorm = -DYHi/DHi-DYLo/DLo
              YNorm = DXHi/DHi+DXLo/DLo
              DNorm = SQRT(XNorm**2+YNorm**2)
              XNorm = XNorm/DNorm*(DHi+DLo)*.5
              YNorm = YNorm/DNorm*(DHi+DLo)*.5
              NNdeBuild = NNde
            END IF
C
          ELSE
C           Build along the bisector.
            FCrnr = .FALSE.
            DHi = SQRT(DXHi**2+DYHi**2)
            DLo = SQRT(DXLo**2+DYLo**2)
            XNorm = -DYHi/DHi-DYLo/DLo
            YNorm = DXHi/DHi+DXLo/DLo
            DNorm = SQRT(XNorm**2+YNorm**2)
            XNorm = XNorm/DNorm*(DHi+DLo)*.5
            YNorm = YNorm/DNorm*(DHi+DLo)*.5
            NNdeBuild = NNde
C
          END IF
C
        ELSE IF (FLo) THEN
C         There is only a lower face. This is the end of
C         the segment.
          FCrnr = .FALSE.
          XNorm = -DYLo
          YNorm = DXLo
          NNdeBuild = NNde
C
        ELSE IF (FHi) THEN
C         There is only a higher face. This is the beginning of
C         the segment.
          FCrnr = .FALSE.
          XNorm = -DYHi
          YNorm = DXHi
          NNdeBuild = NNde
C
        ELSE
          WRITE (*,'(2A)') ' This shouldn''t have happened in SCANBND.',
     &                     ' No face for this node.'
          STOP
C
        END IF
C
C       Reverse the direction if nodes are to be built to the
C       right hand side of a IBndType=4 boundary, e.g. a wake.
        IF (NdeInc.EQ.-1) THEN
          XNorm = -XNorm
          YNorm = -YNorm
          NNdeTmp = NNdePrv
          NNdePrv = NNdeNxt
          NNdeNxt = NNdeTmp
          IF (FCrnr) THEN
            XNormX = -XNormX
            YNormX = -YNormX
            NNdeTmp = NNdeXPrv
            NNdeXPrv = NNdeXNxt
            NNdeXNxt = NNdeTmp
          END IF
        END IF
C
C       Build a node upon NNdeBuild.
        CALL BUILDNDE (XNorm,YNorm,NNdeBuild,NNdePrv,NNdeNxt,
     &                 NNewPrv,PBNNde,PFNew,NBnd,FoundNde,MNewNde,
     &                 LsFlgVvr,FlsFlgVv,FlsOutVv,FTruncate)
C
        IF (FCrnr) THEN
C         Build a node upon NNdeBNxt.
          CALL BUILDNDE (XNormX,YNormX,NNdeX,NNdeXPrv,
     &                   NNdeXNxt,NNewPrv,PBNNde,PFNew,NBnd,
     &                   FoundNde, MNewNde,LsFlgVvr,FlsFlgVv,
     &                   FlsOutVv,FTruncate)
        END IF
C
      END DO
C
      RETURN
      END
