      SUBROUTINE BACKGRD (FlsOutVv,LsFlgVvr,FlsFlgVv,FStretch,
     &                    FNotCon,SpcGrdRt,IVerbose,NtLog,FLogFile,
     &                    ArcRel,DltZero,NdxLArc,NdxNotCon,LsNotCon,
     &                    FAllDisc)
C
C Last update:
C 9Jan97; consider all boundaries doubly open except for types 1,2;
C         reset XYVvr[1-4] at the end.
C 11Aug96: test for bounds of MaxBndNde.
C 20May96; replace WALK with IWALK.
C 13Apr96; treat type 3 boundaries as if they were enforced.
C 16Sep95; allow FNotCol or FAllDisc to act independently.
C 2Dec93,15:40; intro ILLICIT, FAllDisc. Treat stretching of wakes.
C 25Sep93,22:30;intro DltZero, new CALCVVR.
C 10Dec92,19:30;towards 4.0.
C 26Oct92,18:32;replace FLGVVR by FLGVVRCN.
C 30Jun92,16:27;remove RotBkg.
C 29Jun92,18:50;fix bug with IArea nodes.
C 28Jun92,22:14;assign IBndType = 10 to background boundaries.
C 22Jun92,20:22;fix bugs related to StrPw and SpcPw, no IArea for cells
C               outside the domain. fix bug related to FNoArea cases.
C               Use same IFront for all connected boundary segments.
C 21Jun92,15:04;intro check with FoundGap.
C 3Jun92,19:04;fix the bug in assigning IArea.
C 29May92,16:35:You got the ratio!
C 8May92, 22:20. (You got the power!)
C 5May92, 16:45. (reintroduce type9 and illconn nodes again after
C                 stretching modification.)
C 2Mar92, 23:30. (fix bug with non-frontal faces in stretchgrid.)
C 2Mar92, 22:19. (kick *S routines, no spacint increase in stretch.)
C 21Feb92, 23:03. (Fix bug with interpolation on DltStar.)
C 21Feb92, 22:41. (Distinguish CNCTNDE, CNCTSTR, FLGVVR, FVVRSTR.)
C 21Feb92, 02:50. (Intro stretching layer.)
C 16Jul91, 16:10. (Calculating average gradient. Elimination of
C                  unwanted connectivities.)
C
C Copy the existing triangulation of foreground boundaries
C and introduce type 9 boundaries into the background mesh.
C Introduce extra nodes to eliminate unwanted connectitivities
C between boundaries. Introduce nodes to define a stretching
C layer. Welcome to DOMINO software.
C
C Input:
C FlsOutVv: Flaglist of triangles in the initial triangulation that
C           are not inside the domain.
C LsFlgVvr: List of triangles to be retriangulated. Storage 
C           allocated in main program DELAUNDO.
C FlsFlgVv: Flaglist of triangles to be retriangulated. Storage 
C           allocated in main program DELAUNDO.
C FStretch: .TRUE. if a stretching layer is to be built.
C SpcGrdRt: Ratio of spacing gradients at the highest vs the
C           lowest spacing.
C IVerbose: Level of verosdity.
C NtLog:    Logical unit of the logfile.
C FLogFile: IF .T., there is output to the logfile.
C ArcRel:   Relative arclength [0,1] for interpolation stations
C           of the thickness of the inner constantly stretched layer.
C DltZero:  Thickness of that layer.
C NdxLArc:  Index for the last station of each element.
C NdxNotCon: Pointers to different segments within LsNotCon.
C LsNotCon: List of boundaries that may not be connected to a
C           specific one.
C FAllDisc: If .T., all non-consecutive faces between frontal
C           boundaries will be illicit.
C 
C /VVRTC/:  Old connectivity. See main program DELAUNDO for 
C           details.
C /NODES/:  Coordinates of the nodes. See main program
C           DELAUNDO for details.
C
C Output:
C /NODES/:  Coordinates of the nodes. See main program
C           DELAUNDO for details.
C /BKGRD/:  Connectivity of the background mesh. See main program
C           DELAUNDO for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      INCLUDE 'vvrtc.cbl'
      INCLUDE 'bkgrd.cbl'
      DIMENSION FlsFlgVv(1),LsFlgVvr(1),NdxLArc(0:*),ArcRel(*),
     &          DltZero(*),FlsOutVv(1),NrBnd(3),NdePrio(MaxBndNde),
     &          NdxNotCon(0:*),LsNotCon(*)
      REAL X(3),Y(3),SpcMin,SpcMax,SpcGrdRt
      INTEGER IWALK
      MaxNotConNde = (MaxBndNde-MNde)/5
C
      IF (IVerbose.GE.3) THEN
        WRITE (*,'(/3X,A)')
     &  ' Deriving the background mesh.'
      END IF
      IF (FLogFile) THEN
        WRITE (NtLog,'(/3X,A)')
     &  ' Deriving the background mesh.'
      END IF
C
C     Check the dimensions of the grid.
      IF ( Mnde .GT. MaxBndNde ) THEN
        WRITE (*,104) MaxBndNde
        IF ( FLogFile ) WRITE (NtLog,104) MaxBndNde
 104    FORMAT ( ' FATAL: only',I9,' boundary nodes possible.',/,
     &           '        recompile with larger MaxNode or larger',
     &           ' fraction in nodes.cbl.' )
      END IF
C
C     Save the triangulation of the boundaries as a background mesh.
      MBVvr = MVvr
      DO NVvr = 1,MVvr
        NghBVvr(1,NVvr) = NghVvr(1,NVvr)
        NghBVvr(2,NVvr) = NghVvr(2,NVvr)
        NghBVvr(3,NVvr) = NghVvr(3,NVvr)
        NBFrmNde(1,NVvr) = NFrmNde(1,NVvr)
        NBFrmNde(2,NVvr) = NFrmNde(2,NVvr)
        NBFrmNde(3,NVvr) = NFrmNde(3,NVvr)
      END DO
C
C     Introduce type 9 boundaries into the background mesh.
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).EQ.9) THEN
          DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
C
C           Find the background Voronoi vertices to be deleted.
            PBVvr = MBVvr
            PBVvr = IWALK (XNode(NNde),YNode(NNde),PBVvr,
     &                     XNode,YNode,NBFrmNde,NghBVvr,MBVvr,
     &                     FOutCnv)
C           Find the constrained cavity.
            CALL FLGVVRCN (NNde,PBVvr,RadTolMi,MBVvr,XVvr,YVvr,
     &                     NBFrmNde,NghBVvr,RadVvr,FlsOutVv,LsFlgVvr,
     &                     FlsFlgVv,MFlgVvr,FOut)
            IF (FOut) THEN
C             This node could not be introduced.
              WRITE (*,'(A,2(I7,A),3(/I7,2E16.8))') 
     &        ' FATAL: Node',NNde,' could not be introduced'
     &        //' into background triangle',PBVvr,' formed by',
     &        (NBFrmNde(K,PBVvr),XNode(NBFrmNde(K,PBVvr)),
     &         YNode(NBFrmNde(K,PBVvr)),K=1,3)
              STOP
C
            ELSE
C             Connect the new node with the valid structure.
              CALL CNCTNDE (NNde,MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                      RadVvr,LsFlgVvr,FlsFlgVv,MNwVvr)
C
C             Reordering the data structure, deleting obsolete elements.
              CALL PSTPRC (MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,RadVvr,
     &                     LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
            END IF
C
          END DO
        END IF
      END DO
C
C     Calculate the spacing of the nodes on the boundaries.
C     Average the length of both boundary faces formed with
C     this boundary node.
      DO NNde = 1,MNde
        BkgSpc(NNde) = 0.
      END DO
C
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).LE.2) THEN
C         Regular enforced or non-enforced, closed boundary.
          DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)-1
C           Calculate the spacing and accumulate it.
            NNghNde = NNde+1
            DX = XNode(NNde)-XNode(NNghNde)
            DY = YNode(NNde)-YNode(NNghNde)
            Dist = SQRT(DX*DX+DY*DY)
            BkgSpc(NNde) = BkgSpc(NNde)+.5*Dist
            BkgSpc(NNghNde) = BkgSpc(NNghNde)+.5*Dist
          END DO
C         Last node connected to first node on the next segment.
          NNde = NdxLBnNd(NBnd)
          NNghNde = NdxLBnNd(NvNmBn(NmNghLNd(NBnd))-1)+1
          DX = XNode(NNde)-XNode(NNghNde)
          DY = YNode(NNde)-YNode(NNghNde)
          Dist = SQRT(DX*DX+DY*DY)
          BkgSpc(NNde) = BkgSpc(NNde)+.5*Dist
          BkgSpc(NNghNde) = BkgSpc(NNghNde)+.5*Dist
C
        ELSE 
C         All other boundary types are considred doubly open.
          DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)-1
C           Calculate the spacing and accumulate it.
            NNghNde = NNde+1
            DX = XNode(NNde)-XNode(NNghNde)
            DY = YNode(NNde)-YNode(NNghNde)
            Dist = SQRT(DX*DX+DY*DY)
            BkgSpc(NNde) = BkgSpc(NNde)+.5*Dist
            BkgSpc(NNghNde) = BkgSpc(NNghNde)+.5*Dist
          END DO
C         Correct the two open ends.
          BkgSpc(NNghNde) = 2.*BkgSpc(NNghNde)
          NNde = NdxLBnNd(NBnd-1)+1
          BkgSpc(NNde) = 2.*BkgSpc(NNde)
C
        END IF
      END DO
C
C     Find the longest face in all frontal boundaries and the
C     shortest in all wake boundaries.
      DMax2 = -1.
      DMin2 = 1.E25
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).EQ.1) THEN
C         Find first and last node in this segment.
          NdeLo = NdxLBnNd(NBnd-1)+1
          NdeHi = NdxLBnNd(NBnd)
          DO NNde = NdeLo,NdeHi
            IF (NNde.EQ.NdeLo) THEN
C             This node is the first in its row. 
              NNdePrv = NdxLBnNd(NvNmBn(NmNghFNd(NBnd)))
              DXHi = XNode(NNdePrv)-XNode(NNde)
              DYHi = YNode(NNdePrv)-YNode(NNde)
              DNorm2 = DXHi**2+DYHi**2
              DMax2 = MAX(DMax2,DNorm2)
C
            ELSE
C             This is an interior face in a single segment.
              DXHi = XNode(NNde-1)-XNode(NNde)
              DYHi = YNode(NNde-1)-YNode(NNde)
              DNorm2 = DXHi**2+DYHi**2
              DMax2 = MAX(DMax2,DNorm2)
C
            END IF
          END DO
C
        ELSE IF (IBndType(NBnd).EQ.4) THEN
C         Find first and last node in this wake segment.
          NdeLo = NdxLBnNd(NBnd-1)+1
          NdeHi = NdxLBnNd(NBnd)
C
          IF (NdeHi-NdeLo.GE.1) THEN
C           There is at least one face in this segment.
C           Loop over all faces in this segment to find the maximum
C           face length.
            DO NNde = NdeLo,NdeHi
              IF (NNde.EQ.NdeHi) THEN
C               This node is the last in its row. The segment is
C               either open or linked to another boundary, possibly
C               itself.
                IF (IBndType(NBnd).EQ.1) THEN
C                 A frontal boundary in a closed loop.
                  NNdeNxt = NdxLBnNd(NvNmBn(NmNghLNd(NBnd))-1)+1
                  DXHi = XNode(NNdeNxt)-XNode(NNde)
                  DYHi = YNode(NNdeNxt)-YNode(NNde)
                  DNorm2 = DXHi**2+DYHi**2
                  DMin2 = MIN(DMin2,DNorm2)
                END IF
C
              ELSE
C               This is an interior face in a single segment.
                DXHi = XNode(NNde+1)-XNode(NNde)
                DYHi = YNode(NNde+1)-YNode(NNde)
                DNorm2 = DXHi**2+DYHi**2
                DMin2 = MIN(DMin2,DNorm2)
C
              END IF
            END DO
          END IF
C
        ELSE IF (IBndType(NBnd).EQ.2) THEN
C         Find first and last node in this non frontal segment.
          NdeLo = NdxLBnNd(NBnd-1)+1
          NdeHi = NdxLBnNd(NBnd)
C
          IF (NdeHi-NdeLo.GE.1) THEN
C           There is at least one face in this segment.
C           Loop over all faces in this segment to find the maximum
C           face length.
            DO NNde = NdeLo,NdeHi
              IF (NNde.EQ.NdeHi) THEN
C               This node is the last in its row. The segment is
C               either open or linked to another boundary, possibly
C               itself.
                IF (IBndType(NBnd).EQ.1) THEN
C                 A frontal boundary in a closed loop.
                  NNdeNxt = NdxLBnNd(NvNmBn(NmNghLNd(NBnd))-1)+1
                  DXHi = XNode(NNdeNxt)-XNode(NNde)
                  DYHi = YNode(NNdeNxt)-YNode(NNde)
                  DNorm2 = DXHi**2+DYHi**2
                  DMaxOut2 = MAX(DMaxOut2,DNorm2)
                END IF
C
              ELSE
C               This is an interior face in a single segment.
                DXHi = XNode(NNde+1)-XNode(NNde)
                DYHi = YNode(NNde+1)-YNode(NNde)
                DNorm2 = DXHi**2+DYHi**2
                DMaxOut2 = MAX(DMaxOut2,DNorm2)
C
              END IF
            END DO
          END IF
C
        END IF         
      END DO
C
C     Find any inviscid length scale.
      IF (DMax2.LE.0..AND.(DMin2.GT.1.E24.OR..NOT.FStretch)) THEN
        WRITE (*,'(/A)') 
     &  ' FATAL: No Frontal boundaries. Sorry, but do try again.'
        IF (FLogFile)
     &  WRITE (NtLog,'(/A)') 
     &  ' FATAL: No Frontal boundaries. Sorry, but do try again.'
        STOP
      ELSE IF (DMax2.LE.0.) THEN
        DMax = SQRT(DMin2)
      ELSE
        DMax = SQRT(DMax2)
      END IF
      DMaxOut = MAX(SQRT(DMaxOut2),DMax)
      IF (IVerbose.GT.3) WRITE (*,103) DMax,DMaxOut
      IF (FLogFile) WRITE (NtLog,103) DMax,DMaxOut
  103 FORMAT (6X,' Maximum frontal and non-frontal scales:',2E15.7)
C
      IF (SpcGrdRt.LT.0) THEN
        IF (IVerbose.GE.3) 
     &  WRITE (*,'(/A/)') ' WARNING: No negative ratios in spacing'
     &                    //' gradients allowed, 1 taken.'
        IF (FLogFile) 
     &  WRITE (NtLog,'(/A/)') ' WARNING: No negative ratios in spacing'
     &                    //' gradients allowed, 1 taken.'
        SpcPw = 1.
C
      ELSE IF (ABS(SpcGrdRt-1.).GT.1.E-3) THEN
C       Find minimum and maximum spacing values to calculate the
C       exponent for the spacing variation.
        SpcMin = BkgSpc(5)
        SpcMax = BkgSpc(5)
        DO NNde = 6,MNde
          SpcMin = MIN(SpcMin,BkgSpc(NNde))
          SpcMax = MAX(SpcMax,BkgSpc(NNde))
        END DO
C       Calculate the exponent.
        SpcPw = 1./(1.-LOG(SpcGrdRt)/LOG(SpcMax/SpcMin))
        IF (IVerbose.GE.3)
     &  WRITE (*,'(6X,A,F5.3)')' Value for SpcPw:',SpcPw
        IF (FLogFile)
     &  WRITE (NtLog,'(6X,A,F5.3)')' Value for SpcPw:',SpcPw
C       Modify the spacing values.
        DO NNde = 5,MNde
          BkgSpc(NNde) = BkgSpc(NNde)**(1./SpcPw)
        END DO
C
      ELSE
C       Use default.
        SpcPw = 1.
C
      END IF
C
C     Calculate an average spacing gradient in the domain.
      BSpcGrad = 0.D0
      BTotVol = 0.D0
C
C     Calculate the surface of the background cells.
      DO NBVvr = 5,MBVvr
C       The three forming nodes of that background cell.
        N1 = NBFrmNde(1,NBVvr)
        N2 = NBFrmNde(2,NBVvr)
        N3 = NBFrmNde(3,NBVvr)
        X(1) = SNGL(XNode(N1))
        X(2) = SNGL(XNode(N2))
        X(3) = SNGL(XNode(N3))
        Y(1) = SNGL(YNode(N1))
        Y(2) = SNGL(YNode(N2))
        Y(3) = SNGL(YNode(N3))
C
C       Cell surface.
        BkgVol(NBVvr) = AREA(3,X,Y)
C
        IF (.NOT.FlsOutVv(NBVvr)) THEN
C         This is an interior cell. Calculate the magnitude of its
C         spacing gradient.
          XGradSpc = (X(2)-X(1))*BkgSpc(N3)+(X(3)-X(2))*BkgSpc(N1)+
     &               (X(1)-X(3))*BkgSpc(N2)
          YGradSpc = (Y(2)-Y(1))*BkgSpc(N3)+(Y(3)-Y(2))*BkgSpc(N1)+
     &               (Y(1)-Y(3))*BkgSpc(N2)
          BSpcGrad = BSpcGrad+.5D0*SQRT(XGradSpc**2+YGradSpc**2)
          BTotVol = BTotVol+BkgVol(NBVvr)
        END IF
C
      END DO
C
      IF ( FNotCon .OR. FAllDisc ) THEN
C       Eliminate forbidden connectivities.
        FIllCon = .TRUE.
        ISweep = 0
        MNewNde = 0
C
C       Normalize the averaged gradient.
        BSpcGrad = BSpcGrad/BTotVol
C
C       Loop as long as nodes have been introduced to disconnect.
C       Make a zero anticonnectivity list for the extra boundary.
        NdxNotCon(MBnd+1) = NdxNotCon(MBnd)
        DO WHILE (FIllCon.AND.MNewNde.LT.MaxNotConNde)
          FIllCon = .FALSE.
          ISweep = ISweep+1
C
C         Loop over all physical background cells until the first 
C         unwanted connection is found. Then do a tree search to
C         walk to the largest illconnecting circumcircle.
          NBVvrLoop = MBVvr+1
          DO WHILE (NBVvrLoop.GT.5.AND.MNewNde.LT.MaxNotConNde)
            NBVvrLoop = NBVvrLoop-1
            NBVvr = NBVvrLoop
            IF (.NOT.FlsOutVv(NBVvr)) THEN
C
C             Find out whether there are illicit connections.
              CALL ILLICIT (NBFrmNde(1,NBVvr),NdxLBnNd,IBndType,
     &                      MBnd,FAllDisc,LsNotCon,
     &                      NdxNotCon,NmNghLNd,NvNmBn,NrBnd,
     &                      FIllCon23,FIllCon31,FIllCon12)
C
              IF (FIllCon12.OR.
     &            FIllCon23.OR.
     &            FIllCon31) THEN
                Found = .TRUE.
C               An unwanted connectivity has been detected. 
C               Start a tree search to find the largest illconnecting
C               circumcircle.
                Finished = .FALSE.
                DO WHILE (.NOT.Finished)
                  Rad = RadVvr(NBVvr)
                  Ngh1 = NghBVvr(1,NBVvr)
                  Ngh2 = NghBVvr(2,NBVvr)
                  Ngh3 = NghBVvr(3,NBVvr)
                  IF (RadVvr(Ngh1).GT.Rad.AND.FIllCon23) THEN
                    NBVvr = Ngh1
                  ELSE IF (RadVvr(Ngh2).GT.Rad.AND.FIllCon31) THEN
                    NBVvr = Ngh2
                  ELSE IF (RadVvr(Ngh3).GT.Rad.AND.FIllCon12) THEN
                    NBVvr = Ngh3
                  ELSE 
                    Finished = .TRUE.
                  END IF
C
                  IF (.NOT.Finished) THEN
                    CALL ILLICIT (NBFrmNde(1,NBVvr),NdxLBnNd,IBndType,
     &                            MBnd,FAllDisc,LsNotCon,
     &                            NdxNotCon,NmNghLNd,NvNmBn,NrBnd,
     &                            FIllCon23,FIllCon31,FIllCon12)
                  END IF
C
                END DO
C
C               Introduce a new node at the Voronoi vertex and
C               estimate a spacing for it.
                NBVvrStart = NBVvr
                FIllCon = .TRUE.
                MNewNde = MNewNde+1
                NNde = MNde+MNewNde
                IF (MNewNde.LT.MaxNotConNde) THEN
                  XNode(NNde) = XVvr(NBVvr)
                  YNode(NNde) = YVvr(NBVvr)
C
C                 Find the minimum spacing of all three nodes.
                  NB1 = NBFrmNde(1,NBVvr)
                  NB2 = NBFrmNde(2,NBVvr)
                  NB3 = NBFrmNde(3,NBVvr)
                  BSpcMin = MIN(BkgSpc(NB1),BkgSpc(NB2),BkgSpc(NB3))
C                 Extrapolate spacing for NNde from this closest node.
                  BSpcNew = BSpcMin+.5*BSpcGrad*SQRT(RadVvr(NBVvr))
C                 Find the current background spacing at the new node.
                  BSpcOld = BSPCINT(XNode(NNde),YNode(NNde),
     &                              NBVvr,FOutCnv)
C                 Use the larger of the two.
                  BkgSpc(NNde) = MAX(BSpcNew,BSpcOld)
C
C                 Introduce the new node into the background mesh.
C                 Find the background Voronoi vertices to be deleted.
                  NNde = MNde+MNewNde
                  PBVvr = NBVvr
                  PBVvr = IWALK (XNode(NNde),YNode(NNde),PBVvr,
     &                           XNode,YNode,NBFrmNde,NghBVvr,
     &                           MBVvr,FOutCnv)
C
C                 Find the constrained cavity.
                  CALL FLGVVRCN (NNde,PBVvr,RadTolMi,MBVvr,XVvr,YVvr,
     &                           NBFrmNde,NghBVvr,RadVvr,FlsOutVv,
     &                           LsFlgVvr,FlsFlgVv,MFlgVvr,FOut)
                  IF (FOut) THEN
C                   This node could not be introduced. Erase it
                    MNewNde = MNewNde-1
C
                  ELSE
C                   Connect the new node with the valid structure.
                    CALL CNCTNDE (NNde,MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                            RadVvr,LsFlgVvr,FlsFlgVv,MNwVvr)
C
C                   Calculate the volumes of the newly formed triangles.
                    DO NBVv = MBVvr+1,MBVvr+MNwVvr
C                     The three forming nodes of that background cell.
                      N1 = NBFrmNde(1,NBVv)
                      N2 = NBFrmNde(2,NBVv)
                      N3 = NBFrmNde(3,NBVv)
                      X(1) = SNGL(XNode(N1))
                      X(2) = SNGL(XNode(N2))
                      X(3) = SNGL(XNode(N3))
                      Y(1) = SNGL(YNode(N1))
                      Y(2) = SNGL(YNode(N2))
                      Y(3) = SNGL(YNode(N3))
C                     Cell surface.
                      BkgVol(NBVv) = AREA(3,X,Y)
                    END DO
C                   Copy the new values over the ones of disconnected
C                   triangles. 
                    DO IBVvr = 1,MFlgVvr
                      NBVDel = LsFlgVvr(IBVvr)
                      NBVNew = MBVvr+MNwVvr-IBVvr+1
                      BkgVol(NBVDel) = BkgVol(NBVNew)
                    END DO
C
C                   Reorder data structure, delete obsolete elements.
                    CALL PSTPRC (MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                           RadVvr,LsFlgVvr,MFlgVvr,FlsFlgVv,
     &                           MNwVvr)
                  END IF
C
                END IF
C
              END IF
C
            END IF
          END DO
C
        END DO
C
        IF (FIllCon) THEN
C         Insertion of background nodes has exceeded the threshold of
C         maximum number of nodes to be introudced.
          IF (IVerbose.GE.1) THEN
            WRITE (*,'(/A,I5,A/A/)') 'WARNING: After introducing',
     &      MaxNotConNde,' nodes to break unwanted connectivities',
     &      '         they still haven''t been eliminated.'
          END IF 
          IF (FLogFile) THEN
            WRITE (NtLog,'(/A,I5,A/A/)') 'WARNING: After introducing',
     &      MaxNotConNde,' nodes to break unwanted connectivities',
     &      '         they still haven''t been eliminated.'
          END IF 
        END IF
C
        IF (MNewNde.GT.0) THEN
C         Form a new boundary containing all nodes introduced.
          MBnd = MBnd+1
          MNde = MNde+MNewNde
          NdxLBnNd(MBnd) = MNde
          NdxLArc(MBnd) = NdxLArc(MBnd-1)
          IBndType(MBnd) = 10
          NmNghFNd(MBnd) = MaxBound+1
          NmNghLNd(MBnd) = MaxBound+1
          NvNmBn(MaxBound+1) = MBnd
          FIllBnd = .TRUE.
          IF (IVerbose.GT.4) THEN
            WRITE (*,99) MNewNde,MNde-MNewNde,MNde,ISweep
          ELSE IF (IVerbose.GT.2) THEN
            WRITE (*,98) MNewNde
          END IF
          IF (FLogFile) THEN
            WRITE (NtLog,99) MNewNde,MNde-MNewNde,MNde,ISweep
          END IF
   98     FORMAT (6X,I7,' nodes in the background grid to break',
     &            ' unwanted connectivity.')
   99     FORMAT (6X,I7,' nodes, numbers',I7,' through',I7/
     &            6X,' in the background grid to break',
     &            ' unwanted connectivity in',I7,' sweeps.')
C
        ELSE
C         Form an empty boundary.
          MBnd = MBnd+1
          MNde = MNde+MNewNde
          NdxLBnNd(MBnd) = NdxLBnNd(MBnd-1)
          NdxLArc(MBnd) = NdxLArc(MBnd-1)
          IBndType(MBnd) = 10
          NmNghFNd(MBnd) = MaxBound+1
          NmNghLNd(MBnd) = MaxBound+1
          NvNmBn(MaxBound+1) = MBnd
          FIllBnd = .FALSE.
C
        END IF
C
      END IF
C
      IF (FStretch) THEN
C       Introduce nodes around each surface to define a stretching
C       layer.
C
C       Calculate the stretching in the domain.
        DomStr = 1.
C
C       Calculate the stretching power.
        CALL SUPCALC (DltStar,BndStr,DomStr,DMax,StrPw,IVerbose,
     &                NtLog,FLogFile)
        StrPwInv = 1./StrPw
C
C       Initialize the stretching at vertices of the 
C       background grid. Loop over all boundaries.
        DO NBnd =1,MBnd
          IF (IBndType(NBnd).EQ.1) THEN
C           This is a frontal boundary. Stretch it.
            DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
              BkgStr(NNde) = BndStr
            END DO
C
          ELSE IF (IBndType(NBnd).EQ.4) THEN
C           This is a frontal wake boundary. Stretch it.
C           Use a supercircle interpolation of the stretching
C           value depending on the distance between the nodes.
            DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
              BkgStr(NNde) = BndStr
            END DO
C
          ELSE
C           This is a non-frontal, no stretching boundary.
            DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
              BkgStr(NNde) = DomStr
            END DO
c
          END IF
        END DO
C
C       Reset counters.
        MNewNde = 0
C
C       Loop over all triangles in the foreground grid to find a
C       set of nodes that describe the viscous layer. At this stage
C       nodes along concave boundary segments are not permitted.
        FDoIntr = .FALSE.
        DO NV = 5,MVvr
          IF (.NOT.FlsOutVv(NV)) THEN
            PCellIni = NV
C           Calculate the radius and location of the circumcircle,
C           as the Rad/X/YVvr now pertain to the background grid and
C           the current location is tested on the foreground.
            CALL CALCVVR (NFrmNde(1,NV),XVvrNV,YVvrNV,RadVvrNV,FLinDeg)
            CALL FNDVSCND (NFrmNde(1,NV),RadVvrNV,XVvrNV,YVvrNV,
     &                     PCellIni,FDoIntr,
     &                     XNode,YNode,NdxLBnNd,MBnd,IBndType,
     &                     NvNmBn,NmNghLNd,MNde,MNewNde,
     &                     BTol,DltStar,BndSTr,DomStr,DMax,
     &                     BkgStr,BkgSpc,NdePrio,IFront)
          END IF
        END DO
C
        IF (MNewNde.GT.0) THEN
C         Form a new boundary containing all nodes introduced.
          MBnd = MBnd+1
          MNde = MNde+MNewNde
          NdxLBnNd(MBnd) = MNde
          NdxLArc(MBnd) = NdxLArc(MBnd-1)
          IBndType(MBnd) = 10
          NmNghFNd(MBnd) = MaxBound+2
          NmNghLNd(MBnd) = MaxBound+2
          NvNmBn(MaxBound+2) = MBnd
C
          IF (IVerbose.GT.4) THEN
            WRITE (*,101) MNewNde,MNde-MNewNde,MNde
          ELSE IF (IVerbose.GT.2) THEN
            WRITE (*,100) MNewNde
          END IF
          IF (FLogFile) THEN
            WRITE (NtLog,101) MNewNde,MNde-MNewNde,MNde
          END IF
  100     FORMAT (6X,I7,' nodes in the background grid to form',
     &            ' a stretching layer.')
  101     FORMAT (6X,I7,' nodes, numbers',I7,' through',I7/
     &            6X,' in the background grid to form',
     &            ' a stretching layer.')
C
          IF (MNde.GE.MaxBndNde) THEN
C           Insertion of background nodes has exceeded the threshold of
C           maximum number of nodes in the background.
            IF (IVerbose.GE.1) 
     &        WRITE (*,'(/A,I5,A/A,I5,A/)') 'FATAL:',MNde,
     &               ' nodes needed in the background grid,',
     &               '       but only,',MaxBndNde,' allocated.'
            IF (FLogFile)
     &        WRITE (NtLog,'(/A,I5,A/A,I5,A/)') 'FATAL:',MNde,
     &               ' nodes needed in the background grid,',
     &               '       but only,',MaxBndNde,' allocated.'
            STOP
          END IF
C
C         Save the triangulation of the boundaries as a background mesh
C         and start a new background triangulation with the stretching
C         nodes. Keep the spacing information from the previous
C         background grid and apply the interpolated stretching
C         information to the nodes for spacing manipulation afterwards.
          MBVvr = MVvr
          DO NVvr = 1,MVvr
            NghBVvr(1,NVvr) = NghVvr(1,NVvr)
            NghBVvr(2,NVvr) = NghVvr(2,NVvr)
            NghBVvr(3,NVvr) = NghVvr(3,NVvr)
            NBFrmNde(1,NVvr) = NFrmNde(1,NVvr)
            NBFrmNde(2,NVvr) = NFrmNde(2,NVvr)
            NBFrmNde(3,NVvr) = NFrmNde(3,NVvr)
            CALL CALCVVR (NFrmNde(1,NVvr),
     &                    XVvr(NVvr),YVvr(NVvr),RadVvr(NVvr),FLinDeg)
          END DO
C
C         Introduce type 9 boundaries into the background mesh.
          DO NBnd = 1,MBnd
            IF (IBndType(NBnd).EQ.9) THEN
              DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
C
C               Find the background Voronoi vertices to be deleted.
                PBVvr = MBVvr
                PBVvr = IWALK (XNode(NNde),YNode(NNde),PBVvr,
     &                         XNode,YNode,NBFrmNde,NghBVvr,
     &                         MBVvr,FOutCnv)
C
C               Find the constrained cavity.
                CALL FLGVVRCN (NNde,PBVvr,RadTolMi,MBVvr,XVvr,YVvr,
     &                         NBFrmNde,NghBVvr,RadVvr,FlsOutVv,
     &                         LsFlgVvr,FlsFlgVv,MFlgVvr,FOut)
C
C               Connect the new node with the valid structure.
                CALL CNCTNDE (NNde,MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                        RadVvr,LsFlgVvr,FlsFlgVv,MNwVvr)
C
C               Reordering the data structure, delete obsolete elements.
                CALL PSTPRC (MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                       RadVvr,LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
              END DO
            END IF
          END DO
C
C         Introduce the stretching boundary into the background mesh.
          NBnd = NvNmBn(MaxBound+2)
          NNde = NdxLBnNd(NBnd-1)
          DO WHILE (NNde.LT.NdxLBnNd(NBnd))
            NNde = NNde+1
C
C           Find the background Voronoi vertices to be deleted.
            PBVvr = MBVvr
            PBVvr = IWALK (XNode(NNde),YNode(NNde),PBVvr,
     &                     XNode,YNode,NBFrmNde,NghBVvr,MBVvr,
     &                     FOutCnv)
C
C           Find the constrained cavity.
            CALL FLGVVRCN (NNde,PBVvr,RadTolMi,MBVvr,XVvr,YVvr,
     &                     NBFrmNde,NghBVvr,RadVvr,FlsOutVv,LsFlgVvr,
     &                     FlsFlgVv,MFlgVvr,FOut)
C
            IF (FOut) THEN
C             This node could not be introduced. Take it out.
              NCpNde = NdxLBnNd(NBnd)
              XNode(NNde) = XNode(NCpNde)
              YNode(NNde) = YNode(NCpNde)
              BkgStr(NNde) = BkgStr(NCpNde)
              BkgSpc(NNde) = BkgSpc(NCpNde)
              NNde = NNde-1
              NdxLBnNd(NBnd) = NdxLBnNd(NBnd)-1
              MNde = MNde-1
C
            ELSE
C             Connect the new node with the valid structure.
              CALL CNCTNDE (NNde,MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                      RadVvr,LsFlgVvr,FlsFlgVv,MNwVvr)
C
C             Reordering the data structure, delete obsolete elements.
              CALL PSTPRC (MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                     RadVvr,LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
            END IF
C
          END DO
C
        END IF
C
C       Recalculate the surface of the background cells.
        DO NBVvr = 5,MBVvr
C         The three forming nodes of that background cell.
          N1 = NBFrmNde(1,NBVvr)
          N2 = NBFrmNde(2,NBVvr)
          N3 = NBFrmNde(3,NBVvr)
          X(1) = SNGL(XNode(N1))
          X(2) = SNGL(XNode(N2))
          X(3) = SNGL(XNode(N3))
          Y(1) = SNGL(YNode(N1))
          Y(2) = SNGL(YNode(N2))
          Y(3) = SNGL(YNode(N3))
C
C         Cell surface.
          BkgVol(NBVvr) = AREA(3,X,Y)
C
C         Flag the cell if it prescribes stretching.
          IF (BkgStr(N1).GT.1..OR.BkgStr(N2).GT.1..OR.
     &        BkgStr(N3).GT.1.) THEN
            FBkgStr(NBVvr) = .TRUE.
          ELSE
            FBkgStr(NBVvr) = .FALSE.
          END IF
        END DO
C
C       Loop over all boundaries to assign a IFront value to them.
C       For connected segments formed from different NBnd take the
C       value NBnd of the first boundary segment in the list.
        DO NBnd = 1,MBnd
          IFront(NBnd) = 0
        END DO
C
C       Assign values to regular closed loop boundaries first.
        DO NBnd = 1,MBnd
          IF (IFront(NBnd).EQ.0.AND.IBndType(NBnd).LT.3) THEN
C           This segment has no IFront yet.
            FOpen = .TRUE.
            NSgmnt = NBnd
C           Loop over all segments connected to NBnd and assign NBnd.
            MBndChk = 1
            DO WHILE (FOpen.AND.MBndChk.LE.MBnd)
              MBndChk = MBndChk+1
              NxtBnd = NvNmBn(NmNghLNd(NSgmnt))
              IFront(NxtBnd) = NBnd
              NSgmnt = NxtBnd
              IF (NxtBnd.EQ.NBnd) FOpen = .FALSE.
            END DO
            IF (MBndChk.GT.MBnd) THEN
              WRITE (*,'(A,I4,A,I4)') 
     &        ' FATAL: No closed loop on regular boundary',NBnd,
     &        ' named',NmeBnd(NBnd)
              IF (FLogFile)
     &        WRITE (*,'(A,I4,A,I4)') 
     &        ' FATAL: No closed loop on regular boundary',NBnd,
     &        ' named',NmeBnd(NBnd)
              STOP
            END IF
          END IF
        END DO
C
C       Any other boundaries impinging on regular ones inherit the
C       regular IFront. Others get their own. Currently, only 
C       single Type 4 segments that are at most connected to one
C       regular boundary are allowed.
        DO NBnd = 1,MBnd
          IF (IFront(NBnd).EQ.0.AND.IBndType(NBnd).GE.3) THEN
C           This segment has no IFront yet.
            FOpen = .TRUE.
            NSgmnt = NBnd
C           Find the upper end.
            IF (NmNghLNd(NBnd).NE.0) THEN
              IFront(NBnd) = IFront(NvNmBn(NmNghLNd(NBnd)))
            ELSE IF (NmNghFNd(NBnd).NE.0) THEN
              IFront(NBnd) = IFront(NvNmBn(NmNghFNd(NBnd)))
            ELSE
              IFront(NBnd) = NBnd
            END IF
          END IF
        END DO
C
C       Loop over all cells to define areas assigned to certain 
C       boundaries and to ensure that the enclosing viscous boundary
C       is not open.
        DO NBVvr = 5,MBVvr
          IArea(NBVvr) = -1
        END DO
        FoundGap = .TRUE.
        NSweep = 0
        MSweep = MNde
        DO WHILE (FoundGap.AND.NSweep.LT.MSweep)


c          write (*,*) ' sweep',nsweep+1
c          ntopfl = 99
c          open (ntopfl,file='379.dpl',form='formatted')
c          CALL HULDPL (MBVvr,NBFrmNde,NghBVvr,BkgSpc,BkgStr,FBkgStr,
c     &                 RadVvr,FBkgrd,NtOpFl)


          NSweep = NSweep+1
          FoundGap = .FALSE.
          NBVvr = 4
          FNoArea = .FALSE.
          DO WHILE (NBVvr.LT.MBVvr.AND..NOT.FNoArea)
            NBVvr = NBVvr+1
C           Check whether this triangle is permissible and find the
C           associated element.
            CALL FNDIAREA (FlsOutVv(NBVvr),NBFrmNde(1,NBVvr),NdxLBnNd,
     &                     IBndType,IFront,IArea(NBVvr),FNoArea)
          END DO
C
          IF (FNoArea) THEN
C           This is a triangle that connects different boundaries.
C           Eliminate by introducing a node. Use BTol = BAnarchy = 0.
            FoundGap = .TRUE.
            MNewNde = 0
            BAnarchy = 0.
            PCellIni = NBVvr
            FDoIntr = .TRUE.
            CALL FNDVSCND (NBFrmNde(1,NBVvr),RadVvr(NBVvr),
     &                     XVvr(NBVvr),YVvr(NBVvr),PCellIni,FDoIntr,
     &                     XNode,YNode,NdxLBnNd,MBnd,IBndType,
     &                     NvNmBn,NmNghLNd,MNde,MNewNde,
     &                     BAnarchy,DltStar,BndSTr,DomStr,DMax,
     &                     BkgStr,BkgSpc,NdePrio,IFront)
C
            IF (MNewNde.GT.0) THEN
C             Try to introduce this node.
              MNde = MNde+1
              NdxLBnNd(MBnd) = MNde
C
C             Find spacing and stretching magnitude.
              PBCell = NBVvr
              BSpc = BSPCINT(XNode(MNde),YNode(MNde),PBCell,FOutCnv)
              DR = SQRT(RadVvr(NBVvr))
              BkgStrI = BndStr-DR/DltStar*(BndStr-DomStr)
              BkgStr(MNde) = MAX(DomStr,BkgStrI)
c              BkgSpc(MNde) = MIN(DMax/BkgStr(MNde),BSpc)
              BkgSpc(MNde) = BSpc
C
C             Find the background Voronoi vertices to be deleted.
              PBVvr = NBVvr
              PBVvr = IWALK (XNode(MNde),YNode(MNde),PBVvr,
     &                       XNode,YNode,NBFrmNde,NghBVvr,MBVvr,
     &                       FOutCnv)
C             Find the constrained cavity.
              CALL FLGVVRCN (MNde,PBVvr,RadTolMi,MBVvr,XVvr,YVvr,
     &                       NBFrmNde,NghBVvr,RadVvr,FlsOutVv,
     &                       LsFlgVvr,FlsFlgVv,MFlgVvr,FOut)
C
C             Connect the new node with the valid structure.
              CALL CNCTNDE (MNde,MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                      RadVvr,LsFlgVvr,FlsFlgVv,MNwVvr)
C  
C             Calculate the volumes of the newly formed triangles.
              DO NBVv = MBVvr+1,MBVvr+MNwVvr
C               The three forming nodes of that background cell.
                N1 = NBFrmNde(1,NBVv)
                N2 = NBFrmNde(2,NBVv)
                N3 = NBFrmNde(3,NBVv)
                X(1) = SNGL(XNode(N1))
                X(2) = SNGL(XNode(N2))
                X(3) = SNGL(XNode(N3))
                Y(1) = SNGL(YNode(N1))
                Y(2) = SNGL(YNode(N2))
                Y(3) = SNGL(YNode(N3))
C               Cell surface.
                BkgVol(NBVv) = AREA(3,X,Y)
C               Flag the cell if it prescribes stretching.
                IF (BkgStr(N1).GT.1..OR.BkgStr(N2).GT.1..OR.
     &              BkgStr(N3).GT.1.) THEN
                  FBkgStr(NBVv) = .TRUE.
                  IArea(NBVv) = -1
                ELSE
                  FBkgStr(NBVv) = .FALSE.
                  IArea(NBVv) = 0
                END IF
              END DO
C             Copy the new values over the ones of disconnected
C             triangles. Erase IArea.
              DO IBVvr = 1,MFlgVvr
                NBVDel = LsFlgVvr(IBVvr)
                NBVNew = MBVvr+MNwVvr-IBVvr+1
                BkgVol(NBVDel) = BkgVol(NBVNew)
                FBkgStr(NBVDel) = FBkgStr(NBVNew)
                IArea(NBVDel) = IArea(NBVNew)
              END DO
C
C             Reordering the data structure, delete obsolete elements.
              CALL PSTPRC (MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                     RadVvr,LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
            END IF
          END IF
C
        END DO
C
C       Loop over all background triangles to assign IArea to
C       triangles in the stretching layer that are not connected to
C       the frontal surfaces and have IArea=0 so far.
        DO NBVvr = 5,MBVvr
          IF (FBkgStr(NBVvr).AND.IArea(NBVvr).EQ.0.AND.
     &        .NOT.FlsOutVv(NBVvr)) THEN
            CALL FIXIAREA(NBVvr,NBFrmNde,NghBVvr,MBVvr,BkgStr,IArea,
     &                    XNode,YNode,IVerbose,NtLog,FLogFile)
          END IF
        END DO
C
C       Reintroduce the nodes to prevent illconnectivity into the
C       background mesh.
        NBnd = NvNmBn(MaxBound+1)
        NNde = NdxLBnNd(NBnd-1)
        DO WHILE (NNde.LT.NdxLBnNd(NBnd))
          NNde = NNde+1
C
C         Find the background Voronoi vertices to be deleted.
          PBVvr = MBVvr
          PBVvr = IWALK (XNode(NNde),YNode(NNde),PBVvr,
     &                   XNode,YNode,NBFrmNde,NghBVvr,MBVvr,
     &                   FOutCnv)
C
C         Find the constrained cavity. As additional constraint
C         to being in the domain, already guaranteed, a constraint
C         of being outside any viscous region is imposed.
          CALL FLGVVRCN (NNde,PBVvr,RadTolMi,MBVvr,XVvr,YVvr,
     &                   NBFrmNde,NghBVvr,RadVvr,FBkgStr,LsFlgVvr,
     &                   FlsFlgVv,MFlgVvr,FOut)
C
          IF (FOut) THEN
C           This node could not be introduced. Take it out.
            DO NCpNde = NNde+1,MNde
              XNode(NCpNde-1) = XNode(NCpNde)
              YNode(NCpNde-1) = YNode(NCpNde)
              BkgStr(NCpNde-1) = BkgStr(NCpNde)
              BkgSpc(NCpNde-1) = BkgSpc(NCpNde)
            END DO
            DO NBVvr = 5,MBVvr
              DO KNde = 1,3
                IF (NBFrmNde(KNde,NBVvr).GT.NNde) 
     &            NBFrmNde(KNde,NBVvr) = NBFrmNde(KNde,NBVvr)-1
              END DO
            END DO
            DO NCpBnd = NBnd,MBnd
              NdxLBnNd(NCpBnd) = NdxLBnNd(NCpBnd)-1
            END DO
            MNde = MNde-1
            NNde = NNde-1
C
          ELSE
C           Connect the new node with the valid structure.
            CALL CNCTNDE (NNde,MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                    RadVvr,LsFlgVvr,FlsFlgVv,MNwVvr)
C
C           Calculate the volumes of the newly formed triangles.
            DO NBVv = MBVvr+1,MBVvr+MNwVvr
C             The three forming nodes of that background cell.
              N1 = NBFrmNde(1,NBVv)
              N2 = NBFrmNde(2,NBVv)
              N3 = NBFrmNde(3,NBVv)
              X(1) = SNGL(XNode(N1))
              X(2) = SNGL(XNode(N2))
              X(3) = SNGL(XNode(N3))
              Y(1) = SNGL(YNode(N1))
              Y(2) = SNGL(YNode(N2))
              Y(3) = SNGL(YNode(N3))
C             Cell surface.
              BkgVol(NBVv) = AREA(3,X,Y)
              FBkgStr(NBVv) = .FALSE.
            END DO
C           Copy the new values over the ones of disconnected
C           triangles. 
            DO IBVvr = 1,MFlgVvr
              NBVDel = LsFlgVvr(IBVvr)
              NBVNew = MBVvr+MNwVvr-IBVvr+1
              BkgVol(NBVDel) = BkgVol(NBVNew)
              FBkgStr(NBVDel) = .FALSE.
            END DO
C
C           Reordering the data structure, delete obsolete elements.
            CALL PSTPRC (MBVvr,XVvr,YVvr,NBFrmNde,NghBVvr,
     &                   RadVvr,LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
          END IF
C
        END DO
C
C       Adjust BkgStr to accomodate a thin layer of constant
C       stretching.
        DO NBnd = 1,MBnd
          IF (NdxLArc(NBnd).GT.NdxLArc(NBnd-1)) THEN
C           This boundary segment has some interpolation stations.
C
C           Calculate the arc-length of the segment.
            AllArc = 0.
            N2 = NdxLBnNd(NvNmBn(NmNghFNd(NBnd)))
            X(2) = XNode(N2)
            Y(2) = YNode(N2)
            DO N2 = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
              X(1) = X(2)
              Y(1) = Y(2)
              X(2) = XNode(N2)
              Y(2) = YNode(N2)
              Dist = SQRT((X(1)-X(2))**2+(Y(1)-Y(2))**2)
              AllArc = AllArc+Dist
            END DO
C
C           Loop over all the nodes of this segment to change BkgStr.
            Arc = 0.
            NNde = NdxLBnNd(NvNmBn(NmNghFNd(NBnd)))
            Interval = 1
            ArcILo = ArcRel(NdxLArc(NBnd-1)+Interval)
            ArcIHi = ArcRel(NdxLArc(NBnd-1)+1+Interval)
            DArc = ArcIHi-ArcILo
            DltZeroLo = DltZero(NdxLArc(NBnd-1)+Interval)
            DltZeroHi = DltZero(NdxLArc(NBnd-1)+1+Interval)
            DDltZ = DltZeroHi-DltZeroLo
            X(2) = XNode(NNde)
            Y(2) = YNode(NNde)
            DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
C
C             Calculate the local fraction of the arclength of this 
C             node.
              X(1) = X(2)
              Y(1) = Y(2)
              X(2) = XNode(NNde)
              Y(2) = YNode(NNde)
              Dist = SQRT((X(1)-X(2))**2+(Y(1)-Y(2))**2)
              Arc = Arc+Dist
              ArcFrac = Arc/AllArc
C
C             Find the two interpolation stations that surround this
C             node.
              IF (ArcIHi.LT.ArcFrac) THEN
C               Move to the next higher interval.
                Interval = Interval+1
                ArcILo = ArcIHi
                DltZeroLo = DltZeroHi
                ArcIHi = ArcRel(NdxLArc(NBnd-1)+1+Interval)
                DltZeroHi = DltZero(NdxLArc(NBnd-1)+1+Interval)
                DArc = ArcIHi-ArcILo
                DDltZ = DltZeroHi-DltZeroLo
              END IF
C
C             Interpolate for NNde.
              DltZeroNNde = DltZeroLo+DDltZ*(ArcFrac-ArcILo)/DArc
C             Extrapolate.
              GradS = (BndStr-1.)/DltStar
              DBkgStr = 1./(1./DltZeroNNde/GradS-1./(BndStr-1.))
C             Limit the 'lift' of BkgStr at the node to something
C             between 0. and twice the linear gradient.
              DBkgStrMax = 2.*BndStr/DltStar*DltZeroNNde
              DBkgStr = MIN(MAX(0.,DBkgStr),DBkgStrMax)
              BkgStr(NNde) = BkgStr(NNde)+DBkgStr
C
            END DO
C
          END IF
        END DO
C
C       Adjust BkgStr to taper out wakes.
        DO NBnd = 1,MBnd
          IF (IBndType(NBnd).EQ.4) THEN
C           This is a wake. Set the stretching such that we
C           have isotropy at the outer edge of the layer considering
C           the spacing of the nodesa on the wake.
C           First find the stretching value at the point the
C           wake is connected to.
            IF (NvNmBn(NmNghFNd(NBnd)).EQ.NBnd.OR.
     &          NmNghFNd(NBnd).EQ.0) THEN
C             This boundary is not connected to a solid surface.
              BStrMax = BndStr
C
            ELSE
C             Pointer to the solid boundary from the wake.
              NxtBnd1 = NvNmBn(NmNghFNd(NBnd))
C             Pointer from this boundary to its previous segment
C             that contains the triply connecting node at its end.
              NxtBnd2 = NvNmBn(NmNghFNd(NBnd))
              NdeTE = NdxLBnNd(NxtBnd2)
              BStrMax = BkgStr(NdeTE)
C
            END IF
C
C           Find the largest spacing along this wake.
            DMaxWake = BkgSpc(NdxLBnNd(NBnd-1)+1)
            DO NNde = NdxLBnNd(NBnd-1)+2,NdxLBnNd(NBnd)
              DMaxWake = MAX(DMaxWake,BkgSpc(NNde))
            END DO
C
            IF (DMaxWake.GT.DMax) THEN
C             Try to taper.
              DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
                BSpc = MAX(DMax,BkgSpc(NNde))
                StrLoc = 1.+(BStrMax-1.)*(BSpc-DMaxWake)/(DMax-DMaxWake)
                StrFrac = (BSpc-DMaxWake)/(DMax-DMaxWake)
                BkgStr(NNde) = 1.+(BStrMax-1.)*StrFrac
c                DltFrac = 1.-(1.-StrFrac**StrPw)**StrPwInv
c                BkgStr(NNde) = 1.+(BStrMax-1.)*DltFrac
              END DO
            END IF
C
          END IF
        END DO
C
C       Erase IFront and FBkgStr for all cells along the wake that
C       now specify isotropy to prevent division by zero in BUILDNDE.
        DO NBVvr = 5,MBVvr
          IF (FBkgStr(NBVvr)) THEN
            B1 = BkgStr(NBFrmNde(1,NBVvr))
            B2 = BkgStr(NBFrmNde(2,NBVvr))
            B3 = BkgStr(NBFrmNde(3,NBVvr))
            IF (B1.EQ.1.AND.B2.EQ.1.AND.B3.EQ.1) THEN
              FBkgStr(NBVvr) = .FALSE.
              IArea(NBVvr) = 0
            END IF
          END IF
        END DO
C
      END IF
C
      IF (IVerbose.GT.3.OR.FLogFile) THEN
C       Calculate the average gradient of background spacing.
        GradMax = -1.E20
        BSpcGrad = 0.
        BTotVol = 0.
        DO NBVvr = 5,MBVvr
          IF (.NOT.FLsOutVv(NBVvr)) THEN
C           This is an interior cell. Calculate the magnitude of its
C           spacing gradient.
            N1 = NBFrmNde(1,NBVvr)
            N2 = NBFrmNde(2,NBVvr)
            N3 = NBFrmNde(3,NBVvr)
            X(1) = SNGL(XNode(N1))
            X(2) = SNGL(XNode(N2))
            X(3) = SNGL(XNode(N3))
            Y(1) = SNGL(YNode(N1))
            Y(2) = SNGL(YNode(N2))
            Y(3) = SNGL(YNode(N3))
            XGradSpc = (X(2)-X(1))*BkgSpc(N3)+(X(3)-X(2))*BkgSpc(N1)+
     &               (X(1)-X(3))*BkgSpc(N2)
            YGradSpc = (Y(2)-Y(1))*BkgSpc(N3)+(Y(3)-Y(2))*BkgSpc(N1)+
     &               (Y(1)-Y(3))*BkgSpc(N2)
            BSurf = AREA(3,X,Y)
            GradH = SQRT(XGradSpc**2+YGradSpc**2)/2./BSurf
            BSpcGrad = BSpcGrad+.5D0*SQRT(XGradSpc**2+YGradSpc**2)
            BTotVol = BTotVol+BkgVol(NBVvr)
            IF (GradMax.LT.GradH) THEN
              GradMax = GradH
              NGradMax = NBVvr
            END IF
          END IF
        END DO
C
C       Normalize the averaged gradient.
        BSpcGrad = BSpcGrad/BTotVol
C
        IF (IVerbose.GT.3) WRITE (*,102) GradMax,BSpcGrad,NGradMax
        IF (FLogFile) WRITE (NtLog,102) GradMax,BSpcGrad,NGradMax
  102   FORMAT (6X,' Maximum(Average) Gradient in the background grid:',
     &          F8.5,'(',F8.5,')'/6X,' in cell',I7,'.')
      END IF


C     Recalculate the Voronoi tesselation of the foreground.
      DO NVvr = 5,MVvr
        CALL CALCVVR (NFrmNde(1,NVvr),XVvr(NVvr),YVvr(NVvr),
     &                RadVvr(NVvr),FLinDeg)
      END DO
C     Reset the Vvr outside the convex hull.
      DO NVvr = 1,4
        XVvr(NVvr) = XNode(NVvr)
        YVvr(NVvr) = YNode(NVvr)
      END DO
C     
      RETURN
      END
