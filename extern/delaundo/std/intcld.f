      SUBROUTINE INTCLD (FlsOutVv,LsFlgVvr,FlsFlgVv,IVSmooth,
     &                   FAskRow,MIsoRow,IVerbose,NtLog,FLogFile,
     &                   FStretch,
     &                   DTol,DTol2,QTol2,QTolRtHi,QTolRtLo,RadTolMi,
     &                   SpcNew,FlsGoodVv,NFVvr,NBVvr,LsNgh,LsNde,
     &                   NdeFrnt,MaxFrontEl,
     &                   MNde,XNode,YNode,
     &                   MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &                   MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &                   NvNmBn,IBndType,MLmt,LmtNde,
     &                   MBVvr,NBFrmNde,NghBVvr,
     &                   BkgSpc,BkgVol,DMax,SpcPw,
     &                   BkgStr,BndStr,DomStr,StrPw,StrPwInv,FBkgStr,
     &                   NLevel,ILevel,MVvrPrvLv,MaxLsFlg)
C
C Last update:
C 20May96;new SPCOLCHK with LsFlgVvr.
C 9May94,16:45; go to MG.
C 14Nov92,4:59;intro IVSmooth.
C 24Sep92,19:24;use properly QTol for sidelength comparisons.
C 29Jun92,15:38;clear triangles with FlsGoodVv if refinement was tried.
C 29Jun92,13:40;initialize FlsGoodVv for newly built triangles.
C 24Jun92,18:20;intro FlsGoodVv, use FLGVVRCN.
C     17Jul91, 11:02. (Front tracking with IBndType and FlsOutVv.)
C     5Jul91, 19:36. (Eliminate NghNde.)
C     3Jul91, 15:10. (pass grid as argument to introduction routines.)
C
C Introduce interior nodes after the boundaries have been
C triangulated. In case of coarser MG levels, warp the coarse grid
C nodes on the vertices of the finer mesh. Welcome to DOMINO software.
C
C Input:
C LsFlgVvr:
C FlsFlgVv:
C IVSmooth: Indicator how to treat the outermost viscous cells.
C FAskRow:
C MIsoRow:
C IVerbose:
C NtLog:
C FLogFile:
C FStretch:
C DTol:
C DTol2:
C QTol2:
C QTolRtHi:
C QTolRtLo:
C RadTolMi:
C SpcNew:
C FlsGoodVv:
C NFVvr:
C NBVvr:
C LsNgh:
C LsNde:
C NdeFrnt:
C MaxFrontEl:
C MNde:
C XNode:
C YNode:
C MVvr:
C NFrmNde:
C NghVvr:
C XVvr:
C YVvr:
C RadVvr:
C MBnd:
C NdxLBnNd:
C NmNghFNd:
C NmNghLNd:
C NvNmBn:
C IBndType:
C MLmt:
C LmtNde:
C MBVvr:
C NBFrmNde:
C NghBVvr:
C BkgSpc:
C BkgVol:
C DMax:
C SpcPw:
C BkgStr:
C BndStr:
C DomStr:
C StrPw:
C StrPwInv:
C FBkgStr:
C NLevel:
C ILevel:
C MVvrPrvLvC
C MaxLsFlg: Dimension of LsFlgVvr...
C
C Output:
C LsFlgVvr:
C FlsFlgVv:
C MIsoRow:
C SpcNew:
C FlsGoodVv:
C NFVvr:
C NBVvr:
C LsNgh:
C LsNde:
C NdeFrnt:
C MaxFrontEl:
C MNde:
C XNode:
C YNode:
C MVvr:
C NFrmNde:
C NghVvr:
C XVvr:
C YVvr:
C RadVvr:
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      REAL SpcPw,StrPw,StrPwInv
      DIMENSION FlsOutVv(*),LsFlgVvr(*),FlsFlgVv(*),
     &          SpcNew(*),FlsGoodVv(*),Nde(5),
     &          NBndNde(5),S(3),NFVvr(*),NBVvr(0:*),
     &          LsNgh(*),LsNde(*),
     &          NdeFrnt(2,*),XNode(*),YNode(*),NFrmNde(3,*),NghVvr(3,*),
     &          XVvr(*),YVvr(*),RadVvr(*),NdxLBnNd(0:*),NmNghFNd(*),
     &          NmNghLNd(*),NvNmBn(-1:*),IBndType(*),LmtNde(*),
     &          NBFrmNde(3,*),NghBVvr(3,*),BkgSpc(*),BkgStr(*),
     &          BkgVol(*),FBkgStr(*),ILevel(*)
C      LOGICAL*1 FalseFlag
C      PARAMETER ( FalseFlag = .FALSE. )
      NBVvr(0) = 5
      MBndNde = NdxLBnNd(MBnd)
C
C     Initialize FlsGoodVv to .T. for all triangles that are
C     outside the domain and have an edge on the boundary.
      FlsGoodVv(1) = .FALSE.
      FlsGoodVv(2) = .FALSE.
      FlsGoodVv(3) = .FALSE.
      FlsGoodVv(4) = .FALSE.
      DO NVvr = 5,MVvr
        FlsGoodVv(NVvr) = .FALSE.
      END DO
C
C     Mark all triangles outside the domain as good in order to
C     prevent node insertion. The possible frontal edges at the
C     non-frontal boundaries are suppressed with the check on 
C     boundary types.
      DO NVvr = 5,MVvr
        IF (FlsOutVv(NVvr))  FlsGoodVv(NVvr) = .TRUE.
      END DO
C
      IF (FStretch) THEN
C       Deal with the stretched space by flagging all protected
C       cells as good.
C
        IF (IVSmooth.GT.0) THEN
C         Find the outermost cells.
          DO NVvr = 5,MVvr
            Ngh1 = NghVvr(1,NVvr)
            Ngh2 = NghVvr(2,NVvr)
            Ngh3 = NghVvr(3,NVvr)
            IF (.NOT.(FlsOutVv(Ngh1).AND.FlsOutVv(Ngh2).AND.
     &                FlsOutVv(Ngh3))) THEN
C             This cell has an unprotected neighbor. unprotect it.
              FlsFlgVv(NVvr) = .TRUE.
            END IF
          END DO
        END IF
C
        IF (IVSmooth.EQ.2) THEN
C         Unprotect the outermost cells and their neighbors 
          DO NVvr = 5,MVvr
            IF (FlsFlgVv(NVvr)) THEN
              Ngh1 = NghVvr(1,NVvr)
              Ngh2 = NghVvr(2,NVvr)
              Ngh3 = NghVvr(3,NVvr)
              FlsOutVv(NVvr) = .FALSE.
              FlsOutVv(Ngh1) = .FALSE.
              FlsOutVv(Ngh2) = .FALSE.
              FlsOutVv(Ngh3) = .FALSE.
            END IF
          END DO
C
        ELSE IF (IVSmooth.EQ.1) THEN
C         Unprotect only the outermost cells. 
          DO NVvr = 5,MVvr
            IF (FlsFlgVv(NVvr)) FlsOutVv(NVvr) = .FALSE.
          END DO
C
        END IF
C
C       Mark all interior, according to IVSmooth, cells as good.
        DO NVvr = 5,MVvr
          IF (FlsOutVv(NVvr).AND.
     &        (NFrmNde(1,NVvr).GT.NdxLBnNd(MBnd).OR.
     &         NFrmNde(2,NVvr).GT.NdxLBnNd(MBnd).OR.
     &         NFrmNde(3,NVvr).GT.NdxLBnNd(MBnd))) THEN
C             One of the forming nodes is a viscous node. As the
C             cell is protected, it must be an internal visous cell.
              FlsGoodVv(NVvr) = .TRUE.
          END IF
        END DO
C
      END IF
C
C     Protect all cells outside the domain.
      CALL FLGOUT (LsFlgVvr,FlsFlgVv,MOutVvr,
     &             MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &             NvNmBn,IBndType,MNde,XNode,YNode,
     &             MVvr,NFrmNde,NghVvr,
     &             NLevel,ILevel)
      DO NVvr = 5,MVvr
        IF (FlsFlgVv(NVvr)) FlsOutVv(NVvr) = .TRUE.
      END DO
C
      IF (MIsoRow.EQ.0.AND.FAskRow) THEN
        WRITE (*,104)
  104   FORMAT (6X,' How many isotropic rows?',$)
        IF (FLogFile) WRITE (NtLog,104) 
        READ (*,*) NXtraRow
        IF (FLogFile) WRITE (NtLog,'(I4)') NXtraRow
        IF (NXtraRow.LT.1) RETURN
        MIsoRow = MIsoRow+NXtraRow
      END IF
C
C     Insert while bad triangles are found.
      Found = .TRUE.
      MRow = 0
      MTry = 0
      DO WHILE (Found.AND.MRow.LT.MIsoRow)
        Found = .FALSE.
        MRow = MRow+1
C       Reset the counter of new nodes to introduce.
        MNewNde = 0
C
C       Loop over all existing triangles.
        DO NVvr = 5,MVvr
          IF (.NOT.FlsGoodVv(NVvr)) THEN
C           This is an internal triangle that is not cleared, yet.
            Found = .TRUE.
C           Numbers of the forming nodes.
            Nde(1) = NFrmNde(1,NVvr)
            Nde(2) = NFrmNde(2,NVvr)
            Nde(3) = NFrmNde(3,NVvr)
            Nde(4) = Nde(1)
            Nde(5) = Nde(2)
C           Length of the sides.
            S(3) = (XNode(Nde(1))-XNode(Nde(2)))**2+
     &             (YNode(Nde(1))-YNode(Nde(2)))**2
            S(1) = (XNode(Nde(2))-XNode(Nde(3)))**2+
     &             (YNode(Nde(2))-YNode(Nde(3)))**2
            S(2) = (XNode(Nde(3))-XNode(Nde(1)))**2+
     &             (YNode(Nde(3))-YNode(Nde(1)))**2
C           Find the longest face.
            SMax = DMAX1(S(1),S(2),S(3))
            SMin = DMIN1(S(1),S(2),S(3))
C
            IF (SMin.GE.QTol2*SMax) THEN
              FlsGoodVv(NVvr) = .TRUE.
            ELSE
C             List all short edges.
              DO KSide = 1,3
                IF (S(KSide).LT.QTol2*SMax) THEN
                  MTry = MTry+1
                  IF (MTry.GT.MaxFrontEl) THEN
                    IF (IVerbose.GT.0) 
     &              WRITE (*,102) MTry,MaxFrontEl
                    IF (FLogFile)
     &              WRITE (NtLog,102) MTry,MaxFrontEl
  102               FORMAT (' FATAL: Attempt to list more than',I5,
     &                      ' possible frontal edges,'/
     &                      '        but only',I5,' allocated.')
                    STOP
                  END IF
C                 Cell to start the search with in FNDNDE. Take the
C                 good triangle as it is unlikely to be broken.
                  NFVvr(MTry) = NghVvr(KSide,NVvr)
                  NBVvr(MTry) = MBVvr
                  NdeFrnt(1,MTry) = NVvr
                  NdeFrnt(2,MTry) = KSide
                END IF
              END DO
            END IF
C
          END IF
        END DO
C
C       Loop over all listed edges.
        MSkip = 0
        DO NTry = 1,MTry
          NVvr = NdeFrnt(1,NTry)
          KSide = NdeFrnt(2,NTry)
          IF (FlsGoodVv(NghVvr(KSide,NVvr))) THEN
C           This edge might be frontal, proceed.
            Nde(1) = NFrmnde(1,NVvr)
            Nde(2) = NFrmnde(2,NVvr)
            Nde(3) = NFrmnde(3,NVvr)
            Nde(4) = Nde(1)
            Nde(5) = Nde(2)
            Frontal = .TRUE.
            IF (Nde(KSide+1).LE.NdxLBnNd(MBnd)) THEN
C             This node is a boundary node. Check whether it is on
C             a frontal boundary.
              NBnd = 1
              DO WHILE (NdxLBnNd(NBnd).LT.Nde(KSide+1))
                NBnd = NBnd+1
              END DO
              IF (IBndType(NBnd).NE.1) Frontal = .FALSE.
            END IF
            IF (Frontal) THEN
C             List the two forming nodes of the frontal edge.
              NdeFrnt(1,NTry) = Nde(KSide+1)
              NdeFrnt(2,NTry) = Nde(KSide+2)
            ELSE
              NdeFrnt(1,NTry) = 0
              MSkip = MSkip+1
            END IF
          ELSE
            NdeFrnt(1,NTry) = 0
            MSkip = MSkip+1
          END IF
        END DO
C
C       Loop over all edges. Check whether they are frontal. Construct
C       a new point for each edge and try to introduce.
        NNew = MNde
        DO NTry = 1,MTry
C
          IF (NdeFrnt(1,NTry).NE.0) THEN
C           Construct a new point for each edge and try to introduce.
            CALL FNDNDE (NdeFrnt(1,NTry),NdeFrnt(2,NTry),
     &                   NBVvr(NTry),MNewNde,FBuild,SpcNew(NTry),
     &                   MNde,XNode,YNode,
     &                   MBVvr,NBFrmNde,NghBVvr,
     &                   BkgSpc,BkgVol,DMax,SpcPw,
     &                   BkgStr,BndStr,DomStr,StrPw,StrPwInv,FBkgStr,
     &                   QTolRtHi,QTolRtLo,NLevel)
C
C           Memorize the original neighbor of suitable shape,
C           NFVvr(NTry) that the badly shaped cell at the origin
C           of this edge can be flagged good.
            NghOrig = NFVvr(NTry)
C
            IF (FBuild) THEN
              NNew = MNde+MNewNde
C             Find the foreground cell that contains the new node.
              PFNew = IWALK(XNode(NNew),YNode(NNew),NFVvr(NTry),
     &                     XNode,YNode,NFrmNde,NghVvr,MVvr,FoutCnv)


C             Dump the current grid. Don't forget to uncomment
C             the definition of FalseFlag at the top.
C              justDoIt = 0
C              if ( justDoIt ) then
C                open ( 11,FILE='dump.dpl',status='unknown',
C     &               form='formatted')
C                CALL HULDPL (MVvr,NFrmNde,NghVvr,
C     &               BkgSpc,BkgStr,FBkgStr,
C     &               Mnde+MnewNde-1,XNode,YNode,
C     &               RadVvr,FalseFlag,11)
C                close (11)
C              endif

              IF (FlsOutVv(PFNew)) THEN
C               The cell that contains NNew is outside the domain.
C               Discard NNew.
                FBuild = .FALSE.
                MNewNde = MNewNde-1
C
              ELSE IF (NLevel.LT.2) THEN
C               Finest Grid. Check the spacing.
C               Distance from the circumcenter to the new node.
                CrcCntDst = SQRT((XVvr(PFNew)-XNode(NNew))**2+
     &                           (YVvr(PFNew)-YNode(NNew))**2)
C               Shortest distance between the disk and the circumcircle.
                Rim = SQRT(RadVvr(PFNew))-
     &                CrcCntDst-DTol*SpcNew(NTry)
                IF (Rim.GT.0.) THEN
C                 The spacing disc around this node is competely 
C                 contained in the circumcircle around PFNew.  
C                 This guarantees proper  spacing.
                ELSE
C                 Check the spacing with the nodes in the structure.
                  CALL SPCOLCHK (NNew,NFVvr(NTry),NBVvr(NTry),
     &                           SpcNew(NTry),
     &                           FSpcOK,NdeX,
     &                           MNde,XNode,YNode,
     &                           MVvr,NFrmNde,NghVvr,XVvr,YVvr,
     &                           MBVvr,NBFrmNde,BkgStr,FBkgStr,
     &                           BndStr,DomStr,StrPw,StrPwInv,
     &                           DTol2,DMax,
     &                           LsFlgVvr,FlsFlgVv,MaxLsFlg)
C
                  IF (.NOT.FSpcOK) THEN
                    FBuild = .FALSE.
                    MNewNde = MNewNde-1
                    IF (NdeX.GT.MNde) THEN
C                     NdeX is on the outermost row, merge it.
                      CALL NDEMRG(NdeX,NNew,NFVvr(NTry),NBVvr(NTry),
     &                            XNode,YNode,MNde+MNewNde,
     &                            MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                            FlsOutVv,LsFlgVvr,FlsFlgVv,
     &                            LsNde,LsNgh,SpcNew(NTry),RadTolMi,
     &                            MBVvr,NBFrmNde,BkgStr,FBkgStr,
     &                            BndStr,DomStr,StrPw,StrPwInv,
     &                            DTol2,DMax,MaxLsFlg)
                    END IF
                  END IF
C
                END IF
C
              ELSE 
C               Coarser grid level. Warp NNew onto an existing node
C               of the previous level.
                XNew = XNode(NNew)
                YNew = YNode(NNew)
                NdxP = 1-MVvrPrvLv
                PFPrv = MVvrPrvLv
                
                CALL IVVALK(XNew,YNew,PFPrv,NNdeClose,
     &                      XVvr(NdxP),YVvr(NdxP),NFrmNde(1,NdxP),
     &                      NghVvr(1,NdxP),MVvrPrvLv,
     &                      FOutCnv,ILevel,NghLvMax)
                IF ( NNdeClose .EQ. 0 ) THEN
C                 IVVALK failed. In Multigrid, no recovery. ;-(.
                  WRITE (*,'(A)')
     &            ' FATAL: IVVALK failed.'
                  STOP
                END IF
      
                IF (NghLvMax.LT.NLevel) THEN
C                 There is a buffer of coarsened nodes around NNdeClose.
C                 Warp and intro.
                  NNew = NNdeClose
                  FBuild = .TRUE.
C                 Find the containing cell of the warped node.
                  PFNew = IWALK(XNode(NNew),YNode(NNew),PFNew,
     &                          XNode,YNode,NFrmNde,NghVvr,MVvr,FoutCnv)
                ELSE
C                 Try to find a vertex connected to NNdeClose that
C                 has only NLevel-1 order neighbors.
                  CALL IVVNXT (XNew,YNew,PFPrv,NNdeClose,
     &                         XVvr(NdxP),YVvr(NdxP),
     &                         NFrmNde(1,NdxP),NghVvr(1,NdxP),MVvrPrvLv,
     &                         MBndNde,XNode,YNode,ILevel,MNde,NLevel,
     &                         FOutCnv,NNdeNxt,NghLvMax,DNxt,
     &                         IVerbose,FLogFile,NtLog)
C
                  IF (NghLvMax.LT.NLevel) THEN
C                   There is a buffer of coarsened nodes around 
C                   NNdeNxt. Warp and intro.
                    NNew = NNdeNxt
                    FBuild = .TRUE.
C                   Find the containing cell of the warped node.
                    PFNew = IWALK(XNode(NNew),YNode(NNew),PFNew,
     &                            XNode,YNode,NFrmNde,NghVvr,MVvr,
     &                            FoutCnv)
                  ELSE
C                   Discard, not coarse enough.
                    FBuild = .FALSE.
                    MNewNde = MNewNde-1
                  END IF
C
                END IF
C
              END IF
C
              IF (FBuild) THEN
C               There is a suitable new vertex. Introduce.
C               Find the constrained cavity.
                CALL FLGVVRCN (NNew,PFNew,RadTolMi,
     &                         MVvr,XVvr,YVvr,NFrmNde,
     &                         NghVvr,RadVvr,FlsOutVv,LsFlgVvr,FlsFlgVv,
     &                         MFlgVvr,FOut)
C               Connect.
C               Prevent ungraceful abortion on linear degeneracy.
                MNwVvr = -99
                CALL CNCTNDE (NNew,MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                        LsFlgVvr,FlsFlgVv,MNwVvr)

                IF (MNwVvr.GT.0) THEN
                  ILevel(NNew) = NLevel
C                 Erase the skewness flags for the new triangles.
                  DO ILVvr = 1,MFlgVvr
                    NVvr = LsFlgVvr(ILVvr)
                    FlsGoodVv(NVvr) = .FALSE.
                  END DO
                  DO NVvr = MVvr+1,MVvr+MNwVvr-MFlgVvr
                    FlsGoodVv(NVvr) = .FALSE.
                  END DO
                  DO NVvr = MVvr+1,MVvr+MNwVvr
                    FlsOutVv(NVvr) = .FALSE.
                  END DO
C
C                 Clean up.
                  CALL PSTPRCS (LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr,
     &                          FlsOutVv,
     &                          MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &                          XNode,YNode)
C
                ELSE
C                 Unsuccessful CNCTNDE.
                  FBuild = .FALSE.
                  IF (IVerbose.GT.5)
     &                 write (*,'(A,I7,E11.5,E11.5)')
     &                 ' Rejected:',NNew,XNode(NNew),YNode(NNew) 
                END IF
C
              END IF
C
            END IF
C
            IF (.NOT.FBuild) THEN
C             Find back the original cell, check whether the
C             edge still exists, mark the neighbor across as
C             good.
              IF (FlsGoodVv(NghOrig)) THEN
                Nde(1) = NFrmNde(1,NghOrig)
                Nde(2) = NFrmNde(2,NghOrig)
                Nde(3) = NFrmNde(3,NghOrig)
                Nde(4) = Nde(1)
                Nde(5) = Nde(2)
                DO KNde = 1,3
                  IF (Nde(KNde+2).EQ.NdeFrnt(1,NTry).AND.
     &                Nde(KNde+1).EQ.NdeFrnt(2,NTry)) THEN
                    FlsGoodVv(NghVvr(KNde,NghOrig)) = .TRUE.
                  END IF
                END DO
              END IF
            END IF
C
          END IF
C
        END DO
C
        IF (MNewNde.GT.0.AND.Found) THEN
C         Update  the number of nodes in the structure.
          IF (NLevel.EQ.1) MNde = MNde+MNewNde
          MTry = MTry-MSkip
C
C         User information.
          IF (IVerbose.GE.5) THEN
            WRITE (*,101)  MTry,MNewNde,MNde,MRow
          ELSE IF (IVerbose.GE.3) THEN
            WRITE (*,100)  MNewNde,MNde,MRow
          END IF
          IF (FLogFile)
     &    WRITE (NtLog,101)  MTry,MNewNde,MNde,MRow
  100     FORMAT (6X,I7,' new,',I7,' total nodes in ',I4,'. row',$)
  101     FORMAT (6X,I7,' tried,',I7,' new,',I7,' total nodes in ',
     &            I4,'. row',$)
C
          IF (MRow.GE.MIsoRow.AND.FAskRow) THEN
C           Prompt for extra rows.
            WRITE (*,99)  
            IF (FLogFile)
     &      WRITE (NtLog,99) 
   99       FORMAT (', how many more rows?',$)
            READ (*,*) NXtraRow
            IF (FLogFile)
     &      WRITE (NtLog,'(I4)') NXtraRow
            IF (NXtraRow.LT.1) RETURN
            MIsoRow = MIsoRow+NXtraRow
          ELSE
C           Linefeed.
            IF (IVerbose.GE.3) WRITE (*,98)  
            IF (FLogFile) WRITE (NtLog,98) 
   98       FORMAT ('.')
          END IF
          MTry = 0
C
        ELSE
          RETURN
        END IF
C
      END DO
C
      RETURN
      END
