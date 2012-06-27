      PROGRAM DELAUNDO
C
C Last update:
C 2Apr97; new chkbnd, that should be able to treat all geometries.
C         Avoid failure if a constructed node leads to degeneracy in
c         intcld. Version 5.4.
C 9Jan97; fix calculation of background scales in backgrd.f, treat
C         edges that are too long in fndnde. Version 5.3
C 21Nov96; fix stupid bug in ivvalk. Version 5.2
C 27Aug96; reset FlsOutVv after viscous meshing for LPLC.
C 11Aug96; pass MaxNode into readfip for bounds check.
C 22May96; 5.0.
C 18May96; new huldpl without nodes.cbl, new INTCLD.
C 12May96: read formatted coordinates as doubles.
C 13Apr96: fix bug in backgrd.f that didn't condsider type3
C          boundaries in the spacing calculation.
C 9Jan96: fix typo in format statement 33.
C Aug95; 4.4: collect various big fixes, foremost a fixed
C        ivvnxt.
C Sep94; 4.3: new construction of the ideal point that is
C        is better behaved when spacing gradients are large.
C April94; towards 4.2. (MLevel,LPLC,SWAPS,whatever)
C 5Dec93,13:45;intro FBndTest.
C 30Oct93,17:30;intro DltZero,DCoord.
C 9Feb93,23:59;Welcome to Version 4.0.
C
C DELAUNDO performs a Delaunay Triangulation of a given set of
C boundary points. Either a given interior point cloud is 
C used or a point cloud is being constructed with the frontal
C Domino approach. This point cloud is constructed either
C isotropically or in a stretched manner, depending on values
C interpolated on a background grid. The background grid is
C derived from the initial triangulation of boundary nodes and
C information inhibiting triangles to connect certain boundary
C segments. For stretching further information about the 
C thickness of the stretched layer close to solid walls and
C the maximum aspect ratio is needed.
C The grid is constructed with the Bowyer Algorithm and
C diagonal swapping is used to assure boundary
C integrity. Different output formats compatible with VKB's
C decplot, DDZ's dplot or AvS are available. Welcome to DOMINO 
C software.
C
C Standards of notation conceived by DOMINO-software within 
C the routines of DELAUNDO: 
C Max..: dimensions of arrays,
C I....: counters,
C N....: indices in lists,
C K....: position in the triangle,
C M....: sums, amounts,
C Ls...: temporary lists,
C Nv...: inverse lists,
C Fls..: flaglists (LOGICAL*1),
C Ndx..: pointers within arrays.
C
C Common Block /BKGRD/:  
C QTol:     Minimum tolerable ratio in sidelengths.
C DTol:     Minimum tolerable ratio of interpolated
C           spacing vs. found spacing.
C DltStar:  Thickness of a stretching layer around frontal 
C           frontal boundaries.
C BkgSpc:   Vector of the spacing values defined on the background
C           mesh.
C BkgStr:   Vector of the stretching values for the stretching
C           transformation.
C BkgVol:   Vector of triangle areas for gradient evaluation
C           according to Gauss' theorem on the background grid.
C MBVvr:    Number of Voronoi vertices in the background mesh.
C NBFrmNde: Array of the three forming nodes of each Voronoi vertex. 
C NghBVvr:  Array of the three neighboring Voronoi vertices to each 
C           Vvr.
C
C Common Block /NODES/:  
C MaxNode:  Maximum allowable number of nodes.
C MaxBound: Maximum allowable number of boundary segments.
C MaxBndNde:Maximum allowable number of boundary nodes.
C XNode:    Vector of x-coordinates of the nodes.
C YNode:    Vector of y-coordinates of the nodes.
C MBnd:     Number of boundaries.
C NmeBnd:   Vector of names given to the boundary segments.
C NvNmBnd:  Vector of numbers of the boundaries given the name.
C NmNghFNd: Vector of the names of the boundary neighbouring 
C           to the first node of the segment.
C NmNghLNd: Vector of the names of the boundary neighbouring 
C           to the last node of the segment.
C MBnNde:   Number of boundary nodes.
C IBndType: Type of boundary. See READFIP/READPTS for details.
C MNtNd:    Number of interior nodes.
C MNde:     Number of nodes introduced in the structure.
C NdxLBnNd: Vector of indices of the last node on each
C           boundary segment.
C LmtNde:   Vector of nodes determining each a closed set of
C           boundary segments.
C MLmt:     Number of closed sets of boundary segments (elements).
C NdxNotCon:Vector of pointers to LsNotCon boundaries that may not be
C           connected to the current boundary in the background
C           mesh.
C LsNotCon: Array of numbers of boundaries that may not be
C           conntected to the current boundary in the background
C           mesh.
C
C Common Block /VVRTC/:
C XVvr:     Vector of x-coordinates of the formed Voronoi Vertices.
C YVvr:     Vector of y-coordinates of the formed Voronoi Vertices.
C RadVvr:   Vector of the Squared radii of the circumcircles around
C           each Voronoi Vertice.
C RadTol:   Thickness of the circumcircle.
C RadTolPl: Factor for the lower bound of the thickness of the
C           circumcircle.
C RadTolMi: Factor for the higher bound of the thickness of the
C           circumcircle.
C MVvr:     Number of Voronoi Vertices in the structure.
C NFrmNde:  Array of the three forming nodes of each Voronoi
C           Vertice.
C NghVvr:   Array of the three neighbouring Voronoi vertices to each
C           Voronoi vertice.
C
C Common block /CTRLTXT/:
C NpFlNm:   Name of the input file of DBL.PTS type.
C NpFlNm2:  Name of the input file of .geo type.
C OpFlNm:   Name of the output file of .OUT or .GRI type.
C Title1-4: 4 Titles to be given for plotting purposes.
C Version:  Version name.
C
C Output:
C The output in VKB format is performed by the routines GRIWRITE
C or OUTWRITE which are called within OUTHUL or OUTGRI. Output in
C the .DPL format is performed by TRIDPL or HULDPL. See there 
C for details.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      INCLUDE 'nodes.cbl'
      INCLUDE 'bkgrd.cbl'
      INCLUDE 'vvrtc.cbl'

      DIMENSION LsFlgVvr(MaxLsFlg),FlsFlgVv(2*MaxNode),
     &          LsOutVvr(MaxLsFlg),FlsOutVv(2*MaxNode),Tme(2),
     &          Index(1*MaxBndNde),Index2(1*MaxBndNde),
     &          MNrBnd(MaxBound),NrBnd(1*MaxBndNde),NdeCrnr(MaxBound),
     &          NQFrmNde(3,MaxLsFlg),NghQVvr(3,MaxLsFlg),
     &          FVvrChk(3,2*MaxNode),LsNVvr(MStck),LsKNgh(MStck),
     &          FChkBnd(MaxBound),
     &          XFrst(MaxBound),YFrst(MaxBound),XLst(MaxBound),
     &          YLst(MaxBound),ArcRel(MaxBndNde),DltZero(MaxBndNde),
     &          NdxLarc(0:MaxBound),NdxNotCon(0:MaxBound),
     &          LsNotCon(10*MaxBound),SpcNew(MaxBndNde),
     &          FlsGoodVv(2*MaxNode),NFVvr(MaxFrontEl),
     &          NBVvr(0:MaxFrontEl),LsNgh(MaxLsFlg),
     &          LsNde(MaxLsFlg),NdeFrnt(2,MaxFrontEl),
     &          NdxMVvr(0:10),ILevel(MaxNode),MinRes(MaxBound)
      CHARACTER*255 NpFlNm,NpFlNm2,OpFlNm,LogFlNm,Title1,Title2,Title3,
     &             Title4
      CHARACTER Code*9, Version*4, Date*14
      CHARACTER*1 CharOutType,CharOutForm,CharLevel
      REAL SpcGrdRt,RelFac
      DOUBLE PRECISION Coord(2*MaxBndNde)
      DOUBLE PRECISION DCoord(MaxBndNde)

      LOGICAL*1 FalseFlag
      Parameter ( FalseFlag = .FALSE. )
C
      Code = 'Delaundo'
      Version = '5.4'
      Date = '02 April 1997'
C
C     Logical units.
      NtPts = 1
      NtOpFl = 3
      NtCtr = 2
      NtLog = 4
C     Other garbage.
      NLevel = 1
      NdxMVvr(0) = 0
      TimeZero = DTIME(Tme)
      ICharZero = ICHAR('0')
C
C     Read the control parameters.
      CALL GETCTRL (NtCtr,NtLog,IVerbose,NpFlNm,KHiNpFlNm,
     &              FNpFlFrm,FIntNde,FConNde,FAskRow,FNotCon,
     &              SpcGrdRt,DTol,QTol,FStretch,BTol,DltStar,
     &              BndStr,MVisRow,ISmooth,MIsoRow,FFlatSwap,
     &              AnglMax,MCyclSwap,OpFlNm,KHiOpFlNm,
     &              CharOutType,CharOutForm,
     &              Title1,KHiTitle1,Title2,KHiTitle2,
     &              Title3,KHiTitle3,Title4,KHiTitle4,
     &              FLogFile,Code,Version,Date,FAllDisc,MLevel,
     &              FLplc,Beta,RelFac,MSweep)
C
      DTol2 = DTol**2
      QTol2 = QTol**2
      QtolRtHi = SQRT(1./QTol2-.25)
      QTolRtLo = SQRT(QTol2-.25)
C
C     Read the data from the DBL.PTS file.
      IF (FNpFlFrm) THEN
        OPEN(NtPts,FILE=NpFlNm,STATUS='OLD',FORM='FORMATTED')
        REWIND NtPts
        FBndTest = .TRUE.
        CALL READFIP (NtPts,NtLog,IVerbose,FLogFile,
     &                MBnd,NmeBnd,NvNmBn,NdxLBnNd,
     &                NmNghFNd,NmNghLNd,IBndType,
     &                LmtNde,MLmt,NdxNotCon,LsNotCon,MNde,XNode,YNode,
     &                ArcRel,DltZero,DltStar,NdxLArc,
     &                Coord,DCoord,XFrst,YFrst,XLst,YLst,FChkBnd,
     &                FBndTest,MinRes,MaxNode)
        CLOSE (NtPts)
      ELSE
        OPEN(NtPts,FILE=NpFlNm,STATUS='OLD',FORM='UNFORMATTED')
        REWIND NtPts
        CALL READPTS (NtPts,NdxNotCon,LsNotCon)
        CLOSE (NtPts)
      END IF
      MUserBnd = MBnd
C
C     Define the initial triangulation of the convex hull.
      TimeSetup = DTIME(Tme)
      CALL SETUP (NdxLBnNd(MBnd),XNode,YNode,
     &            MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &            RadTolRel,RadTol,RadTolMi,RadTolPl,
     &            IVerbose,FLogFile,NtLog)
C
C     Establish a pointer to the different boundary segments.
      IF (FIntNde) THEN
C       Use the given internal point cloud.
        MBnd = MBnd+1
        NdxLBnNd(MBnd) = MNde
        IBndType(MBnd) = 0
      ELSE
C       Don't ue any internal points.
        MNde = NdxLBnNd(MBnd)
      END IF
C
C     Loop over the nodes.
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).NE.9) THEN
          PFVvr = MVvr
          DO NNde = NdxLBnNd(NBnd-1)+1,NdxLBnNd(NBnd)
C
C           Find the Voronoi vertices to be deleted.
            PFVvr = IWALK(XNode(NNde),YNode(NNde),PFVvr,
     &                    XNode,YNode,NFrmNde,NghVvr,MVvr,FOutCnv)
C
C           Find the constrained cavity.
            CALL FLGVVRCN (NNde,PFVvr,RadTolMi,MVvr,XVvr,YVvr,NFrmNde,
     &                     NghVvr,RadVvr,FlsOutVv,LsFlgVvr,FlsFlgVv,
     &                     MFlgVvr,FOut)
C
C           Check whether FLGVVRCN was successful.
            IF (FOut) THEN
              N1 = NFrmNde(1,PFVvr)
              N2 = NFrmNde(2,PFVvr)
              N3 = NFrmNde(3,PFVvr)
              IF (FLogFile) WRITE (NtLog,38)
     &          PFVvr,N1,XNode(N1),YNode(N1),N2,XNode(N2),YNode(N2),
     &          N3,XNode(N3),YNode(N3),NNde,XNode(NNde),YNode(NNde)
              WRITE (*,38)
     &          PFVvr,N1,XNode(N1),YNode(N1),N2,XNode(N2),YNode(N2),
     &          N3,XNode(N3),YNode(N3),NNde,XNode(NNde),YNode(NNde)
 38           FORMAT (/
     &        ' FATAL: Cell',I7,' formed with vertices'/
     &        3('            ',I7,' at x/y',2E16.8/),
     &        '        does not contain vertex'/
     &        '            ',I7,' at x/y',2E16.8/
     &        '        in its guaranteed circumcircle.',
     &        ' Do check for vertex duplication.')
              STOP
            END IF
C
C           Connect the new node with the valid structure.
            CALL CNCTNDE (NNde,MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                    LsFlgVvr,FlsFlgVv,MNwVvr)
C
C           Reordering the data structure, deleting obsolete elements.
            CALL PSTPRC (MVvr,XVvr,YVvr,NFrmNde,NghVvr,RadVvr,
     &                   LsFlgVvr,MFlgVvr,FlsFlgVv,MNwVvr)
C
          END DO
        END IF
      END DO
C
C     In case of FConNde = .F..
      NdxMVvr(1) = MVvr
C
      IF (CharOutType.NE.'i') THEN
C       Check the boundaries.
        TimeCheck = DTIME(Tme)
        CALL CHKBND(MLmt,LmtNde,NdxLBnNd,NvNmBn,NmNghFNd,MBnd,
     &       IBndType,XNode,YNode,NLevel,ILevel,RadTolMi,
     &       MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &       FlsOutVv,FlsFlgVv,LsFlgVvr,LsNde,LsNgh,MaxLsFlg)

C       Flag all triangles outside the domain.
        CALL FLGOUT (LsOutVvr,FlsOutVv(1),MOutVvr,
     &               MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &               NvNmBn,IBndType,MNde,XNode,YNode,
     &               MVvr,NFrmNde(1,1),NghVvr(1,1),
     &               NLevel,ILevel)
C
C       Eliminate all circumcircles of the outside triangles and mark
C       this region as nicely triangulated.
        DO NVvr = 1,MVvr
          IF (FlsOutVv(NVvr)) RadVvr(NVvr) = 0.D0
        END DO
      END IF
C
      TimeBkgrd = DTIME(Tme)
      MUsrBnd = MBnd
      IF (FConNde.OR.CharOutType.EQ.'b') THEN
C       Construct the background mesh to interpolate spacing on.
        CALL BACKGRD (FlsOutVv,LsFlgVvr,FlsFlgVv,FStretch,
     &                FNotCon,SpcGrdRt,IVerbose,NtLog,FLogFile,
     &                ArcRel,DltZero,NdxLArc,NdxNotCon,LsNotCon,
     &                FAllDisc)
        CALL CHKBND(MLmt,LmtNde,NdxLBnNd,NvNmBn,NmNghFNd,MBnd,
     &       IBndType,XNode,YNode,NLevel,ILevel,RadTolMi,
     &       MBVvr,NBFrmNde,NghBVvr,XVvr,YVvr,RadVvr,
     &       FlsOutVv,FlsFlgVv,LsFlgVvr,LsNde,LsNgh,MaxLsFlg)
      END IF
      MBkgBnd = MBnd-MUsrBnd
C
      TimeVisCon = DTIME(Tme)
      IF (FStretch.AND.FConNde.AND.MVisRow.GT.0) THEN
        CALL VISBOX (LsFlgVvr,FlsFlgVv,FlsOutVv,FAskRow,MVisRow,
     &               IVerbose,NtLog,FLogFile)
        MBnd = MUsrBnd+MBkgBnd
      END IF
      MVisNode = MNde-NdxLBnNd(MBnd)
C     In case of FConNde = .F..
      NdxMVvr(1) = MVvr
C
      TimeIsoCon = DTIME(Tme)
      IF (FConNde.AND.(MIsoRow.GT.0.OR.FAskRow)) THEN
C       Construct interior cloud.
        IF (IVerbose.GE.3)
     &  WRITE (*,'(/3X,A)') ' Constructing isotropic nodes:'
        IF (FLogFile)
     &  WRITE (NtLog,'(/3X,A)') ' Constructing isotropic nodes:'
C
        CALL INTCLD (FlsOutVv,LsFlgVvr,FlsFlgVv,IVSmooth,
     &               FAskRow,MIsoRow,IVerbose,NtLog,FLogFile,
     &               FStretch,
     &               DTol,DTol2,QTol2,QTolRtHi,QTolRtLo,RadTolMi,
     &               SpcNew,FlsGoodVv,NFVvr,NBVvr,LsNgh,LsNde,
     &               NdeFrnt,MaxFrontEl,
     &               MNde,XNode,YNode,
     &               MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &               MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &               NvNmBn,IBndType,MLmt,LmtNde,
     &               MBVvr,NBFrmNde,NghBVvr,
     &               BkgSpc,BkgVol,DMax,SpcPw,
     &               BkgStr,BndStr,DomStr,StrPw,StrPwInv,FBkgStr,
     &               NLevel,ILevel,0,MaxLsFlg)
C
C       Smoothen the mesh using Richter's barycentrage pond'er'e.
C       Note that a call to LPLC invalidates the contents of X/YVvr,
C       RadVvr.
        IF (FLplc) THEN
C         Reset FlsOutVv to the cells outside the domain.
          CALL LPLC (IVerbose,NtLog,FLogFile,
     &               MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,FlsOutVv,
     &               MNde,XNode,YNode,
     &               MBnd,NdxLBnNd,NvNmBn,NmNghFNd,NmNghLNd,IBndType,
     &               Beta,RelFac,MSweep,MVisNode)
C         Restore the connectivity to Delaunay, recalculate the Vvr.
          CALL DLNYRSTR (MVvr,NFrmNde,NghVvr,XVvr,YVvr,RadVvr,
     &                   XNode,YNode,MCyclSwap,
     &                   FlsOutVv,FVvrChk,MStck,LsNVvr,LsKNgh,
     &                   IVerbose,NtLog,FLogFile)
C
        END IF
C
C       Coarsen MLevel Multigrid levels.
        MVvrL = MVvr
C       Initialize the original set of boundary nodes to be at level 1,
C       if needed.
        IF (MLevel.GT.1) THEN
          DO NNde = 5,NdxLBnNd(MBnd)
            ILevel(NNde) = 1
          END DO
        END IF
        NdxMVvr(1) = MVvr
        MVvrL = MVvr
C
        DO NLevel = 2,MLevel
          IF (IVerbose.GE.2) WRITE (*,33) NLevel,2**(NLevel-1)
          IF (FLogFile) WRITE (NtLog,33) NLevel,2**(NLevel-1)
   33     FORMAT (/3X,' Generating coarse grid at level',I3,
     &            ' i.e. 1/',I3)
C
C         Start a new set of cells. I.e., a
C         new grid starts at MVvr+1.
          NdxP = NdxMVvr(NLevel-1)+1
          MVvrPrvLv = NdxMVvr(NLevel-1)-NdxMVvr(NLevel-2)
C         Create a new convex hull.
          CALL SETUP (NdxLBnNd(MUserBnd),XNode,YNode,
     &                MVvrL,XVvr(NdxP),YVvr(NdxP),
     &                NFrmNde(1,NdxP),NghVvr(1,NdxP),RadVvr(NdxP),
     &                RadTolRel,RadTol,RadTolMi,RadTolPl,
     &                IVerbose,FLogFile,NtLog)
C
          CALL COARSEINI (NLevel,ILevel,
     &                    MBnd,MUserBnd,NdxLBnNd,IBndType,NmNghFNd,
     &                    NmNghLNd,NmeBnd,NvNmBn,MinRes,
     &                    MVvrL,XVvr(NdxP),YVvr(NdxP),RadVvr(NdxP),
     &                    NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &                    RadTolMi,FlsOutVv(NdxP),FlsFlgVv,LsFlgVvr,
     &                    MNde,XNode,YNode,
     &                    BkgSpc,IVerbose,FLogFile,NtLog)
C
          CALL CHKBND(MLmt,LmtNde,NdxLBnNd,NvNmBn,NmNghFNd,MBnd,
     &         IBndType,XNode,YNode,NLevel,ILevel,RadTolMi,
     &         MVvrL,NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &         XVvr(NdxP),YVvr(NdxP),RadVvr(NdxP),
     &         FlsOutVv,FlsFlgVv,LsFlgVvr,LsNde,LsNgh,MaxLsFlg)

C
          CALL FLGOUT (LsOutVvr,FlsOutVv(NdxP),MOutVvr,
     &                 MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &                 NvNmBn,IBndType,MNde,XNode,YNode,
     &                 MVvrL,NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &                 NLevel,ILevel)
C
          CALL INTCLD (FlsOutVv(NdxP),LsFlgVvr,FlsFlgVv,IVSmooth,
     &                 FAskRow,MIsoRow,IVerbose,NtLog,FLogFile,
     &                 FStretch,
     &                 DTol,DTol2,QTol2,QTolRtHi,QTolRtLo,RadTolMi,
     &                 SpcNew,FlsGoodVv,NFVvr,NBVvr,LsNgh,LsNde,
     &                 NdeFrnt,MaxFrontEl,
     &                 MNde,XNode,YNode,
     &                 MVvrL,NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &                 XVvr(NdxP),YVvr(NdxP),RadVvr(NdxP),
     &                 MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &                 NvNmBn,IBndType,MLmt,LmtNde,
     &                 MBVvr,NBFrmNde,NghBVvr,
     &                 BkgSpc,BkgVol,DMax,SpcPw,
     &                 BkgStr,BndStr,DomStr,StrPw,StrPwInv,FBkgStr,
     &                 NLevel,ILevel,MVVrPrvLv,MaxLsFlg)

          NdxMVvr(NLevel) = NdxMVvr(NLevel-1)+MVvrL
          IF (IVerbose.GE.2) 
     &      WRITE (*,36) MVvrPrvLv,MVvrL,FLOAT(MVvrPrvLv)/MVvrL
          IF (FLogFile) 
     &      WRITE (NtLog,36) MVvrPrvLv,MVvrL,FLOAT(MVvrPrvLv)/MVvrL
   36     FORMAT (6X,' Coarsened from',I7,' to ',I7,
     &            ' cells, i.e. by a factor of',F5.2)
        END DO
      END IF
      MIsoNode = MNde-MVisNode-NdxLBnNd(MBnd)
C
      IF (FFlatSwap) THEN
C       Reset FlsOutVv to the cells outside the domain.
        CALL FLGOUT (LsOutVvr,FlsOutVv,MOutVvr,
     &               MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &               NvNmBn,IBndType,MNde,XNode,YNode,
     &               NdxMVvr(1),NFrmNde(1,1),NghVvr(1,1),
     &               1,ILevel)
C       Try to swap away all angles larger than AnglMax.
        ISwap = 1
        CALL SWAPS (NdxMVvr(1),NFrmNde,NghVvr,XNode,YNode,MCyclSwap,
     &              AnglMax,FlsOutVv,FVvrChk,MStck,LsNVvr,LsKNgh,
     &              IVerbose,NtLog,FLogFile,ISwap)
C
      END IF
C
C     User Info.
      MBndNde = 0
      MAutoNde = 0
      DO NBnd = 1,MBnd
        IF (IBndType(NBnd).EQ.9.OR.IBndType(NBnd).EQ.10) THEN
          MAutoNde = MAutoNde+NdxLBnNd(NBnd)-NdxLBnNd(NBnd-1)
        ELSE
          MBndNde = MBndNde+NdxLBnNd(NBnd)-NdxLBnNd(NBnd-1)
        END IF
      END DO
      IF (IVerbose.GE.1)
     &WRITE(*,32) MBndNde,MAutoNde,MVisNode,MIsoNode,
     &            MNde-MAutoNde-4,MVvr-MOutVvr
      IF (FLogFile)
     &WRITE(NtLog,32) MBndNde,MAutoNde,MVisNode,MIsoNode,
     &                MNde-MAutoNde-4,MVvr-MOutVvr
   32 FORMAT (/'    Grid size:'/
     &        '       Number of specified boundary nodes:   ',I6/
     &        '       Number of background boundary nodes:  ',I6/
     &        '       Number of constructed stretched nodes:',I6/
     &        '       Number of constructed isotropic nodes:',I6/
     &        '       Total number of nodes in the grid:    ',I6/
     &        '       Number of cells:                      ',I6)
C
      TimeWrite = DTIME(Tme)
C     Grid seems to be valid.
      CLOSE (NtOpFl)
      IF (CharOutForm.EQ.'d'.OR.CharOutForm.EQ.'u') THEN
C       Use .DPL or .inp format.
        OPEN(NtOpFl,FILE=OpFlNm,STATUS='UNKNOWN',FORM='FORMATTED')
      ELSE
C       Let's produce the VKB-output:
        OPEN(NtOpFl,FILE=OpFlNm,STATUS='UNKNOWN',FORM='UNFORMATTED')
      END IF
      REWIND NtOpFl
C
      IF (CharOutType.EQ.'h'.OR.CharOutType.EQ.'i') THEN
C       Show the convex hull.
        IF (CharOutForm.EQ.'d') THEN
C         Plot the triangulation of the convex hull in dpl format.
          FBkgrd = .FALSE.
          CALL HULDPL (NdxMVvr(1),NFrmNde,NghVvr,BkgSpc,BkgStr,FBkgStr,
     &                 Mnde,XNode,YNode,
     &                 RadVvr,FBkgrd,NtOpFl)
          DO NLevel = 2,MLevel
            CharLevel = CHAR(NLevel+ICharZero)
            CLOSE (NtOpFl)
            OPEN(NtOpFl,FILE=OpFlNm(1:KHiOpFlnm-1)//'.'//CharLevel,
     &           STATUS='UNKNOWN',FORM='FORMATTED')
            NdxP = NdxMVvr(NLevel-1)+1
            MVvrL = NdxMVvr(NLevel)-NdxP+1
            CALL HULDPL (MVvrL,NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &                   BkgSpc,BkgStr,FBkgStr,
     &                   Mnde,XNode,YNode,
     &                   RadVvr(NdxP),FBkgrd,NtOpFl)
          END DO

        ELSE IF (CharOutForm.EQ.'o') THEN
C         Plot the triangulation of the convex hull in .out format.
          DO IOutVvr = 1,MOutVvr
            FlsOutVv(LsOutVvr(IOutVvr)) = .FALSE.
          END DO
          MOutVvr = 0
          FOutFl = .TRUE.
C          CALL OUTHUL (LsOutVvr,FlsOutVV,MOutVvr,NtOpFl,FOutFl)
          WRITE (*,39)
          STOP
        ELSE IF (CharOutForm.EQ.'g') THEN
C         Plot the triangulation of the convex hull in .gri format.
          DO IOutVvr = 1,MOutVvr
            FlsOutVv(LsOutVvr(IOutVvr)) = .FALSE.
          END DO
          MOutVvr = 0
          FOutFl = .FALSE.
C          CALL OUTHUL (LsOutVvr,FlsOutVV,MOutVvr,NtOpFl,FOutFl)
          WRITE (*,39)
 39       FORMAT ( ' FATAL: No .gri or .out formats at the moment.' )
          STOP
        END IF
C
      ELSE IF (CharOutType.EQ.'b') THEN
C       Show the background grid.
        IF (CharOutForm.EQ.'d') THEN
C         Plot the background grid in .dpl format.
          FBkgrd = .TRUE.
          CALL HULDPL (MBVvr,NBFrmNde,NghBVvr,BkgSpc,BkgStr,FBkgStr,
     &                 MNde,Xnode,YNode,
     &                 RadVvr,FBkgrd,NtOpFl)
        END IF
C
      ELSE IF (CharOutType.EQ.'t') THEN
C       Triangulation.
        IF (CharOutForm.EQ.'d') THEN
C         Write triangulation to .DPL file.
          CALL FLGOUT (LsOutVvr,FlsOutVv(1),MOutVvr,
     &                 MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &                 NvNmBn,IBndType,MNde,XNode,YNode,
     &                 MVvr,NFrmNde(1,1),NghVvr(1,1),
     &                 1,ILevel)
C
          CALL RMBKG (NdxMVvr(MLevel),NFrmNde,MNde,XNode,YNode,
     &                MBnd,NdxLBnNd,IBndType)
C
          CALL TRIDPL (LsOutVvr,FlsOutVv,MOutVvr,
     &                 LsFlgVvr,NtOpFl,Code,Version,
     &                 MVvr,NFrmNde,NghVvr,
     &                 XVvr,YVvr,RadVvr,
     &                 MNde,XNode,YNode,
     &                 MBnd,NdxLBnNd,NmeBnd,IBndType,
     &                 NrBnd,MNrBnd,NdeCrnr,
     &                 Index,Index2)
C
          DO NLevel = 2,MLevel
            CharLevel = CHAR(NLevel+ICharZero)
            CLOSE (NtOpFl)
            OPEN(NtOpFl,FILE=OpFlNm(1:KHiOpFlnm-1)//'.'//CharLevel,
     &           STATUS='UNKNOWN',FORM='FORMATTED')
            NdxP = NdxMVvr(NLevel-1)+1
            MVvrL = NdxMVvr(NLevel)-NdxP+1
            CALL FLGOUT (LsOutVvr,FlsOutVv(NdxP),MOutVvr,
     &                   MLmt,LmtNde,MBnd,NdxLBnNd,NmNghFNd,NmNghLNd,
     &                   NvNmBn,IBndType,MNde,XNode,YNode,
     &                   MVvrL,NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &                   NLevel,ILevel)
C
            CALL TRIDPL (LsOutVvr,FlsOutVv(NdxP),MOutVvr,
     &                   LsFlgVvr,NtOpFl,Code,Version,
     &                   MVvrL,NFrmNde(1,NdxP),NghVvr(1,NdxP),
     &                   XVvr(NdxP),YVvr(NdxP),RadVvr(NdxP),
     &                   MNde,XNode,YNode,
     &                   MBnd,NdxLBnNd,NmeBnd,IBndType,
     &                   NrBnd,MNrBnd,NdeCrnr,
     &                   Index,Index2)
          END DO
        ELSE IF (CharOutForm.EQ.'o') THEN
C         Triangulation for output in .out format.
          FOutFl = .TRUE.
C          CALL TRIANGLE (LsOutVvr,FlsOutVV,MOutVvr,NtOpFl,FOutFl)
          WRITE (*,39)
          STOP
        ELSE IF (CharOutForm.EQ.'g') THEN
C         Triangulation for output in .gri format.
          FOutFl = .FALSE.
C          CALL TRIANGLE (LsOutVvr,FlsOutVV,MOutVvr,NtOpFl,FOutFl)
          WRITE (*,39)
          STOP
        END IF
C
      ELSE IF (CharOutType.EQ.'q') THEN
C       Quadratic triangulation.
        IF (CharOutForm.EQ.'d') THEN
          IF ( MNde .GE. MaxBndNde ) THEN
            WRITE (*,40) MaxBndNde
 40         FORMAT ( ' FATAL: Quadratic routines are compiled for',I7,
     &               ' nodes only.' )
            STOP
          ELSE
C           Write quadratic triangulation to .dpl file.
            CALL QUADDPL (LsOutVvr,FlsOutVv,MOutVvr,LsFlgVvr,
     &                    NtOpFl,Code,Version,
     &                    MVvr,NFrmNde,NQFrmNde,NghVvr,NghQVvr,
     &                    XVvr,YVvr,RadVvr,
     &                    MNde,XNode,YNode,
     &                    MBnd,NdxLBnNd,NmeBnd,IBndType,
     &                    NrBnd,MNrBnd,NdeCrnr,
     &                    Index,Index2)
          END IF
        ELSE IF (CharOutForm.EQ.'o') THEN
C         Create quadratic triangles for .out format.
          FOutFl = .TRUE.
          WRITE (*,39)
C          STOP
C          CALL QUADTRI (LsOutVvr,FlsOutVV,MOutVvr,NtOpFl,FOutFl)
        ELSE IF (CharOutForm.EQ.'g') THEN
C         Create quadratic triangles for .gri format.
          FOutFl = .FALSE.
C          CALL QUADTRI (LsOutVvr,FlsOutVV,MOutVvr,NtOpFl,FOutFl)
          WRITE (*,39)
C          STOP
        END IF
C
      END IF
      CLOSE (NtOpFl)
C
C     Goodbye.
      TimeFin = DTIME(Tme)
      IF (IVerbose.GE.4)
     &WRITE(*,30) TimeSetup,TimeCheck,
     &            TimeBkgrd,TimeVisCon,TimeIsoCon,TimeWrite,
     &            TimeFin,TimeFin+TimeWrite+TimeIsoCon+TimeVisCon+
     &            TimeBkgrd+TimeCheck+TimeSetup
      IF (FLogFile)
     &WRITE(NtLog,30) TimeSetup,TimeCheck,
     &            TimeBkgrd,TimeVisCon,TimeIsoCon,TimeWrite,
     &            TimeFin,TimeFin+TimeWrite+TimeIsoCon+TimeVisCon+
     &            TimeBkgrd+TimeCheck+TimeSetup
   30 FORMAT(/'    Resources used:'/
     &     6X,F8.1,' sec to read the input,'/
     &     6X,F8.1,' sec to triangulate,'/
     &     6X,F8.1,' sec to preserve  boundaries,'/
     &     6X,F8.1,' sec to modify the background grid,'/
     &     6X,F8.1,' sec to construct stretched points,'/
     &     6X,F8.1,' sec to construct isotropic points,'/
     &     6X,F8.1,' sec to write the output, thus'/
     &     6X,F8.1,' sec in total.')
      IF (IVerbose.GE.2) THEN
        WRITE (*,31) OpFlNm(1:KHiOpFlnm-1)
        DO NLevel = 2,MLevel
          CharLevel = CHAR(NLevel+ICharZero)
          WRITE (*,34) OpFlNm(1:KHiOpFlnm-1)//'.'//CharLevel
        END DO
        WRITE (*,35)
      END IF
      IF (FLogFile) THEN
        WRITE (NtLog,31) OpFlNm(1:KHiOpFlnm-1)
        DO NLevel = 2,MLevel
          CharLevel = CHAR(NLevel+ICharZero)
          WRITE (NtLog,34) OpFlNm(1:KHiOpFlnm-1)//'.'//CharLevel
        END DO
        WRITE (NtLog,35)
      END IF
   31 FORMAT (/'    Output written to:'/7X,A)
   34 FORMAT (7X,A)
   35 FORMAT (/)
C
      STOP
      END
