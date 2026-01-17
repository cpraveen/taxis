      SUBROUTINE READFIP (NtPts,NtLog,IVerbose,FLogFile,
     &                    MBnd,NmeBnd,NvNmBn,NdxLBnNd,
     &                    NmNghFNd,NmNghLNd,IBndType,
     &                    LmtNde,MLmt,NdxNotCon,LsNotCon,MNde,
     &                    XNode,YNode,ArcRel,DltZero,DltStar,NdxLArc,
     &                    Coord,DCoord,XFrst,YFrst,XLst,YLst,FChkBnd,
     &                    FBndTest,MinRes,MaxNode)
C
C Last update:
C 11Aug96; test for bounds of MaxNode.
C 19May96; initialize ndxnotcon(0).
C 12May96; read coordinates as doubles. Since points are not moved
C          anymore, we can keep all of the accuracy desired. 
C 16Sep95; fix bug in reading ANTICO information.
C 15Mar95;unify MNdeU counter, compare it properly to MBndNde.
C 7Mar95;explicit boundary loops for Coord quantities. Avoid the
C        ill-defined FORTRAN status on END conditons.
C 24Sep94;intro MinRes.
C 25Jul93,14:05; wake type boundaries have NmNghLNd = 0,
C                no connectivity test for type 3 (non-enforced) sets,
C                check for double use of boundary names.
C 5Dec93,13:40; intro FBndTest for IPOL.
C 18Nov93,16:50; create a pointer LmtNde for IBndType = 4 boundaries.
C 17Sep93,18:30; intro DltZero.
C 8Feb93,13:37; towards 4.0.
C
C     READFIP reads the input data for DELAUNDO from
C     a formatted .PTS file. Welcome to DOMINO software.
C
C Input:
C NpPts:    Number of the logical unit of the input file.
C NtLog:    Number of the logical unit of the log file.
C IVerbose: Level of user information.
C FLogFile: If .T., a logfile is written.
C Coord:    Storage for coordinates of at least 2*MaxBndNde.
C DCoord:   Storage for interpolation stations.
C X/YFrst:  Vector for overlap check of at least MaxBound.
C X/YLst:   Vector for overlap check of at least MaxBound.
C FChkBnd:  LOGICAL*1 vector of at least MaxBound.
C FBndTest: If .T., boundary connectivity and overlap testing is done.
C
C Output:
C IVerbose: Verbosity.
C FLogFile: .T. if a logfile shall be written
C MBnd:     Number of boundaries.
C NmeBnd:   Name of current boundary. Names may be arbitrarily
C           chosen between 1 and 100 and must not be ordered.
C NmNghFnd: Name of the boundary having as last node 
C           the first node of the current boundary.
C NmNghLnd: Name of the boundary having as first node
C           the last node of the current boundary.
C IBndType: Type of the boundary given:
C               1:  regular, enforced, in foreground triangulation.
C               2:  enforced, but non-frontal, in foreground.
C               3:  non-enforced by CHKBND, non-frontal in foreground.
C               4:  wake. frontal to both sides, enforced.
C               9:  in background.
C               >0: counterclockwise, domain to the left.
C               <0: clockwise, domain to the  right.
C LmtNde:   Vector of pointers to closed loops of boundary segments.
C MLmt:     Number of closed loops.
C NdxNotCon:Pointer to the boundaries that may not be connected to
C           this boundary in the background mesh.
C LsNotCon: Names of the boundaries that may not be connected to
C           this boundary in the background mesh. Note that on 
C           output LsNotCon contains the numbers of these boundaries.
C MNde:     Total number of nodes.
C XNode:    X-coordinate of the boundary node.
C YNode:    Y-coordinate of the boundary node.
C           Boundary nodes are contiguous in such a way that
C           proceeding to the following boundary node the
C           domain is to the left.
C ArcRel:   Relative arclength [0,1] for interpolation stations
C           of the thickness of the inner constantly stretched layer.
C DltZero:  Thickness of that layer.
C DltStar:  Thickness of the whole viscous layer.
C MinRes:   Minimum resolution of the boundary.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION FChkBnd(*),XFrst(*),YFrst(*),ArcRel(*),DltZero(*),
     &          XLst(*),YLst(*),
     &          XNode(*),YNode(*),NmeBnd(*),NvNmBn(-1:*),NdxLBnNd(0:*),
     &          NmNghFNd(*),NmNghLNd(*),IBndType(*),LmtNde(*),
     &          LsNotCon(*),NdxNotCon(0:*),
     &          NdxLArc(0:*),MinRes(*)
      CHARACTER CString*256,CharTxt*1
      DOUBLE PRECISION DCoord(*)
      DOUBLE PRECISION Coord(*)
C     STATIC NCo
C
C     Set default flags.
      FDNmeBnd = .TRUE.
      FDNmNghFNd = .TRUE.
      FDNmNghLNd = .TRUE.
      FDIBndType = .TRUE.
      FDMNdeU = .TRUE.
C
C     Create an index of the last node of the first segment, NdxLBnNd,
C     keeping in mind, that the first 4 nodes are the corners of
C     the convex hull and the first node of each segment is
C     considered to belong to the previous boundary segment.
      NvNmBn(0) = 0
      NdxLBnNd(0) = 4
      NdxLArc(0) = 0
      NdxNotCon(0) = 0
      MBnd = 0
      MNde = 4
      MStat = 0
      MCoord = 0
      MDCoord = 0
C
      IF (IVerbose.GE.3) THEN
        WRITE (*,'(/3X,A)')
     &  ' Reading boundary nodes from .pts file.'
      END IF
      IF (FLogFile) THEN
        WRITE (NtLog,'(/3X,A)')
     &  ' Reading boundary nodes from .pts file.'
      END IF
C
C     Open a scratch file for char to int/real conversion.
      OPEN (99,STATUS='SCRATCH',FORM='FORMATTED')
C
C     Read from .pts file.
      CharTxt = " "
      MCharTxt = 0
      FRequired = .FALSE.
      FNoCtr = .FALSE.
      FileEnd = .FALSE.
      DO WHILE (.NOT.FileEnd)
        READ (NtPts,'(A)',END=993) CString
        GO TO 994
  993   CString = 'ENDDAT'
C       Find the non-blank part of the string.
  994   KLo = 1
        DO WHILE (CString(KLo:KLo).EQ.' '.AND.KLo.LE.255)
          KLo = KLo+1
        END DO
C
        IF (CString(KLo:KLo).EQ.'%'.OR.
     &      CString(KLo:KLo).EQ.'!'.OR.
     &      CString(KLo:KLo).EQ.'?'.OR.
     &      KLo.EQ.256) THEN
C         Comment or blank line, ignore.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'NAMEBN') THEN
C         Name of this boundary.
          CALL READINTEGER (NtPts,CharTxt,MCharTxt,FNoCtr,FRequired,
     &                      FDNmeBnd,NmeBnd(NBnd))
          FIntNde = .FALSE.
          FBndNde = .FALSE.
          FAntiCo = .FALSE.
          FDltZer = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'NRBNDE'.OR.
     &           CString(KLo:KLo+5).EQ.'NRINDE') THEN
C         Number of boundary nodes in this segment.
          CALL READINTEGER (NtPts,CharTxt,MCharTxt,FNoCtr,FRequired,
     &                      FDMNdeU,MNdeU)
          FBndNde = .FALSE.
          FIntNde = .FALSE.
          FDltZer = .FALSE.
          FAntiCo = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'NFRSBN') THEN
C         Name of the adjacent segment at the first node.
          CALL READINTEGER (NtPts,CharTxt,MCharTxt,FNoCtr,FRequired,
     &                      FDNmNghFNd,NmNghFNd(NBnd))
          FBndNde = .FALSE.
          FIntNde = .FALSE.
          FDltZer = .FALSE.
          FAntiCo = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'NLSTBN') THEN
C         Name of the adjacent segment at the last node.
          CALL READINTEGER (NtPts,CharTxt,MCharTxt,FNoCtr,FRequired,
     &                      FDNmNghLNd,NmNghLNd(NBnd))
          FBndNde = .FALSE.
          FIntNde = .FALSE.
          FAntiCo = .FALSE.
          FDltZer = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'ITYPBN') THEN
C         Type of the boundary.
          CALL READINTEGER (NtPts,CharTxt,MCharTxt,FNoCtr,FRequired,
     &                      FDIBndType,IBndType(NBnd))
          FBndNde = .FALSE.
          FIntNde = .FALSE.
          FAntiCo = .FALSE.
          FDltZer = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'MINRES') THEN
C         Minimum resolution of the boundary segment.
          CALL READINTEGER (NtPts,CharTxt,MCharTxt,FNoCtr,FRequired,
     &                      FDIMinRes,MinRes(NBnd))
          FBndNde = .FALSE.
          FIntNde = .FALSE.
          FAntiCo = .FALSE.
          FDltZer = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'ANTICO') THEN
C         Start the list of the segments that may not be 
C         connected to this one.
          FAntiCo = .TRUE.
          FIntNde = .FALSE.
          FBndNde = .FALSE.
          FDltZer = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'BNDEXY') THEN
C         Start the list of the coordinates of the boundary nodes.
          FBndNde = .TRUE.
          FIntNde = .FALSE.
          FAntiCo = .FALSE.
          FDltZer = .FALSE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'DLTZER') THEN
C         Start the list of interpolation stations for DltZero.
          FBndNde = .FALSE.
          FIntNde = .FALSE.
          FAntiCo = .FALSE.
          FDltZer = .TRUE.
C
        ELSE IF (CString(KLo:KLo+5).EQ.'NEWBND'.OR.
     &           CString(KLo:KLo+5).EQ.'INDEXY'.OR.
     &           CString(KLo:KLo+5).EQ.'ENDDAT') THEN
C
          IF (CString(KLo:KLo+5).EQ.'ENDDAT') FileEnd = .TRUE.
C
          IF (MCoord.GT.0) THEN
C           End the previous boundary.
C
            IF (FDNmeBnd) THEN
C             This boundary has no name, give it a default. Default
C             will be the the number of the boundary, if not already
C             in use, or the first free name. 
C             Check whether the number NBnd is already used.
              FUsed = .FALSE.
              DO NNBnd = 1,MBnd-1
                IF (NmeBnd(NNBnd).EQ.NBnd) FUsed = .TRUE.
              END DO
              IF (.NOT.FUsed) THEN
C               NBnd is not used as a name. Use it as the name.
                NmeBnd(NBnd) = NBnd
              ELSE
C               NBnd is used as a name. Find the first free name.
                Found = .FALSE.
                NTest = 0
                DO WHILE (.NOT.Found)
                  NTest = NTest+1
                  FUsed = .FALSE.
                  DO NNBnd = 1,MBnd-1
                    IF (NmeBnd(NNBnd).EQ.NTest) FUsed = .TRUE.
                  END DO
                  IF (.NOT.FUsed) THEN
                    NmeBnd(NBnd) = NTest
                    Found = .TRUE.
                  END IF
                END DO
              END IF
C     
            ELSE
C             Check whether the given name is already used.
              FUsed = .FALSE.
              DO NNBnd = 1,MBnd-1
                IF (NmeBnd(NNBnd).EQ.NmeBnd(NBnd)) THEN
                  IF (FLogFile) WRITE (NtLog,17) NmeBnd(NBnd),NBnd,NNBnd
                  WRITE (*,17) NmeBnd(NBnd),NBnd,NNBnd
                  STOP
 17               FORMAT(
     &            ' FATAL: Boundary name',I3,' of boundary',I3/
     &               '        already assigned to boundary',I3,'.')
                END IF
              END DO
C     
            END IF
C     
            IF (IBndType(NBnd).EQ.4.OR.IBndType(NBnd).EQ.-4) THEN
C             This is a wake, per default doubly open ended.
              IF (FDNmNghFNd) NmNghFNd(NBnd) = 0
              IF (FDNmNghLNd) NmNghLNd(NBnd) = 0
C
            ELSE IF (IBndType(NBnd).EQ.3.OR.IBndType(NBnd).EQ.-3.OR.
     &               FIntNde) THEN
C             This is a non-enforced set of foreground nodes.
              IF (FDIBndType) IBndType(NBnd) = 3
              IF (FDNmNghFNd) NmNghFNd(NBnd) = 0
              IF (FDNmNghLNd) NmNghLNd(NBnd) = 0
C
            ELSE IF (IBndType(NBnd).EQ.1.OR.IBndType(NBnd).EQ.-1.OR.
     &               IBndType(NBnd).EQ.2.OR.IBndType(NBnd).EQ.-2) THEN
C             This is a enforced frontal/non-frontal boundary.
              IF (FDNmNghFNd) NmNghFNd(NBnd) = NmeBnd(NBnd)
              IF (FDNmNghLNd) NmNghLNd(NBnd) = NmeBnd(NBnd)
              IF (FDIBndType) IBndType(NBnd) = 1
C
            END IF
C
            IF (NmeBnd(NBnd).NE.0) NvNmBn(NmeBnd(NBnd)) = NBnd
            NdxNotCon(NBnd) = NdxNotCon(NBnd-1)+MNot

C           Reset check-flag.
            FChkBnd(NBnd) = .FALSE.
C
            MCoord = 2*INT(MCoord/2.)
            IF ((IBndType(NBnd).EQ.3.OR.IBndType(NBnd).EQ.4).AND.
     &          NmNghFNd(NBnd).EQ.0.OR.
     &          (IBndType(NBnd).EQ.-3.OR.IBndType(NBnd).EQ.-4).AND.
     &          NmNghLNd(NBnd).EQ.0.OR.
     &          IBndType(NBnd).EQ.9) THEN
C             This is a doubly open ended line of nodes with the domain
C             to both sides. Use first node.

C             Check bounds.
              IF ( MNde + MCoord/2 .GT. MaxNode ) THEN
                WRITE (*,19) MaxNode
                IF ( FLogFile ) WRITE (NtLog,19) MaxNode
 19             FORMAT ( ' FATAL: Only',I9,' nodes possible.',/,
     &                   '        recompile with larger MaxNode',
     &                   ' in nodes.cbl.' )
                STOP
              END IF
              DO NCoord = 2,MCoord,2
                NNde = MNde+NCoord/2
                XNode(NNde) = Coord(NCoord-1)
                YNode(NNde) = Coord(NCoord)
              END DO
              XFrst(NBnd) = Coord(1)
              YFrst(NBnd) = Coord(2)
              XLst(NBnd) = Coord(MCoord-1)
              YLst(NBnd) = Coord(MCoord)
              NdxLBnNd(NBnd) = NdxLBnNd(NBnd-1)+MCoord/2
              MNde = NdxLBnNd(NBnd)
C
            ELSE IF (IBndType(NBnd).GT.0) THEN
C             This is a boundary in a closed loop with the domain to
C             its left or a singly open ended line of nodes.
C             The first node is the last one of the previous
C             segment.
              IF ( MNde + MCoord/2 - 1 .GT. MaxNode ) THEN
                WRITE (*,19) MaxNode
                IF ( FLogFile ) WRITE (NtLog,19) MaxNode
                STOP
              END IF
              DO NCoord = 4,MCoord,2
                NNde = MNde+NCoord/2-1
                XNode(NNde) = Coord(NCoord-1)
                YNode(NNde) = Coord(NCoord)
              END DO
              XFrst(NBnd) = Coord(1)
              YFrst(NBnd) = Coord(2)
              XLst(NBnd) = Coord(MCoord-1)
              YLst(NBnd) = Coord(MCoord)
              NdxLBnNd(NBnd) = NdxLBnNd(NBnd-1)+MCoord/2-1
              MNde = NdxLBnNd(NBnd)
C
            ELSE IF (IBndType(NBnd).LT.0) THEN
C             This is a boundary in a closed loop with the domain to
C             its right or a singly open ended line of nodes.
C             The last node is the first one of the previous
C             segment.
              IF ( MNde + MCoord/2 - 1 .GT. MaxNode ) THEN
                WRITE (*,19) MaxNode
                IF ( FLogFile ) WRITE (NtLog,19) MaxNode
                STOP
              END IF
              DO NCoord = MCoord-2,2,-2
                NNde = MNde+(MCoord-NCoord)/2
                XNode(NNde) = Coord(NCoord-1)
                YNode(NNde) = Coord(NCoord)
              END DO
C             Switch the neighboring segments.
              NmeDummy = NmNghFNd(NBnd)
              NmNghFNd(NBnd) = NmNghLNd(NBnd)
              NmNghLNd(NBnd) = NmeDummy
              XFrst(NBnd) = Coord(MCoord-1)
              YFrst(NBnd) = Coord(MCoord)
              XLst(NBnd) = Coord(1)
              YLst(NBnd) = Coord(2)
              NdxLBnNd(NBnd) = NdxLBnNd(NBnd-1)+MCoord/2-1
              MNde = NdxLBnNd(NBnd)
C
            END IF
C
C           Interpolation stations in DCoord for DltZero.
            DO NCoord = 2,MDCoord,2
              IF (IBndType(NBnd).GT.0) THEN
C               Boundary given with the domain to the left.
                NStat = MStat+NCoord/2
              ELSE
C               Boundary given with the domain to the right. Invert.
                NStat = MStat+(MDCoord-NCoord)/2+1
              END IF
              ArcRel(NStat) = DCoord(NCoord-1)
              DltZero(NStat) = DCoord(NCoord)
C
              IF (DltZero(NStat).GE.DltStar) THEN
                WRITE (*,'((/2(A,I3),A/2(A,E13.5),A/))')
     &          ' FATAL: On boundary',NBnd,' named',NmeBnd(NBnd),',',
     &          '        DltZero =',DltZero(NStat),
     &          ' exceeds DltStar =',DltStar,'.'
                IF (FLogFile) THEN
                  WRITE (NtLog,'((/2(A,I3),A/2(A,E13.5),A/))')
     &            ' FATAL: On boundary',NBnd,' named',NmeBnd(NBnd),',',
     &            '        DltZero =',DltZero(NStat),
     &            ' exceeds DltStar =',DltStar,'.'
                END IF
                STOP
              END IF
                
            END DO
            NdxLArc(NBnd) = NdxLArc(NBnd-1)+MDCoord/2
            MStatPrev = MStat
            MStat = NdxLArc(NBnd)
C
C           Make sure there is a station at ArcRel = 0.
            IF (ArcRel(MStatPrev+1).GT.0.AND.MDCoord.GT.0) THEN
C             Apparently there isn't. Move all points one position
C             up and create a station at zero with the first value.
              DO NStat = MStat,MStatPrev+1,-1
                ArcRel(NStat+1) = ArcRel(NStat)
                DltZero(NStat+1) = DltZero(NStat)
              END DO
              MStat = MStat+1
              NdxLArc(NBnd) = MStat
              ArcRel(MStatPrev+1) = -1.
            END IF
C
C           Make sure there is a station at ArcRel = 1.
            IF (ArcRel(MStat).LT.1.AND.MDCoord.GT.0) THEN
C             Add a station at 1.
              MStat = MStat+1
              NdxLArc(NBnd) = MStat
              ArcRel(MStat) = 2.
              DltZero(MStat) = DltZero(MStat-1)
            END IF
C
C           Check whether the stations are properly ordered and
C           DltZero is always less than DltStar.
            DO NStat = MStatPrev+2,MStat
              IF (ArcRel(NStat).LT.ArcRel(NStat-1)) THEN

C                write (*,'(I3,2F10.7)') 
C     &                         (IStat,ArcRel(IStat),DltZero(IStat),
C     &                          IStat = MStatPrev+1,MStat)

                WRITE (*,'((/2(A,I3)/A,A/))')
     &          ' FATAL: On boundary',NBnd,' named',NmeBnd(NBnd),
     &          '        interpolation stations for DltZero not',
     &          '        properly ordered.'
                IF (FLogFile) THEN
                  WRITE (NtLog,'((/2(A,I3)/A,A/))')
     &            ' FATAL: On boundary',NBnd,' named',NmeBnd(NBnd),
     &            '        interpolation stations for DltZero not',
     &            '        properly ordered.'
                END IF
                STOP
              END IF
            END DO
C
            IF (IVerbose.GE.3) THEN
C             List the current segment.
              WRITE (*,12) MBnd,NmeBnd(MBnd),IBndType(MBnd),MCoord/2
              IF (NmNghFNd(NBnd).NE.0)
     &          WRITE (*,16) NmNghFNd(NBnd),XFrst(NBnd),YFrst(NBnd)
              IF (NmNghLNd(NBnd).NE.0)
     &          WRITE (*,16) NmNghLNd(NBnd),XLst(NBnd),YLst(NBnd)
              MNotCon = NdxNotCon(NBnd)-NdxNotCon(NBnd-1)
              IF (MNotCon.GT.0)
     &          WRITE (*,13) MNotCon,
     &            (LsNotCon(I),I=NdxNotCon(NBnd-1)+1,NdxNotCon(NBnd))
              IF (MDCoord.GT.0)
     &          WRITE (*,14) MDCoord/2
              IF (MinRes(NBnd).GT.2)
     &          WRITE (*,18) MinRes(NBnd)
            END IF
            IF (FLogFile) THEN
C             List the current segment.
              WRITE (NtLog,12) MBnd,NmeBnd(MBnd),IBndType(MBnd),MCoord/2
              IF (NmNghFNd(NBnd).NE.0)
     &          WRITE (NtLog,16) NmNghFNd(NBnd),XFrst(NBnd),YFrst(NBnd)
              IF (NmNghLNd(NBnd).NE.0)
     &          WRITE (NtLog,16)  NmNghLNd(NBnd),XLst(NBnd),YLst(NBnd)
              MNotCon = NdxNotCon(NBnd)-NdxNotCon(NBnd-1)
              IF (MNotCon.GT.0)
     &          WRITE (NtLog,13) MNotCon,
     &            (LsNotCon(I),I=NdxNotCon(NBnd-1)+1,NdxNotCon(NBnd))
              IF (MDCoord.GT.0)
     &          WRITE (NtLog,14) MDCoord/2
              IF (MinRes(NBnd).GT.2)
     &          WRITE (NtLog,18) MinRes(NBnd)
            END IF
            
 12         FORMAT (
     &      6X,' Boundary',I3,' named',I3,' type',I3,
     &         ' read with',I5,' vertices.')
 16         FORMAT (
     &      9X,' Connected to boundary named',I3,' at X,Y =',2E16.8)
 13         FORMAT (
     &      9X,' Must not be connected to',I3,
     &      ' boundaries named',(10I3))
 14         FORMAT (
     &      9X,I6,' interpolation stations for DltZero given.')
 18         FORMAT (
     &      9X,' Minimum coarsegrid resolution:',I3,' vertices.')
C
            MBndNde = MCoord/2
            IF (.NOT.FDMNdeU.AND.MBndNde.NE.MNdeU) THEN
              IF (FLogFile)
     &          WRITE (NtLog,15) MNdeU,MBndNde,MBnd,NmeBnd(MBnd)
              IF (IVerbose.GT.2)
     &          WRITE (*,15) MNdeU,MBndNde,MBnd,NmeBnd(MBnd)
            END IF
 15         FORMAT (
     &      9X,' WARNING:',I5,' nodes expected, but',I5,
     &         ' vertices found'/
     &      9X,'         for boundary',I3,' named',I3,'.')
C
          END IF
C
          IF (CString(KLo:KLo+5).EQ.'NEWBND') THEN
C           Open a new boundary.
            MBnd = MBnd+1
            NBnd = MBnd
            MCoord = 0
            MDCoord = 0
            MNot = 0
            FBndNde = .FALSE.
            FIntNde = .FALSE.
            FAntiCo = .FALSE.
            FDNmeBnd = .TRUE.
            FDNmNghFNd = .TRUE.
            FDNmNghLNd = .TRUE.
            FDIBndType = .TRUE.
            MinRes(NBnd) = 2
C
          ELSE IF (CString(KLo:KLo+5).EQ.'INDEXY') THEN
C           Start reading interior nodes.
            MBnd = MBnd+1
            NBnd = MBnd
            FDNmeBnd = .TRUE.
            FDIBndType = .FALSE.
            IBndType(NBnd) = 3
            FDNmNghFNd = .FALSE.
            NmNghFNd(NBnd) = 0
            FDNmNghLNd = .FALSE.
            NmNghLNd(NBnd) = 0
            FIntNde = .TRUE.
            MCoord = 0
            FBndNde = .FALSE.
            FAntiCo = .FALSE.
C
          END IF
C
        ELSE IF (FBndNde.OR.FIntNde) THEN
C         Extract node coordinates from the list.
          REWIND 99
          WRITE (99,'(A)') CString
          REWIND 99
C          READ (99,*,END=995) (Coord(NCo),NCo=MCoord+1,MCoord+999)
C  995     MCoord = NCo-1
C         SGI-Fortran doesn't deliver NCo properly at the end of the
C         loop. Do an explicit loop.
          DO NCo = 0,999
            REWIND 99
            READ (99,*,END=995) (Coord(NCoX),NCoX=MCoord+1,MCoord+NCo)
          END DO
  995     MCoord = MCoord+NCo-1
C
        ELSE IF (FDltZer) THEN
C         Extract interpolation stations for DltZero from the list.
          REWIND 99
          WRITE (99,'(A)') CString
          REWIND 99
C          READ (99,*,END=998) (DCoord(NCo),NCo=MDCoord+1,MDCoord+999)
C  998     MDCoord = NCo-1
          DO NCo = 1,999
            REWIND 99
            READ (99,*,END=998) 
     &                (DCoord(NCoX),NCoX=MDCoord+1,MDCoord+NCo)
          END DO
  998     MDCoord = MDCoord+NCo-1
C
        ELSE IF (FAntiCo) THEN
C         Extract anti connectivity from the list.
          REWIND 99
          WRITE (99,'(A)') CString
          REWIND 99
C          READ (99,*,END=997) (LsNotCon(NNot),
C     &                         NNot=NdxNotCon(NBnd-1)+MNot+1,
C     &                              NdxNotCon(NBnd-1)+MNot+999)
C  997     MNot = NNot-1-NdxNotCon(NBnd-1)

C         New loop structure that runs also on the SGI that handles
C         the error condition as a failure and resets the index. Try
C         over and over again to read without error and keep the last
C         index.
          DO NNot = 1,999
            REWIND 99
            READ (99,*,END=997) (LsNotCon(NNotX),
     &                           NNotX=NdxNotCon(NBnd-1)+MNot+1,
     &                                 NdxNotCon(NBnd-1)+MNot+NNot)
          END DO
  997     MNot = MNot+NNot-1
C
        ELSE
          WRITE (*,'(2A/)') 
     &    ' WARNING: Unknown Keyword in .pts file:',CString(KLo:KLo+5)
          IF (FLogFile) 
     &    WRITE (NtLog,'(2A/)') 
     &    ' WARNING: Unknown Keyword in .pts file:',CString(KLo:KLo+5)
C
        END IF
C
      END DO
C
C     Find the extension of the domain.
      XMax = XNode(5)
      XMin = XNode(5)
      YMax = YNode(5)
      YMin = YNode(5)
      DO Node = 6,NdxLBnNd(MBnd)
        XMax=MAX(XNode(Node),XMax)
        XMin=MIN(XNode(Node),XMin)
        YMax=MAX(YNode(Node),YMax)
        YMin=MIN(YNode(Node),YMin)
      END DO
      XYExt = MAX(XMax-XMin,YMax-YMin)
      XYDiff = XYExt*1.E-7
C
C     Convert boundary names to boundary numbers for anticonnectivity.
      DO KBnd = 1,NdxNotCon(MBnd)
        LsNotCon(KBnd) = NvNmBn(LsNotCon(KBnd))
      END DO
C
      IF (.NOT.FBndTest) RETURN
C
C     Check boundaries.
      DO NBnd = 1,MBnd
        IBndType(NBnd) = ABS(IBndType(NBnd))
        IF (IBndType(NBnd).EQ.1.OR.IBndType(NBnd).EQ.2) THEN
C         This is a standard frontal or non-frontal boundary.
          NghF = NvNmBn(NmNghFNd(NBnd))
          XD = ABS(XFrst(NBnd)-XLst(NghF))
          YD = ABS(YFrst(NBnd)-YLst(NghF))
          IF (XD.GT.XYDiff.OR.YD.GT.XYDiff) THEN
            WRITE (*,'(2(2(2(A,I3),A,2E13.5,A/)))')
     &      ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &      ' with X/Y',XFrst(NBnd),YFrst(NBnd),' and',
     &      '        boundary',NghF,' named',NmeBnd(NghF),
     &      ' with X/Y',XLst(NghF),YLst(NghF),
     &      ' don''t overlap!'
            IF (FLogFile) 
     &      WRITE (NtLog,'(2(2(2(A,I3),A,2E13.5,A/)))')
     &      ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &      ' with X/Y',XFrst(NBnd),YFrst(NBnd),' and',
     &      '        boundary',NghF,' named',NmeBnd(NghF),
     &      ' with X/Y',XLst(NghF),YLst(NghF),
     &      ' don''t overlap!'
            STOP
          END IF
          NghL = NvNmBn(NmNghLNd(NBnd))
          XD = ABS(XLst(NBnd)-XFrst(NghL))
          YD = ABS(YLst(NBnd)-YFrst(NghL))
          IF (XD.GT.XYDiff.OR.YD.GT.XYDiff) THEN
            WRITE (*,'(2(2(2(A,I3),A,2E13.5,A/)))')
     &      ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &      ' with X/Y',XLst(NBnd),YLst(NBnd),' and',
     &      '        boundary',NghL,' named',NmeBnd(NghL),
     &      ' with X/Y',XFrst(NghL),YFrst(NghL),
     &      ' don''t overlap!'
            IF (FLogFile) 
     &      WRITE (NtLog,'(2(2(2(A,I3),A,2E13.5,A/)))')
     &      ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &      ' with X/Y',XLst(NBnd),YLst(NBnd),' and',
     &      '        boundary',NghL,' named',NmeBnd(NghL),
     &      ' with X/Y',XFrst(NghL),YFrst(NghL),
     &      ' don''t overlap!'
            STOP
          END IF
C
        ELSE IF (IBndType(NBnd).EQ.4) THEN
C         This is a frontal wake-type boundary.
          NghF = NvNmBn(NmNghFNd(NBnd))
          NghFF = NvNmBn(NmNghFNd(NghF))
          IF (NghF.NE.0) THEN
C           This is a line that is connected to a boundary segment.
            IF (IBndType(NghF).NE.1.AND.IBndType(NghF).NE.2) THEN
              WRITE (*,'(2(3(A,I3),A/),2A)')
     &        ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &        ' type',IBndType(NBnd),' must not be connected to',
     &        '        boundary',NghF,' named',NmNghLNd(NghF),
     &        ' type',IBndType(NghF),'.',
     &        '        Only regular type 1/2 boundaries may be',
     &        ' connected to type 3/4 boundaries.'
              IF (FLogFile)
     &        WRITE (NtLog,'(2(3(A,I3),A/),2A)')
     &        ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &        ' type',IBndType(NBnd),' must not be connected to',
     &        '        boundary',NghF,' named',NmNghLNd(NghF),
     &        ' type',IBndType(NghF),'.',
     &        '        Only regular type 1/2 boundaries may be',
     &        ' connected to type 3/4 boundaries.'
              STOP
            END IF
            XD = ABS(XFrst(NBnd)-XLst(NghFF))
            YD = ABS(YFrst(NBnd)-YLst(NghFF))
            IF (XD.GT.XYDiff.OR.YD.GT.XYDiff) THEN
              WRITE (*,'(2(2(2(A,I3),A,2E13.5,A/)))')
     &        ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &        ' with X/Y',XFrst(NBnd),YFrst(NBnd),' and',
     &        '        boundary',NghF,' named',NmeBnd(NghF),
     &        ' with X/Y',XLst(NghFF),YLst(NghFF),
     &        ' don''t overlap!'
              IF (FLogFile)
     &        WRITE (NtLog,'(2(2(2(A,I3),A,2E13.5,A/)))')
     &        ' FATAL: Boundary',NBnd,' named',NmeBnd(NBnd),
     &        ' with X/Y',XFrst(NBnd),YFrst(NBnd),' and',
     &        '        boundary',NghF,' named',NmeBnd(NghF),
     &        ' with X/Y',XLst(NghFF),YLst(NghFF),
     &        ' don''t overlap!'
              STOP
            END IF
          END IF
C
        ELSE IF (IBndType(NBnd).EQ.9) THEN
C         This is a user-defined set of background nodes implicitely
C         defining spacing. Is doubly open ended.
C
        ELSE IF (IBndType(NBnd).EQ.3) THEN
C         This is a user-defined set of foreground nodes.
C
        ELSE
          IF (FLogFile) WRITE (NtLog,11)
     &                  IBndType(NBnd),NBnd,NmeBnd(NBnd)
          WRITE (*,11) IBndType(NBnd),NBnd,NmeBnd(NBnd)
 11       FORMAT (/
     &    ' FATAL: Boundary type',I3,' for boundary',I3,
     &    ' named',I3,' is illegal!')
          STOP
C
        END IF
C
C       Check Anti-Connectivity info.
        NNotCon = NdxNotCon(NBnd-1)
        DO WHILE (NNotCon.LT.NdxNotCon(NBnd))
          NNotCon = NNotCon+1
          NrNotCon = LsNotCon(NNotCon)
          NmNotCon = NmeBnd(NrNotCon)
          IF (NmNotCon.EQ.NmNghFNd(NBnd).OR.
     &        NmNotCon.EQ.NmNghFNd(NBnd)) THEN
C
            WRITE (*,'(2(2(A,I3)/),2A/A)')
     &      ' WARNING: Segments',NBnd,' named',NmeBnd(NBnd),
     &      '               and',NrNotCon,' named',NmNotCon,
     &      '          are linked and cannot be disconnected in the',
     &      ' background grid.',' AntiCo information ignored.'
            IF (FLogFile)
     &      WRITE (NtLog,'(2(2(A,I3)/),2A/A)')
     &      ' WARNING: Segments',NBnd,' named',NmeBnd(NBnd),
     &      '               and',NrNotCon,' named',NmNotCon,
     &      '          are linked and cannot be disconnected in the',
     &      ' background grid.',' AntiCo information ignored.'
C
C           Remove this entry and copy all remaining down.
            DO INotCon = NNotCon+1,NdxNotCon(MBnd)
              LsNotCon(INotCon-1) = LsNotCon(INotCon)
            END DO
            DO IBnd = NBnd,MBnd
              NdxNotCon(IBnd) = NdxNotCon(IBnd)-1
            END DO
C
C           Find the corresponding counter entry in NrNotCon and
C           remove that one as well.
            INotCon = NdxNotCon(NrNotCon-1)
            DO WHILE (INotCon.LT.NdxNotCon(NrNotCon))
              INotCon = INotCon+1
              IF (LsNotCon(INotCon).EQ.NBnd) THEN
C               Remove this entry in NrNotCon and copy the 
C               remaining down.
                DO IINotCon = INotCon+1,NdxNotCon(MBnd)
                  LsNotCon(IINotCon-1) = LsNotCon(IINotCon)
                END DO
                DO IBnd = NBnd,MBnd
                  NdxNotCon(IBnd) = NdxNotCon(IBnd)-1
                END DO
              END IF
            END DO
C
          END IF
C
C         Make sure NrNotCon carries NBnd as entry in LsNotCon.
C         Find the corresponding counter entry in NrNotCon.
          Found = .FALSE.
          DO INotCon = NdxNotCon(NrNotCon-1)+1,NdxNotCon(NrNotCon)
            IF (LsNotCon(INotCon).EQ.NBnd) Found = .TRUE.
          END DO
          IF (.NOT.Found) THEN
C           Add NBnd.
            DO IINotCon = NdxNotCon(MBnd),INotCon+1,-1
              LsNotCon(IINotCon+1) = LsNotCon(IINotCon)
            END DO
            DO IBnd = NBnd,MBnd
              NdxNotCon(IBnd) = NdxNotCon(IBnd)+1
            END DO
            LsNotCon(NdxNotCon(NrNotCon)) = NBnd
          END IF
C
        END DO
C
      END DO
C
C     Specify one boundary node on each element and count the
C     number of elements, correct information of adjacent elements.
      MLmt = 0
C
C     Pick a boundary segment.
      DO NBnd = 1,MBnd
C
        IF (.NOT.FChkBnd(NBnd).AND.IBndType(NBnd).LT.3) THEN
C         Segment hasn't been used so far.
          NCurrBnd = NBnd
          FChkBnd(NBnd) = .TRUE.
          NTrgtBnd = 0
C
C         Walk through its adjacent segments.
          Found = .FALSE.
          MBndChk = 1
          DO WHILE (.NOT.Found.AND.MBndChk.LE.MBnd+1)
C
            IF (NCurrBnd.EQ.NTrgtBnd.OR.NmNghLNd(NCurrbnd).EQ.0) THEN
C             This is the end of the row of boundary segments.
              IF (IBndType(NBnd).LT.3) THEN
C               Increase sum of elements.
                MLmt = MLmt+1
C               Set pointer to second point of first segment of the row.
                LmtNde(MLmt) = NdxLBnNd(NBnd-1)+2
              END IF
              Found = .TRUE.
C
            ELSE
C             Take the following segment.
              MBndChk = MBndChk+1
              NCurrBnd = NvNmBn(NmNghLNd(NCurrBnd))
              FChkBnd(NCurrBnd) = .TRUE.
C
C             If not happened yet initialize the target segment.
              IF (NTrgtBnd.EQ.0) NTrgtBnd = NBnd
C
            END IF
C
          END DO
C
          IF (MBndChk.GT.MBnd+1) THEN
            WRITE (*,'(2A,I4,A,I4)') 
     &                  ' FATAL: The loop of boundary segments',
     &                  ' around boundary',NBnd,' named',NmeBnd(NBnd),
     &                  '        seems not to be closed.',
     &                  ' Sorry, but do try again.'
            STOP
          END IF
C
C
        ELSE IF (IBndType(NBnd).EQ.4) THEN
C         This is a wake-type boundary. It is connected at the
C         beginning to a solid boundary and open at the end or
C         open at both ends. Set the pointer to the last node.
          FChkBnd(NBnd) = .TRUE.
          MLmt = MLmt+1
          LmtNde(MLmt) = NdxLBnNd(NBnd)
C
        END IF
      END DO
C
C     Erase clockwise boundary types.
      DO NBnd = 1,MBnd
        IBndType(NBnd) = ABS(IBndType(NBnd))
      END DO
C
      CLOSE (99)
      RETURN
      END
