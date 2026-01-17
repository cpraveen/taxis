      PROGRAM IPOL
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      PARAMETER (MaxNode=5000,MaxBound=20,MaxBndNde=MaxNode,
     &           MaxInt=50, MaxPolyCoeff=4)
      DOUBLE PRECISION THere,
     &     XBegin,YBegin,XFinish,YFinish,XCenter,YCenter,Radius,
     &     PolyCoeff(0:MaxPolyCoeff),AlphaBegin,AlphaFinish,
     &     PiBegin, PiFinish,SinePower,
     &     XTrans,YTrans,Alpha,CosAlpha,SinAlpha,Scale,
     &     XSpline(MaxNode),YSpline(MaxNode), TSpline(MaxNode),
     &     TNew(MaxNode), XNew(MaxNode), YNew(MaxNode),
     &     XNode(MaxNode),YNode(MaxNode),
     &     XFrst(MaxBound),YFrst(MaxBound),XLst(MaxBound),
     &     YLst(MaxBound),Coord(2*MaxBndNde),DCoord(MaxBndNde)
      REAL ArcRel(MaxInt), DltZero(MaxInt),
     &     ArcRelNew(MaxInt), DltZeroNew(MaxInt),
     &     TStation, RhoStation,
     &     TInt(0:MaxInt), TIntOrig(0:MaxInt),
     &     RhoInt(0:MaxInt), RhoIntOrig(0:MaxInt)
      CHARACTER Version*6, LastUpdate*20, Char*6,CtrFlNm*80,
     &     OpFlNm*80,NpFlNm*80,Curve*80,String*80
      CHARACTER CString*256
      INTEGER DefXYNewBegin, DefXYTrans, DefXYNewFinish,
     &        DefAlpha, DefScale, DefXYRotOrigin
      INTEGER DefXYBegin, DefXYFinish, DefXYCenter,
     &     DefAlphaBeginFinish,DefPiBeginFinish, DefSinePower,
     &     DefRadius,  DefPolyCoeff(0:MaxPolyCoeff),
     &     DefXYThird, SplineBufferEmpty, SplineBufferChanged,
     &     NmeBnd(MaxBound),NvNmBn(-1:MaxBound+2),
     &     NmNghFNd(MaxBound),NmNghLNd(MaxBound),
     &     IBndType(MaxBound),NdxLBnNd(0:MaxBound),
     &     LmtNde(MaxBound),NdxLarc(0:MaxBound),NdxNotCon(0:MaxBound),
     &     LsNotConFile(0:MaxBound),
     &     MinRes(MaxBound), NmeNotCon(MaxBound),
     &     MNdeCrv, MNdePts
      LOGICAL*1 FileEnd,FChkBnd(MaxBound)

      Version = '3.2'
      LastUpdate = '22 Mar 1998'
      WRITE (*,'(A,A3,A,A11,A)')
     &     ' This is IPOL, version ', Version ,' of ', LastUpdate,
     &     ', welcome to Domino Software.'

      IF( IARGC() .gt. 0) THEN
        CALL GETARG (1,CtrFlNm)

C       Get input from file.
        NtIn = 1
        OPEN(NtIn,FILE=CtrFlNm,FORM='FORMATTED',STATUS='OLD')
        REWIND (NtIn)
      ELSE
        
        NtIn = 0
        DO WHILE ( NtIn .EQ. 0 )
          WRITE (*,'(3X,2A)')
     &         ' Type HELPME for help, ? to be prompted,',
     &         ' a .ictr input file, or EXITUS. '
          READ (*,'(A)') CtrFlNm
          CALL FILENMLEN ( CtrFlNm, 80 )
          IF ( CtrFlNm(1:1) .EQ. '?' ) THEN
C           Get input from tty.
            NtIn = 5
          ELSE IF ( CtrFlNm(1:4) .EQ. 'HELP' ) THEN
            CALL HELPME
            WRITE (*,'(/)')
          ELSE IF ( CtrFlNm(1:4) .EQ. 'EXIT' ) THEN
            STOP
          ELSE
C           Get input from file.
            NtIn = 1
            OPEN(NtIn,FILE=CtrFlNm,FORM='FORMATTED',STATUS='OLD')
            REWIND (NtIn)
          END IF
        END DO

      END IF
      
      NtOp = 2
      NtP = 3
C     Open a scratch file for char to int/real conversion.
      OPEN (99,STATUS='SCRATCH',FORM='FORMATTED')
      FList = .FALSE.
      IVerbose = 3
      RhoScaleInt = 1.
      RhoScaleEnd = 1.
      SplineBufferEmpty = 1
      SplineBufferChanged = 0
C
      CALL GETDEFAULTS ( XBegin, YBegin, XFinish, YFinish,
     &                   XCenter, YCenter, Radius,
     &                   AlphaBegin, AlphaFinish,
     &                   PiBegin, PiFinish, SinePower,
     &                   PolyCoeff, MNdeCrv, MNdePts, RhoIntOrig,
     &                   MIter, LvlBlend, IClosed, KNdeCF, KNdeCL,
     &                   NmeCurvNew, NghFNew, NghLNew, IBTypeNew,
     &                   MNotCon )
      
      CALL MAPDEFAULTS ( DefXYNewBegin, DefXYNewFinish, 
     &                   DefXYTrans, ITrans,
     &                   DefAlpha, DefXYRotOrigin, IRot,
     &                   DefScale, IScale, IReflect )
      
      CALL CRVDEFAULTS ( DefXYBegin, DefXYFinish, DefXYThird,
     &                   DefXYCenter, DefAlphaBeginFinish,
     &                   DefPiBeginFinish, DefSinePower,
     &                   DefRadius, Defpolycoeff, MaxPolyCoeff )

C
      Char = '?'
      FileEnd = .FALSE.
      DO WHILE ( Char .NE. 'ENDDAT' )
        CALL READKEY ( NtIn, Char, FileEnd )
C
        IF (Char.EQ.'ENDDAT') THEN
C
        ELSE IF ( Char.EQ.'HELPME' ) THEN
          CALL HELPME
          
        ELSE IF (Char.EQ.'SCREEN') THEN
          READ (NtIn,'(A)') String

C         Strip trailing blanks.
          KPos = 80
          DO WHILE ( String(KPos:KPos) .EQ. ' ' .AND. KPos.GT.1)
            KPos = KPos-1
          END DO
          WRITE (*,'(A)') String(1:KPos)

        ELSE IF (Char.EQ.'LISTON') THEN
          FList = .TRUE.
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A)')
     &            ' LISTON: Begin listing interpolated vertices. '
C
        ELSE IF (Char.EQ.'LISTOF') THEN
          FList = .FALSE.
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A)')
     &            ' LISTOF: Stop listing interpolated vertices. '
C
        ELSE IF (Char.EQ.'VERBOS') THEN
          READ (NtIn,*) IVerbose
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I5)')
     &            ' VERBOS: Setting verbosity to ',IVerbose
C
        ELSE IF (Char.EQ.'RESETA') THEN
          CALL GETDEFAULTS ( XBegin, YBegin, XFinish, YFinish,
     &         XCenter, YCenter, Radius,
     &         AlphaBegin, AlphaFinish,
     &         PiBegin, PiFinish, SinePower,
     &         PolyCoeff, MNdeCrv, MNdePts, RhoIntOrig,
     &         MIter, LvlBlend, IClosed, KNdeCF, KNdeCL,
     &         NmeCurvNew, NghFNew, NghLNew, IBTypeNew,
     &         MNotCon )
          
          CALL MAPDEFAULTS ( DefXYNewBegin, DefXYNewFinish, 
     &         DefXYTrans, ITrans,
     &         DefAlpha, DefXYRotOrigin, IRot,
     &         DefScale, IScale, IReflect )
          
          CALL CRVDEFAULTS ( DefXYBegin, DefXYFinish, DefXYThird,
     &         DefXYCenter, DefAlphaBeginFinish,
     &         DefPiBeginFinish, DefSinePower,
     &         DefRadius, Defpolycoeff, MaxPolyCoeff )
          
        ELSE IF (Char.EQ.'SCALEI') THEN
          READ (NtIn,*) RhoScaleInt
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F9.4)')
     &            ' SCALEI: Scaling interior spacing with',
     &            RhoScaleInt
C
        ELSE IF (Char.EQ.'SCALEE') THEN
          READ (NtIn,*) RhoScaleEnd
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F9.4)')
     &            ' SACLEE: Scaling spacing at endpoints with',
     &            RhoScaleEnd
C
        ELSE IF (Char(1:1).EQ.'!'.OR.Char(1:1).EQ.'%'.OR.
     &           Char .EQ. 'IGNORE') THEN
C
        ELSE IF (Char.EQ.'OUTFIL') THEN
C         Name of the output .pts file.
          READ ( NtIn, '(A)' ) OpFlNm
          CALL FILENMLEN ( OpFlNm, 80 )
          CLOSE ( NtOp )
          OPEN ( NtOp, FILE=OpFlNm, FORM='FORMATTED', STATUS='UNKNOWN')
          REWIND ( NtOp )
          IF ( IVerbose .GT. 2 ) 
     &      WRITE (*,'(A/3X,A)')
     &            ' OUTFIL: New .pts output file:',OpFlNm

        ELSE IF (Char.EQ.'NEWFIL'.OR.Char.EQ.'INFILE') THEN
C         Open and read new file
          READ ( NtIn, '(A)' ) NpFlNm
          CALL FILENMLEN ( NpFlNm, 80 )
          CLOSE( NtP )
          OPEN( NtP, FILE=NpFlNm, STATUS='OLD', FORM='FORMATTED')
          REWIND ( NtP )
          IF ( IVerbose .GT. 2 ) 
     &      WRITE (*,'(A/3X,A)')
     &            ' NEWFIL: New .pts input file:',NpFlNm
C
          FLogFile = .FALSE.
          NtLog = 7
          DltStar = 1.E25
          FBndTest = .FALSE.
          DO NBnd = -1,MaxBound+2
            NvNmBn(NBnd) = 0
          END DO
          CALL READFIP (NtP,NtLog,IVerbose,FLogFile,
     &                  MBnd,NmeBnd,NvNmBn,NdxLBnNd,
     &                  NmNghFNd,NmNghLNd,IBndType,
     &                  LmtNde,MLmt,NdxNotCon,LsNotConFile,
     &                  MNdeCrvFile,XNode,YNode,
     &                  ArcRel,DltZero,DltStar,NdxLArc,
     &                  Coord,DCoord,XFrst,YFrst,XLst,YLst,FChkBnd,
     &                  FBndTest,MinRes,MaxNode)
C
        ELSE IF (Char.EQ.'FILCRV') THEN
C         Find the curve on the current file.
          READ (NtIn,*) NmeCurv
          NBnd = NvNmBn(NmeCurv)

C         Transfer all segment attributes into the new buffer.
C         Overwriting with .ictr options must happen after loading
C         of the curve.
          NmeCurvNew = NmeCurv
          NghFNew = NmNghFNd(NBnd)
          NghLNew = NmNghLNd(NBnd)
          IBTypeNew = IBndType(NBnd)
          Ndx1 = NdxNotCon(NBnd-1)+1
          MNotCon = NdxNotCon(NBnd) - Ndx1 + 1
          DO INotCon = 1,MNotCon
            NmeNotCon(INotCon) = NmeBnd(LsNotConFile(Ndx1+INotCon))
          END DO
          Ndx1 = NdxLArc(NBnd-1)+1
          MArc = NdxLArc(NBnd) - Ndx1 + 1
          DO IArc = 1,MArc
            ArcRelNew(IArc) = ArcRel(Ndx1+IArc)
            DltZeroNew(IArc) = DltZero(Ndx1+IArc)
          END DO
          MinResNew = MinRes(NBnd)
C
C         Transfer nodes KNdeFirst through KNdeLast of this
C         boundary to XYSpline. Note that the first node of this
C         segment is stored in XYFrst(NBnd). 
          NdxLo = NdxLBnNd(NBnd-1)
          NdxHi = NdxLBnNd(NBnd)
          MNdeBnd = NdxHi - NdxLo + 1
C         Index of the first and last points relative to this segment,
C         i.e. first node on this segment, XYFirst, is 1.
          KNdeFirst = MAX(1,KNdeCF)
          KNdeLast = MIN(MNdeBnd,KNdeCL)
          MNdeSpline = KNdeLast - KNdeFirst +1

          IF ( IVerbose .GT. 2 )
     &      WRITE (*,'(A/I3,2(A,I5),A)')
     &            ' FILCRV: working on curve named',NmeCurv,
     &            ' from node',KNdeFirst,
     &            ' to node',KNdeLast,'.'
          
          IF (NBnd.GT.MBnd.OR.NBnd.LE.0) THEN
            WRITE (*,'(/2(A,I4))') ' FATAL: Boundary named',NmeCurv,
     &                             ' does not exist.'
            STOP
          ELSE IF (KNdeCF.GE.MIN(KNdeCL,MNdeBnd)) THEN
            WRITE (*,'(/2(A,I4))') ' FATAL: First node',KNdeCF,
     &                             ' improperly assigned.'
            STOP
          END IF

          IF ( KNdeFirst .EQ. 1 ) THEN
C           Note that the first point of this boundary is the
C           last point of NmNghFNd and is not listed.
            XSpline(1) = XFrst(NBnd)
            YSpline(1) = YFrst(NBnd)
          ELSE
            XSpline(1) = XNode(NdxLo+KNdeFirst-1)
            YSpline(1) = YNode(NdxLo+KNdeFirst-1)
          END IF
          DO NNde = KNdeFirst+1, MNdeBnd-1
            XSpline(NNde-KNdeFirst+1) = XNode(NdxLo+NNde-1)
            YSpline(NNde-KNdeFirst+1) = YNode(NdxLo+NNde-1)
          END DO
          XSpline(MNdeBnd-KNdeFirst+1) = XLst(NBnd)
          YSpline(MNdeBnd-KNdeFirst+1) = YLst(NBnd)

          SplineBufferEmpty = 0
          SplineBufferChanged = 1
C
        ELSE IF (Char.EQ.'CALCRV') THEN
C         Take an analytic curve.
          READ ( NtIn, '(A)' ) Curve
          IF ( IVerbose .GT. 2 ) 
     &       WRITE (*,'(A/3X,A)')
     &             ' CALCRV: new .pts input curve type:',Curve

C         Ignore leading blanks.
          KPos = 1
          DO WHILE ( Curve(KPos:KPos) .EQ. ' ' .AND. KPos.LT.10)
            KPos = KPos+1
          END DO
C
          IF ( Curve(KPos:KPos+3) .EQ. 'line' .OR.
     &         Curve(KPos:KPos+3) .EQ. 'LINE' ) THEN
            CALL LINE ( XBegin, YBegin, XFinish, YFinish,
     &                  MNdeCrv, MNdeSpline, XSpline(1), YSpline(1) )
            SplineBufferEmpty = 0
            SplineBufferChanged = 1
C
          ELSE IF ( Curve(KPos:KPos+5) .EQ. 'circle' .OR.
     &              Curve(KPos:KPos+5) .EQ. 'CIRCLE' ) THEN
            CALL CIRCLE ( XCenter, YCenter, DefXYCenter,
     &                    AlphaBegin, AlphaFinish, DefAlphaBeginFinish,
     &                    PiBegin, PiFinish, DefPiBeginFinish,
     &                    Radius, DefRadius,
     &                    XBegin, YBegin, DefXYBegin,
     &                    XFinish, YFinish, DefXYFinish,
     &                    XThird, YThird, DefXYThird,
     &                    MNdeCrv, MNdeSpline, XSPline(1), YSpline(1) )
            SplineBufferEmpty = 0
            SplineBufferChanged = 1

          ELSE IF ( Curve(KPos:KPos+6) .EQ. 'polynom' .OR.
     &              Curve(KPos:KPos+6) .EQ. 'POLYNOM' ) THEN
            CALL POLYNOM ( XBegin, XFinish, PolyCoeff,
     &                     MNdeCrv, MNdeSpline,
     &                     XSpline(1), YSpline(1) )          
            SplineBufferEmpty = 0
            SplineBufferChanged = 1
C
          ELSE IF ( Curve(KPos:KPos+3) .EQ. 'sine' .OR.
     &              Curve(KPos:KPos+3) .EQ. 'SINE' ) THEN
            CALL SINE ( AlphaBegin, AlphaFinish, DefAlphaBeginFinish,
     &                  PiBegin, PiFinish, DefPiBeginFinish,
     &                  Radius, DefRadius,
     &                  SinePower, DefSinePower,
     &                  MNdeCrv, MNdeSpline, XSPline(1), YSpline(1) )
            SplineBufferEmpty = 0
            SplineBufferChanged = 1
C
C
          ELSE IF ( Curve(KPos:KPos+3) .EQ. 'naca' .OR.
     &              Curve(KPos:KPos+3) .EQ. 'NACA' ) THEN
            CALL NACA ( MNdeCrv, Curve(KPos+4:80),
     &                  MNdeSpline, XSPline(1), YSpline(1) )
            SplineBufferEmpty = 0
            SplineBufferChanged = 1
C
          ELSE
            WRITE (*,'(2A)')
     &            ' FATAL: unknown curve type:', Curve
            STOP
            
          END IF
C
        ELSE IF ( Char .EQ. 'MPTCRV' ) THEN
          READ ( NtIn, * ) MNdePts
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I5,A)')
     &            ' MPTCUR: Number of points on next curves:',
     &            MNdePts,' points.'
          
        ELSE IF ( Char .EQ. 'MINRES' ) THEN
          READ ( NtIn, * ) MinResNew
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I5,A)')
     &            ' MINRES: carry MINRES value to .pts file:',MinResNew
C
        ELSE IF ( Char .EQ. 'XYBEGI' ) THEN
          READ ( NtIn, * ) XBegin, YBegin
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.7)')
     &            ' XYBEGI: beginning of analytic curves:',
     &            XBegin, YBegin
          DefXYBegin = 0
C         
        ELSE IF ( Char .EQ. 'XYFINI' ) THEN
          READ ( NtIn, * ) XFinish, YFinish
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.7)')
     &            ' XYFINI: end of analytic curves:',
     &            XFinish, YFinish         
          DefXYFinish = 0
          
        ELSE IF ( Char .EQ. 'XYTHIR' ) THEN
          READ ( NtIn, * ) XThird, YThird
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.7)')
     &            ' XYTHIR: intermediate point of analytic curves:',
     &            XThird, YThird
          DefXYThird = 0
C         
        ELSE IF ( Char .EQ. 'XYCENT' ) THEN
          READ ( NtIn, * ) XCenter, YCenter
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.7)')
     &            ' XYCENT: center of analytic curves:',
     &            XCenter, YCenter
          DefXYCenter = 0
C         
        ELSE IF ( Char .EQ. 'ALFCRV' ) THEN
          READ ( NtIn, * ) AlphaBegin, AlphaFinish
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.7)')
     &        ' ALFCRV: beginning/end in degrees of analytic curves:',
     &        AlphaBegin, AlphaFinish
          DefAlphaBeginFinish = 0
C         
        ELSE IF ( Char .EQ. 'PIECRV' ) THEN
          READ ( NtIn, * ) PiBegin, PiFinish
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.7)')
     &    ' PIECRV: beginning/end in units of pi of analytic curves:',
     &        PiBegin, PiFinish
          DefPiBeginFinish = 0

        ELSE IF ( Char .EQ. 'RADIUS' ) THEN
          READ ( NtIn, * ) Radius
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)')
     &            ' RADIUS: New Radius value:',Radius
          DefRadius = 0
          
        ELSE IF ( Char .EQ. 'SINPWR' ) THEN
          READ ( NtIn, * ) SinePower
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)')
     &            ' SINPWR: New exponent for a sine curve:',
     &            SinePower
          DefSinePower = 0
C         
        ELSE IF ( Char .EQ. 'POLYN0' ) THEN
          READ ( NtIn, * ) PolyCoeff(0)
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)') 
     &            ' POLYN0: New Polynomial coefficient 0:',
     &            PolyCoeff(0)
          DefPolyCoeff(0) = 0
          
        ELSE IF ( Char .EQ. 'POLYN1' ) THEN
          READ ( NtIn, * ) PolyCoeff(1)
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)')  
     &            ' POLYN1: New Polynomial coefficient 1:',
     &            PolyCoeff(1)
          DefPolyCoeff(1) = 1
C         
        ELSE IF ( Char .EQ. 'POLYN2' ) THEN
          READ ( NtIn, * ) PolyCoeff(2)
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)')  
     &            ' POLYN2: New Polynomial coefficient 2:',
     &            PolyCoeff(2)
          DefPolyCoeff(2) = 2
C         
        ELSE IF ( Char .EQ. 'POLYN3' ) THEN
          READ ( NtIn, * ) PolyCoeff(3)
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)')  
     &            ' POLYN3: New Polynomial coefficient 3:',
     &            PolyCoeff(3)
          DefPolyCoeff(3) = 3
C         
        ELSE IF ( Char .EQ. 'POLYN4' ) THEN
          READ ( NtIn, * ) PolyCoeff(4)
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.7)')  
     &            ' POLYN4: New Polynomial coefficient 4:',
     &            PolyCoeff(4)
          DefPolyCoeff(4) = 4
C
        ELSE IF ( Char .EQ. 'CLOSED' ) THEN
C         Interpolate closed curves from now on.
          READ (NtIn,*) IClosed
          IF ( IClosed .NE. 1 ) THEN
            WRITE (*,'(A,I3,A)')
     &           ' CLOSED: illegal value:',IClosed,' set to 0.'
            IClosed = 0
          ELSE IF ( IVerbose .GT. 3 ) THEN
            WRITE (*,'(A,I3)')  
     &            ' CLOSED: interpolating on a closed curve:',IClosed
            END IF
          
        ELSE IF ( Char .EQ. 'LVLBLE' ) THEN
C         Interpolate on a different level
          READ ( NtIn, * ) LvlBlend
          IF ( LvlBlend .LT. 0 .OR. LvlBlend .GT. 3 ) THEN
            WRITE (*,'(A,I3,A)')
     &           ' STRAIT: illegal value:',LvlBlend,' set to 2.'
            LvlBlend = 2
          ELSE IF ( IVerbose .GT. 3 ) THEN
            WRITE (*,'(A,I3)')  
     &            ' STRAIT: Interpolate with order:',LvlBlend
          END IF
C         
        ELSE IF ( Char .EQ. 'SPCCLR' ) THEN
C         Remove all spacing interpolation station.
          MInt = 0
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(2A)')  
     &            ' SPCCLR: remove all spacing interpolation',
     &            ' stations.'
C         
        ELSE IF ( Char .EQ. 'SPCING' ) THEN
C         Add a new spacing interpolation station.
          READ ( NtIn, * ) TStation, RhoStation
          CALL ADDSTATION ( MInt, MaxInt, TIntOrig, RhoIntOrig,
     &                      TStation, RhoStation, IVerbose )
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.6)')  
     &            ' SPCING: add a spacing station at arclength/value:',
     &            TStation,RhoStation
C
        ELSE IF ( Char .EQ. 'DLTCLR' ) THEN
C         Remove all DltZero interpolation station.
          MDltZero = 0
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(2A)')  
     &            ' DLTCLR: remove all DLTZER interpolation',
     &            ' stations.'
C         
        ELSE IF ( Char .EQ. 'DLTZER' ) THEN
C         Add a new DltZero interpolation station.
          READ ( NtIn, * ) DltZeroStation, ArcRelStation
          CALL ADDSTATION ( MDltZero, MaxInt, ArcRelNew, DltZeroNew,
     &                      ArcRelStation, DltZeroStation, IVerbose )
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.6)')  
     &            ' DLTZER: add a DLTZER station at arclength/value:',
     &            DltZeroStation, ArcRelStation
C
        ELSE IF ( Char .EQ. 'MRELAX' ) THEN
C         Number of relaxations.
          READ ( NtIn, * ) MIter
          IF ( IVerbose .GT. 2 ) 
     &      WRITE (*,'(2A/I6)')
     &            ' MRELAX: Number of relaxation sweeps on next',
     &            ' curve:',MIter
C         
        ELSE IF (Char.EQ.'NDEFIL') THEN
C         First and last nodes of a file curve to work on.
          READ (NtIn,*) KNdeCF, KNdeCL
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(2A/2I5)') ' NDEFIL: Working on discrete curves',
     &                            ' from, to:', KNdeCF, KNdeCL
C
        ELSE IF (Char.EQ.'OUTNAM') THEN
C         New name for the output curve.
          READ (NtIn,*) NmeCurvNew
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I3)')
     &            ' OUTNAM: New output name for next curve:',
     &            NmeCurvNew
C           
        ELSE IF (Char.EQ.'OUTNGF') THEN
C         New first neighbor.
          READ (NtIn,*) NghFNew
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I3)')
     &            ' OUTNGF: New first neighbor for next curve:',
     &            NghFNew
C           
        ELSE IF (Char.EQ.'OUTNGL') THEN
C         New first neighbor.
          READ (NtIn,*) NghLNew
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I3)')
     &            ' OUTNGL: New last neighbor for next curve:',
     &            NghLNew
C         
        ELSE IF (Char.EQ.'OUTTYP') THEN
C         New first neighbor.
          READ (NtIn,*) IBTypeNew
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I3)')
     &            ' OUTTYP: New boundary type for next curve:',
     &            IBTypeNew
C
        ELSE IF (Char.EQ.'NOTCON') THEN
C         New illicit liasion.
          READ (NtIn,*) CString
          REWIND 99
          WRITE (99,'(A)') CString
          REWIND 99
          DO NEle = 0,999
            REWIND 99
            READ (99,*, END=101) (NmeNotCon(NLine),NLine=1,10000)
          END DO
 101      MNotCon = NEle-1
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/10I3)')
     &            ' NOTCON: Prohibit liasion for next curve to:',
     &            (NmeNotCon(INotCon),INotCon=1,MNotCon)
C
        ELSE IF (Char.EQ.'NEWBEG') THEN
C         .
          READ (NtIn,*) XNewBegin, YNewBegin
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.6)')
     &            ' NEWBEG: new beginning of mapped curve',
     &            XNewBegin, YNewBegin
          DefXYNewBegin = 0
C
        ELSE IF (Char.EQ.'NEWEND') THEN
C         .
          READ (NtIn,*) XNewFinish, YNewFinish
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.6)')
     &            ' NEWEND: new end of mapped curve',
     &            XNewFinish,YNewFinish
          DefXYNewFinish = 0
C
        ELSE IF (Char.EQ.'TRNVEC') THEN
C         .
          READ (NtIn,*) XTrans, YTrans
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.6)')
     &            ' TRNVEC: new translation vector',
     &            XTrans,YTrans
          DefXYTrans = 0
C         
        ELSE IF ( Char .EQ. 'ALFROT' ) THEN
C         Rotation angle in degrees.
          READ (NtIn,*) Alpha
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.6)')
     &            ' ALFROT, rotation angle for transformation:',Alpha
          CosAlpha = COS( Alpha )
          SinAlpha = SIN( Alpha )
          DefAlpha = 0
          IRot = 1
C         
        ELSE IF ( Char .EQ. 'ROTORG' ) THEN
C         Origin of the rotation.
          READ (NtIn,*) XRotOrigin, YRotOrigin
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/2F12.6)')
     &            ' ROTORG: rotation origin for transformation:',
     &            XRotOrigin, YRotOrigin
          DefXYRotOrigin = 0
C         
        ELSE IF ( Char .EQ. 'SCALEC' ) THEN
C         Scaling of the curve.
          READ (NtIn,*) Scale
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/F12.6)') ' SCALEC, curve scaling:',Scale
          DefScale = 0
          IScale = 1
C         
        ELSE IF ( Char .EQ. 'REFLEC' ) THEN
C         Reflect the curve around y=0 if REFLEC=1.
          READ (NtIn,*) IReflect
          IF ( IReflect .NE. 1 ) IReflect = 0
          IF ( IVerbose .GT. 3 ) 
     &      WRITE (*,'(A/I3)')
     &            ' REFLEC, reflect curve around y=0:',IReflect
C         
        ELSE IF ( Char .EQ. 'TRNSLT' .AND.
     &            SplineBufferEmpty .EQ. 1 ) THEN
          WRITE (*,'(A)')
     &          ' WARNING: Spline Buffer is empty, no action taken.'
C         
        ELSE IF ( Char .EQ. 'TRNSLT' ) THEN
          IF ( IVerbose .GT. 2 )
     &      WRITE (*,'(A)')
     &            ' TRNSLT: applying transformation to spline buffer'
          
C         Calculate the mapping parameters.
          CALL CALCMAP ( XSpline(1), YSpline(1),
     &                   XSPline(MNdeSpline), YSpline(MNdeSpline),
     &                   XNewBegin, YNewBegin, XNewFinish, YNewFinish,
     &                   XTrans, YTrans, ITrans,
     &                   Alpha, CosAlpha, SinAlpha,
     &                   XRotOrigin, YRotOrigin, IRot,
     &                   Scale, IScale,
     &                   DefXYNewBegin, DefXYTrans,
     &                   DefXYNewFinish,
     &                   DefAlpha, DefXYRotOrigin,
     &                   DefScale, IVerbose )

C         Map the curve.
          DO NNde = 1, MNdeSpline
            CALL DOMAP ( XSpline(NNde), YSpline(NNde),
     &                   XRotOrigin, YRotOrigin, IReflect,
     &                   XTrans, YTrans, ITrans,
     &                   CosAlpha, SinAlpha, IRot,
     &                   Scale, IScale,
     &                   XSpline(NNde), YSpline(NNde) )
          END DO

C         Try to match endpoints exactly.
C          IF ( DefXYNewBegin .NE. 1 ) THEN
C            XSPline(1) = XNewBegin
C            YSPline(1) = YNewBegin
C          END IF
C          IF ( DefXYNewFinish .NE. 1 ) THEN
C            XSPline(MNdeSpline) = XNewFinish
C            YSPline(MNdeSpline) = YNewFinish
C          END IF

C         
        ELSE IF ( Char .EQ. 'NEWPTS' .AND.
     &            SplineBufferEmpty .EQ. 1 ) THEN
          WRITE (*,'(A)')
     &          ' WARNING: Spline Buffer is empty, no action taken.'

        ELSE IF ( Char .EQ. 'NEWPTS' ) THEN
C         Distribute points along the curve.
          SplineBufferChanged = 1

          IF ( IVerbose .GT. 2 )
     &      WRITE (*,'(A/6X,2(A,I3),A,2(/6X,A,I3,A,2E13.5))')
     &          ' NEWPTS:',' The new curve is named',NmeCurvNew,
     &          ' type',IBTypeNew,',',
     &          ' connected to boundary named',
     &          NghFNew,' at X,Y =,',XSpline(1),YSpline(1),
     &          '       and to boundary named',
     &          NghLNew,' at X,Y =,',XSpline(MNdeSpline),
     &                               YSpline(MNdeSpline)

C         Calculate the arclength of the curve.
          TSpline(1) = 0.
          XHi = XSpline(1)
          YHi = YSpline(1)
          DO NNde = 2, MNdeSpline
            XLo = XHi
            YLo = YHi
            XHi = XSpline(NNde)
            YHi = YSpline(NNde)
            DT = SQRT((XHi-XLo)**2+(YHi-YLo)**2)
            TSpline(NNde) = TSpline(NNde-1)+DT
          END DO

C         Match TIntOrig to the arclength of TSpline.
          TSplineMax = TSpline(MNdeSpline)
          DO NInt = 0,MInt+1
            TInt(NInt) = TIntOrig(NInt) * TSplineMax
          END DO
            
C         Adapt RhoInt to given scalings.
          RhoInt(0) = RhoScaleEnd*RhoIntOrig(0)
          RhoInt(1) = RhoScaleEnd*RhoIntOrig(1)
          DO NInt = 2,MInt-1
            RhoInt(NInt) = RhoScaleInt*RhoIntOrig(NInt)
          END DO
          RhoInt(MInt) = RhoScaleEnd*RhoIntOrig(MInt)
          RhoInt(MInt+1) = RhoScaleEnd*RhoIntOrig(MInt+1)

          IF ( MNdePts .LT. 2 ) THEN
C           Find the number of nodes on the segment via spacing.
C           
C           First point along the new curve.
            XNew(1) = XSpline(1)
            YNew(1) = YSpline(1)
            TNew(1) = 0.
            NInt = 2
C           Find the spacing at the last point, i.e. 0.
            TNew(2) = SPCINT(MInt,NInt,TInt,RhoInt,TNew(1))
C           
C           Find points along the new curve.
            MNew = 1
            DO WHILE ( TNew(MNew+1) .LT. TSplineMax )

              IF ( MNew+1 .EQ. MaxNode ) THEN
                WRITE (*,'(2A,I7/A)')
     &               ' FATAL: The maximum number of nodes in the',
     &               ' segment', MaxNode,
     &               '       is being exceeded.'
                STOP
              END IF
              
              THere = TNew(MNew+1)
              CALL BLEND ( MNdeSpline, LvlBlend-1, IClosed,
     &             XSpline, YSpline, TSpline, 
     &             XNew(MNew+1), YNew(MNew+1), THere)
              MNew = MNew+1
              TNew(MNew+1) = THere+SPCINT(MInt,NInt,TInt,RhoInt,THere)
            END DO
C           
C           Fix the endpoint.
            XNew(MNew)   = XSpline(MNdeSpline)
            YNew(MNew)   = YSpline(MNdeSpline)
            TNew(MNew) = TSplineMax

          ELSE
C           Distribute a fixed number of points evenly.
            MNew = MNdePts
            DT = TSplineMax/MNew

            
            XNew(1) = XSpline(1)
            YNew(1) = YSpline(1)
            TNew(1) = 0.
            
            DO NNde = 2, MNew-1
              TNew(NNde) = NNde*DT
              CALL BLEND ( MNdeSpline, LvlBlend-1, IClosed,
     &             XSpline, YSpline, TSpline, 
     &             XNew(NNde), YNew(NNde), TNew(NNde) )
              
            END DO

            XNew(MNew)   = XSpline(MNdeSpline)
            YNew(MNew)   = YSpline(MNdeSpline)
            TNew(MNew+1) = TSplineMax
          END IF



          IF ( IVerbose .GT. 2 )
     &         WRITE (*,'(6X,A,I5,A)')
     &         ' and contains',MNew,' new nodes.'
C         
C

C         Relax points between 1 and MNew.
          DO NIter = 1,MIter
            DO NNde = MNew-1,2,-1
              DTHi = SQRT((XNew(NNde)-XNew(NNde+1))**2+
     &                    (YNew(NNde)-YNew(NNde+1))**2)
              DTLo = SQRT((XNew(NNde-1)-XNew(NNde))**2+
     &                    (YNew(NNde-1)-YNew(NNde))**2)
              DBoth = DTLo+DTHi
              TIntLo = SPCINT(MInt,NInt,TInt,RhoInt,TNew(NNde)-.5*DTLo)
              TIntHi = SPCINT(MInt,NInt,TInt,RhoInt,TNew(NNde)+.5*DTHi)
C             Compare actual spacing to ideal spacing.
              DT = DBoth*TIntLo/(TIntLo+TIntHi)-DTLo
              IF (DT.LT.0) THEN
                TDT = MAX( -.5, DT/DTLo )*(TNew(NNde)-TNew(NNde-1))
              ELSE
                TDT = MIN(  .5, DT/DTHi )*(TNew(NNde+1)-TNew(NNde))
              END IF
              TNew(NNde) = TNew(NNde)+TDT
C
C             Spline the new point at the averaged location.
              CALL BLEND ( MNdeSpline, LvlBlend-1, IClosed,
     &                     XSpline, YSpline, TSpline,
     &                     XNew(NNde), YNew(NNde), TNew(NNde))
            END DO
          END DO
C
          IF (FList) THEN
C           List points between 1 and MNew.
            DTHi = SQRT((XNew(1)-XNew(2))**2+
     &                  (YNew(1)-YNew(2))**2)
            TIntHi = SPCINT(MInt,NInt,TInt,RhoInt,TNew(1)+.5*DTHi)
            WRITE (*,'(//A/2A/I3,2F7.4,2E15.7,2F7.4)') 'New Points:',
     &           '  N   T      T%        X             ',
     &           'Y            DT   DTintp    D/Ti',
     &           1,0.,0.,XNew(1),YNew(1),DTHi,TIntHi
            DO NNde = 2,MNew-1
              DTLo = DTHi
              DTHi = SQRT((XNew(NNde)-XNew(NNde+1))**2+
     &                    (YNew(NNde)-YNew(NNde+1))**2)
              DBoth = DTLo+DTHi
              TIntLo = SPCINT(MInt,NInt,TInt,RhoInt,TNew(NNde)-.5*DTLo)
              TIntHi = SPCINT(MInt,NInt,TInt,RhoInt,TNew(NNde)+.5*DTHi)
C             Compare actual spacing to ideal spacing.
              DTRel = TIntLo/TIntHi*DTHi/DTLo
              WRITE (*,'(I3,2F7.4,2E15.7,2F7.4,F10.6)') 
     &              NNde,TNew(NNde),TNew(NNde)/TNew(MNew),XNew(NNde),
     &              YNew(NNde),DTHi,TIntHi,DTRel
            END DO
            WRITE (*,'(I3,2F7.4,2E15.7/)') MNew,TNew(NNde),
     &                       TNew(NNde)/TNew(MNew),XNew(MNew),YNew(MNew)
          END IF
C
        ELSE IF ( Char .EQ. 'WRTPTS' .AND.
     &            SplineBufferChanged .EQ. 0 ) THEN
          WRITE (*,'(A)')
     &          ' WARNING: Spline buffer is unchanged since last',
     &          ' write. No action taken.'
C
        ELSE IF ( Char .EQ. 'WRTPTS' ) THEN
          IF ( IVerbose .GT. 3 )
     &      WRITE (*,'(A,A)')
     &            ' WRTPTS: append new buffer to:',OpFlNm
C         Dump the points.
          CALL WRITEPTS (NtOp,MNew,XNew,YNew,
     &                   NmeCurvNew,NghFNew,NghLNew,
     &                   IBTypeNew,MinResNew,
     &                   MNotCon,NmeNotCon,
     &                   MArc,ArcRelNew,DltZeroNew)
          
C         Mark spline buffer as written.
          SplineBufferChanged = 0
C
          CALL MAPDEFAULTS ( DefXYNewBegin, DefXYNewFinish, 
     &                       DefXYTrans, ITrans,
     &                       DefAlpha, DefXYRotOrigin, IRot,
     &                       DefScale, IScale, IReflect )
          
          CALL CRVDEFAULTS ( DefXYBegin, DefXYFinish, DefXYThird,
     &                       DefXYCenter, DefAlphaBeginFinish,
     &                       DefPiBeginFinish, DefSinePower,
     &                       DefRadius, Defpolycoeff, MaxPolyCoeff )
            
C
        ELSE
          WRITE (*,'(2A)') ' WARNING: Unknow keyword in .ictr:',Char
C
        END IF
C
      END DO
   99 WRITE (NtOp,'(A)') ' ENDDAT'
      CLOSE (NtIn)
      CLOSE (NtOp)
      CLOSE (NtP)
      CLOSE (99)
      STOP
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WRITEPTS (NtOp,MNew,XNew,YNew,
     &                   NmeCurvNew,NghFNew,NghLNew,
     &                   IBTypeNew,MinRes,
     &                   MNotCon,NmeNotCon,MArc,ArcRel,DltZero)
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      DIMENSION ArcRel(*), DltZero(*), XNew(*), YNew(*), NmeNotCon(*)
C
C     Write the header info to OpFlNm.
      WRITE (NtOp,1001) NmeCurvNew,NghFNew,NghLNew,IBTypeNew
 1001 FORMAT (' NEWBND'/' NAMEBN'/I3/' NFRSBN'/I3/' NLSTBN'/I3/
     &        ' ITYPBN'/I3)
      
      IF (MNotCon.GT.0) 
     &  WRITE (NtOp,'(A/20I3)') ' ANTICO', (NmeNotCon(I),I=1,MNotCon)
      
      IF (MArc.GT.0) 
     &  WRITE (NtOp,'(A,200(/2F15.7))') ' DLTZER',
     &        (ArcRel(K),DltZero(K),K=1,MArc)
      
      IF ( MinRes .GT. 2 )
     &  WRITE (NtOp,'(A/I3)') ' MINRES', MinRes
      WRITE (NtOp,'(A/I6)') ' NRBNDE', MNew
      
      WRITE (NtOp,'(A)') ' BNDEXY'
      WRITE (NtOp,'(2E15.7)') (XNew(NNde),YNew(NNde),NNde = 1,MNew)
C
      RETURN
      END
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE FILENMLEN ( FileName, MaxLenUser )
C
C     Last update:
C     15Nov96; conceived.
C
C     Skip the leading blanks of a filename and retain the string up to
C     the next blank.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      INTEGER IBegin, IEnd, MaxLenUser
      CHARACTER FileName*1
        
C     Skip the leading blanks.
      IBegin = 1
      DO WHILE ( FileName(IBegin:IBegin) .EQ. ' ' .AND.
     &           IBegin .LT. MaxLenUser )
        IBegin = IBegin+1
      END DO

C     Find the End.
      IEnd = IBegin
      DO WHILE ( FileName(IEnd:IEnd) .NE. ' ' .AND.
     &           IEnd .LT. MaxLenUser )
        IEnd = IEnd+1
      END DO
      
      FileName = FileName(IBegin:IEnd)
      RETURN
      END
      
