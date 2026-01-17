C Last update:
C 11Nov92,21:00; intro StrPwInv.
C 26Sep92,20:17;intro /TOLER/.
C 25Jun92,14:39;increase QTolRtLo to .5 to preclude excessive angles.
C     5May92, 22:25. (You got the power!)
C     12Mar92, 4:31. (remove bug in dimension of FBkgStr.)
C     21Feb92, 2:55. (intro single prec. BTol.)
C     15Feb92, 22:20. (intro stretch parameters, single prec. bkgspc.)
C     6Jul91, 17:15. (Removing misalignment.)
C
C     Common block for DELAUNDO containing the background grid with
C     the prescribed spacing values at the nodes and the distance and
C     skewness tolerances for the frontal algorithm. Welcome to DOMINO
C     software.
C
c      PARAMETER (DTol=.65,DTol2=DTol**2,QTol=.65,QTol2=QTol**2,
c     &           QtolRtHi=2.1168639,QTolRtLo=.5)
c     &           QtolRtHi=SQRT(1./QTol2-.25),
c     &           QTolRtLo=SQRT(QTol2-.25))
C
      DOUBLE PRECISION Pi,PiHalf
      REAL SpcPw,StrPw,StrPwInv
      COMMON /BKGRD/ Pi,PiHalf,BkgStr(MaxBndNde),BkgSpc(MaxBndNde),
     &               BkgVol(2*MaxBndNde),BndStr,DomStr,
     &               BTol,DltStar,DMax,SpcPw,StrPw,StrPwInv,MBVvr,
     &               NBFrmNde(3,2*MaxBndNde),NghBVvr(3,2*MaxBndNde),
     &               FBkgStr(2*MaxBndNde),IArea(2*MaxBndNde)
      COMMON /TOLER/ DTol,DTol2,QTol,QTol2,QtolRtHi,QTolRtLo
