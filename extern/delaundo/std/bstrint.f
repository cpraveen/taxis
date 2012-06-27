      REAL FUNCTION BSTRINT (X,Y,NBVvr,NBFrmNde,XNode,YNode,BkgStr,
     &                       BndStr,DomStr,StrPw,StrPwInv)
C
C Last update:
C 11Nov92,20:30;intro SUPNTPL.
C 1Jul92,21:39;copied from BSPCINT.
C
C Interpolate stretching values assigned to the nodes of a
C background triangle on that triangle.
C
C Input:
C X:        X-coordinate of the node to interpolate for.
C Y:        Y-coordinate of the node to interpolate for.
C NBVvr:    Background cell to start the search with.
C /NODES/:  Coordinates of the nodes. See main program
C           DELAUNDO for details.
C /BKGRD/:  Connectivity of the background mesh. See SUBROUTINE
C           INTCLD for details.
C
C Output:
C BStrInt:  Interpolated spacing at x,y.
C NBVvr:    Background cell that contains x,y.
C     
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C     
      IMPLICIT INTEGER(I-P)
      DOUBLE PRECISION X,Y,XNode(*),YNode(*)
      DIMENSION NBFrmNde(3,*),BkgStr(*)
C
C     Forming nodes of the background cell.
      NB1 = NBFrmNde(1,NBVvr)
      NB2 = NBFrmNde(2,NBVvr)
      NB3 = NBFrmNde(3,NBVvr)
C
C     Interpolate with the power StrPw.
      CALL SUPNTPL(NB1,BkgStr(NB1),XNode(NB1),YNode(NB1),
     &             NB2,BkgStr(NB2),XNode(NB2),YNode(NB2),
     &             NB3,BkgStr(NB3),XNode(NB3),YNode(NB3),
     &             BndStr,BndStr,DomStr,DomStr,StrPw,StrPwInv,
     &             X,Y,BStrInt)
      BStrInt = MAX(1.,BStrInt)
C
      RETURN
      END
