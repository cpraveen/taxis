      SUBROUTINE HSORT(N,ARRIN,INDX)
C
C Last update:
C 6Feb93,21:00;Converted to INTEGER*4
C 14Nov92,1:24; intro RETURN for N<1.
C 7Aug92,20:00;Converted to INTEGER*2
C 7.92,Thanx to Tim Barth.
C
C Do a heapsort on ARRIN.
C
C Input:
C N:     Number of elements to sort.
C ARRIN: Vector of keys to sort for.
C
C Output:
C INDX:  Vector of pointers to each element: 1. = ARRIN(INDX(1)).
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      INTEGER ARRIN(N),INDX(N),Q
C
      DO 11 J=1,N
         INDX(J) = J
 11   CONTINUE
C
      IF (N.LE.1) RETURN
C
      L = N/2 + 1
      IR= N
 10   CONTINUE
      IF(L.GT.1)THEN
         L=L-1
         INDXT = INDX(L)
         Q     = ARRIN(INDXT)
      ELSE
         INDXT = INDX(IR)
         Q     = ARRIN(INDXT)
         INDX(IR) = INDX(1)
         IR = IR - 1
         IF(IR.EQ.1)THEN
            INDX(1) = INDXT
            RETURN
         ENDIF
      ENDIF
C
      I=L
      J=L+L
 20   IF(J.LE.IR)THEN
         IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
         ENDIF
         IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I) = INDX(J)
            I = J
            J = J+J
         ELSE
            J = IR+1
         ENDIF
         GO TO 20
      ENDIF
      INDX(I) = INDXT
      GO TO 10
C
      END

