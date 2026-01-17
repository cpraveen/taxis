      REAL FUNCTION DTIME (Time)
C
C Last update:
C 9Jan96: conceived.
C
C Take the IBM's mclock and convert it to the more standard dtime.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4          5         6        7
C
      NTime = MCLOCK()
      DTime = FLOAT(NTime)/100.
C
      RETURN
      END
