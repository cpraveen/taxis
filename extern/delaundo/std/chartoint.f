      SUBROUTINE CHARTOINT (String,FNoGood,IntAnswer)
C
C Last update:
C 6Oct92,20:00;conceived.
C
C Read a real from a .ctr file for Delaundo. Welcome to DOMINO software.
C
C Input:
C String:    String to be converted to an integer.
C
C Output:
C FNoGood:   .TRUE. if value could not be converted.
C IntAnswer: Converted integer.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER String*80
C
C     Find the character table of the current machine.
      I0 = ICHAR('0')
      I9 = ICHAR('9')
C     Check consecutiveness of the ASCII table.
      IF (I9-I0.NE.9) THEN
        WRITE (*,'(/A)') ' FATAL: character table screwed up. Sorry.'
        STOP
      END IF
C
C     Initially, we are optimistic!
      FNoGood = .FALSE.
      FNegative = .FALSE.
C     Well, somewhat.
      FNoNumbers = .TRUE.
C
C     Ignore leading blanks.
      KPos = 1
      DO WHILE (String(KPos:KPos).EQ.' '.AND.KPos.LT.80)
        KPos = KPos+1
      END DO
C
C     Find the end of the string while assuring its all numbers.
      Intgr = 0
      IF (String(KPos:KPos).EQ.'-') THEN
        FNegative = .TRUE.
        KPos = KPos+1
      END IF
      DO WHILE (String(KPos:KPos).NE.' '.AND.String(KPos:KPos).NE.','
     &          .AND.KPos.LE.80.AND..NOT.FNoGood)
        ILetter = ICHAR(String(KPos:KPos))
        IF (ILetter.LT.I0) THEN
          FNoGood = .TRUE.
        ELSE IF (ILetter.GT.I9) THEN
          FNoGood = .TRUE.
        ELSE
          Intgr = 10*Intgr+ILetter-I0
          FNoNumbers = .FALSE.
        END IF
        KPos = KPos+1
      END DO
C
      IF (FNoNumbers)  THEN
        FNoGood = .TRUE.
      ELSE IF (FNegative) THEN
        IntAnswer = -Intgr
      ELSE
        IntAnswer = Intgr
      END IF
C
      RETURN
      END
