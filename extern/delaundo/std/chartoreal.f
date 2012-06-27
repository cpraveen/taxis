      SUBROUTINE CHARTOREAL (String,FNoGood,RealAnswer)
C
C Last update:
C 6Oct92,20:00;conceived.
C
C Convert a character string to a real. Welcome to DOMINO software.
C
C Input:
C String:     String to be converted to an integer.
C
C Output:
C FNoGood:    .TRUE. if value could not be converted.
C RealAnswer: Converted real.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER String*80
C
C     Find the character table of the current machine.
      IDot = ICHAR('.')
      I0 = ICHAR('0')
      I9 = ICHAR('9')
C
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
      FDot = .FALSE.
      Rel = 0
      Decimal = 1.
      IF (String(KPos:KPos).EQ.'-') THEN
        FNegative = .TRUE.
        KPos = KPos+1
      ELSE
        FNegative = .FALSE.
      END IF
      DO WHILE (String(KPos:KPos).NE.' '.AND.String(KPos:KPos).NE.','
     &          .AND.KPos.LE.80.AND..NOT.FNoGood)
        ILetter = ICHAR(String(KPos:KPos))
        IF (ILetter.EQ.IDot.AND.FDot) THEN
          FNoGood = .TRUE.
        ELSE IF (ILetter.EQ.IDot) THEN
          FDot = .TRUE.
        ELSE IF (ILetter.LT.I0) THEN
          FNoGood = .TRUE.
        ELSE IF (ILetter.GT.I9) THEN
          FNoGood = .TRUE.
        ELSE IF (.NOT.FDot) THEN
          Rel = 10.*Rel+ILetter-I0
          FNoNumbers = .FALSE.
        ELSE 
          Decimal = .1*Decimal
          Rel = Rel+Decimal*(ILetter-I0)
          FNoNumbers = .FALSE.
        END IF
        KPos = KPos+1
      END DO
C
      IF (FNoNumbers) THEN
        FNoGood = .TRUE.
      ELSE IF (FNegative) THEN
        RealAnswer = -Rel
      ELSE
        RealAnswer = Rel
      END IF
C
      RETURN
      END
