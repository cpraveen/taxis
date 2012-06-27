      SUBROUTINE READCHAR (NtCtrl,CharOpt,MCharOpt,CharTxt,MCharTxt,
     &                     FNoCtr,FRequired,FKeyDefault,Answer)
C
C Last update:
C
C
C Read a character from a .ctr file for Delaundo. Welcome to DOMINO
C software.
C
C Input:
C NtCtrl:       Logical number of the unit to read from.
C CharOpt:      Vector of permissible options of 1 character
C MCharOpt:     Number of permissible options.
C CharTxt:      Vector of explaining text lines.
C MCharTxt:     Number of explaining text lines.
C FNoCtr:       .TRUE. if there is no .ctr file to be read.
C FRequired:    .TRUE. if this parameter must be specified.
C
C Output:
C FKeyDefault:  .TRUE. if no value was given.
C Answer:       Value given by the user.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      CHARACTER CharOpt(*)*1,CharTxt(*)*80,String*80,Answer
C
C     Initially, we now knothing!
      FKeyDefault = .TRUE.
C
      IF (.NOT.FNoCtr) THEN
C       Try to read Answer from .ctr file.
        READ (NtCtrl,'(A80)') String
C
C       Ignore leading blanks.
        KPos = 1
        DO WHILE (String(KPos:KPos).EQ.' '.AND.KPos.LT.80)
          KPos = KPos+1
        END DO
C
        DO NOpt = 1,MCharOpt
          IF (String(KPos:KPos).EQ.CharOpt(NOpt)) THEN
            Answer = String(KPos:KPos)
            FKeyDefault = .FALSE.
          END IF
        END DO
      END IF
C
      FOnce = .TRUE.
      DO WHILE (FKeyDefault.AND.(FRequired.OR.FOnce))
        FOnce = .FALSE.
C       Keep on reading Answer if required, at least once.
        WRITE (*,'(/(A))') (CharTxt(NTxt),NTxt=1,MCharTxt)
        READ (*,'(A80)') String
C
C       Ignore leading blanks.
        KPos = 1
        DO WHILE (String(KPos:KPos).EQ.' '.AND.KPos.LT.80)
          KPos = KPos+1
        END DO
C
        DO NOpt = 1,MCharOpt
          IF (String(KPos:KPos).EQ.CharOpt(NOpt)) THEN
            FKeyDefault = .FALSE.
            Answer = String(KPos:KPos)
          END IF
        END DO
C
      END DO
C
      RETURN
      END
