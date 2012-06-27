      SUBROUTINE READINTEGER (NtCtrl,CharTxt,MCharTxt,
     &                        FNoCtr,FRequired,FKeyDefault,IntAnswer)
C
C Last update:
C 10Oct92,18:45;conceived.
C
C Read an integer from a .ctr file for Delaundo. Welcome to DOMINO
C software.
C
C Input:
C NtCtrl:       Logical number of the unit to read from.
C CharTxt:      Vector of explaining text lines.
C MCharTxt:     Number of explaining text lines.
C FNoCtr:       .TRUE. if there is no .ctr file to be read.
C FRequired:    .TRUE. if this parameter must be specified.
C
C Output:
C FKeyDefault:  .TRUE. if no value was given.
C IntAnswer:    Integer value given by the user.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      IMPLICIT INTEGER(I-P),LOGICAL*1(F),DOUBLE PRECISION(R-S,X-Y)
      CHARACTER String*80,CharTxt(*)*80
C
C     Initially, we now knothing!
      FKeyDefault = .TRUE.
C
      IF (.NOT.FNoCtr) THEN
C       Try to read Answer from .ctr file.
        READ (NtCtrl,'(A80)') String
        CALL CHARTOINT(String,FKeyDefault,IntAnswer)
      END IF
C
      FOnce = .TRUE.
      DO WHILE (FKeyDefault.AND.(FRequired.OR.FOnce))
        FOnce = .FALSE.
C       Keep on reading Answer if required, at least once.
        WRITE (*,'(/(A))') (CharTxt(NTxt),NTxt=1,MCharTxt)
        READ (*,'(A80)') String
        CALL CHARTOINT(String,FKeyDefault,IntAnswer)
      END DO
C
      RETURN
      END
