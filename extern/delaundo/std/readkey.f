      SUBROUTINE READKEY (NtCtrl,KeyCtr,FileEnd)
C
C Last update:
C 8Oct92,18:45;conceived.
C
C Read a keyword from a .ctr file for Delaundo. Welcome to DOMINO
C software.
C
C Input:
C NtCtrl:  Logical number of the unit to read from.
C
C Output:
C KeyCtr:  Next keyword given in the .ctr file.
C FileEnd: .TRUE. if NtCtrl has ended.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER String*256,KeyCtr*6
C
      READ (NtCtrl,'(A)',END=99) String
C
C     Find the non-blank part of the string.
      KPos = 1
      DO WHILE (String(KPos:KPos).EQ.' '.AND.KPos.LE.255)
        KPos = KPos+1
      END DO
C
      IF (KPos.EQ.256.OR.
     &    String(KPos:KPos).EQ.'%'.OR.
     &    String(KPos:KPos).EQ.'!') THEN
        KeyCtr = 'IGNORE'
      ELSE
        KeyCtr = String(KPos:KPos+5)
      END IF
      RETURN
C
   99 FileEnd = .TRUE.
      KeyCtr = 'ENDDAT'
      RETURN
C
      END
