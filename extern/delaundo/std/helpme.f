      SUBROUTINE HELPME
C
C Last update:
C 15Nov96; add info for ipol.
C 19May96; new path.
C
C Pop up the help menu and list selected help pages.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT INTEGER(I-P),LOGICAL*1(F)
      CHARACTER*255 Char*1,CharHelpPath,CharLine*80, EnvName
      NtHelp = 11
      MLine = 20
C
C     Get the path for the help pages.
      EnvName = 'DLNDO_HELP'
      CALL PATH ( EnvName, CharHelpPath, KHiHelpPath, 255 )
C
      Finished = .FALSE.
      DO WHILE (.NOT.Finished)
        WRITE (*,'(8(A/),A)')
     &  ' Do you need help on DELAUNDO in general       g,',
     &  ' on the changes of the latest version          n,',
     &  ' on the syntax of the .ctr parameter file      c,',
     &  ' on the syntax of the .pts input file          p,',
     &  ' on the format of the .dpl output file         d,',
     &  ' ',
     &  ' on the syntax of the .ictr file for ipol      i,',
     &  ' ',
     &  ' do you want to exit the help menu             x,',
     &  ' or do you want to stop the program            s?'
        READ (*,'(A)') Char
C
        IF (Char.EQ.'x'.OR.Char.EQ.'X') THEN
          RETURN
C
        ELSE IF (Char.EQ.'s'.OR.Char.EQ.'S') THEN
          STOP
C
        ELSE IF (Char.EQ.'n'.OR.Char.EQ.'N') THEN
          OPEN (NtHelp,FILE=CharHelpPath(1:KHiHelpPath)//'news.txt',
     &          STATUS='OLD',FORM='FORMATTED')
C
        ELSE IF (Char.EQ.'g'.OR.Char.EQ.'G') THEN
          OPEN (NtHelp,FILE=CharHelpPath(1:KHiHelpPath)//'general.txt',
     &          STATUS='OLD',FORM='FORMATTED')
          
        ELSE IF (Char.EQ.'c'.OR.Char.EQ.'C') THEN
          OPEN (NtHelp,FILE=CharHelpPath(1:KHiHelpPath)//'ctrl.txt',
     &          STATUS='OLD',FORM='FORMATTED')
C
        ELSE IF (Char.EQ.'p'.OR.Char.EQ.'P') THEN
          OPEN (NtHelp,FILE=CharHelpPath(1:KHiHelpPath)//'pts.txt',
     &          STATUS='OLD',FORM='FORMATTED')
C
        ELSE IF (Char.EQ.'d'.OR.Char.EQ.'D') THEN
          OPEN (NtHelp,FILE=CharHelpPath(1:KHiHelpPath)//'dpl.txt',
     &          STATUS='OLD',FORM='FORMATTED')
C
        ELSE IF (Char.EQ.'i'.OR.Char.EQ.'I') THEN
          OPEN (NtHelp,FILE=CharHelpPath(1:KHiHelpPath)//'ipol.txt',
     &          STATUS='OLD',FORM='FORMATTED')
C
        END IF
C
        WRITE (*,'(4(A/),A)')
     &  ' The help files on your system are in.',CharHelpPath,
     &  ' Help files are paged in 20 line screenfuls.',
     &  ' Hit return to continue, type x to exit.',
     &  ' ------------------------------------------'
        READ (*,'(A1)') Charline
        WRITE (*,*) ' '
C
        FLoop = .TRUE.
        DO WHILE (FLoop)
          DO NLine = 1,MLine
            READ (NtHelp,'(A)',END=901) CharLine
            WRITE (*,'(A)') CharLine
          END DO
          READ (NtHelp,'(A)',END=901) CharLine
          WRITE (*,'(A,$)') CharLine
          READ (*,'(A1)') CharLine
          IF (CharLine(1:1).EQ.'X'.OR.CharLine(1:1).EQ.'x')
     &      FLoop = .FALSE.
        END DO
  901   CLOSE (NtHelp)
      END DO
C
      END
