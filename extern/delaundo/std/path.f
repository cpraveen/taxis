      SUBROUTINE PATH ( ENV_NAME, ENV_VAL, KHiVal, KHiMax )
C
C Last update:
C 18May96; intro getenv for the paths.
C
C Get a pathname from an environment variable. Find its length
C for appending filenames and append a trailing slash, if necessary.
C
C Input:
C ENV_NAME:   Name of the env.
C KHiMax:     Length of the character arrays that are passed.
C
C Changes to:
C ENV_VAL:    Value of the env.
C KHiVal:     Length of the string.
C
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
C        1         2         3         4         5         6         7
C
      IMPLICIT NONE
      INTEGER KLo, KHi, KHiVal, KHiMax
      CHARACTER*255 ENV_NAME, ENV_VAL

C     Get the env.
      CALL GETENV ( ENV_NAME, ENV_VAL )

C     Find the significant part of the env (the upper bound
C     is needed for filename concatenation.)
      KLo = 1
      DO WHILE ( KLo.LT.KHiMax .AND. ENV_VAL(KLo:KLo).EQ.' ')
        KLo = KLo+1
      END DO
      KHi = KLo
      DO WHILE (KHi.LT.KHiMax .AND. ENV_VAL(KHi:KHi).NE.' ')
        KHi = KHi+1
      END DO
      
      IF ( KLo .EQ. KHi ) THEN
        WRITE (*,'(2A)') ' FATAL: no environment like ',ENV_NAME
        STOP
      ELSE
        IF ( ENV_VAL(KHi-1:KHi-1) .EQ. '/' ) THEN
          ENV_VAL = ENV_VAL(KLo:KHi-1)
          KHiVal = KHi-KLo
        ELSE
C         Append a slash.
          ENV_VAL = ENV_VAL(KLo:KHi-1)//'/'
          KHiVal = KHi-KLo+1
        END IF
      END IF
C
      RETURN
      END
