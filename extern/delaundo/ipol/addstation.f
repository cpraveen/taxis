C     Interpolation routines.
C     
C     Last update
C     
C     Contains:
C     ---------
C     SUBROUTINE ADDSTATION:
C     FUNCTION SPCINT:
C

      
      SUBROUTINE ADDSTATION ( MInt, MaxInt, TInt, RhoInt,
     &                        TStation, RhoStation, IVerbose )

C     Last update
C     
C     Add an interpolation station into an ordered list.
C
C     Input:
C     MInt: Number of already ordered stations in the list.
C     MaxInt: Available ize of the field.
C     TInt, RhoInt: Arclength and Value of the station to insert.
C     TStation, RhoStation: Ordered List of Arclengths and the corr. Values.
C                           Arrays start at 0, run through MaxInt+1.
C     IVerbose: Verbosity level.
C
C     Changes to:
C     MInt:
C     TStation, RhoStation:
C     
C     789 123456789 123456789 123456789 123456789 123456789 123456789 12
C     7  1         2         3         4         5         6         7
C
      IMPLICIT NONE
      REAL TInt(0:*), RhoInt(0:*), TStation, RhoStation
      INTEGER MInt, MaxInt, NInt, IVerbose, NMove

      IF ( MInt .EQ. 0 ) THEN
        TInt(0) = -1.
        TInt(1) = 2.
      END IF
      
      IF ( TStation .LT. 0. .OR. TStation .GT. 1. ) THEN
        IF ( IVerbose .GT. 4 )
     &    WRITE (*,'(2A,2F12.7)') ' WARNING: Spacing point out of',
     &                            ' range, omitted:',
     &                            TStation, RhoStation
        RETURN        
      END IF

C     Find the interval between NInt and NInt+1  to insert TStation.
      NInt = 0
      DO WHILE ( TInt(NInt+1) .LT. TStation )
        NInt = NInt+1
      END DO
      
      IF ( TInt(NInt+1) .EQ. TStation ) THEN
C       Duplicated point, use RhoStation.
        IF ( IVerbose .GT. 4 )
     &    WRITE (*,'(2A,2F12.7)') ' WARNING: Duplicated spacing',
     &                            ' interpolation point, new value:',
     &                            TStation, RhoStation
        RhoInt(NInt+1) = RhoStation

      ELSE
C       New point, check for space in the list.
        IF ( MInt+1 . EQ. MaxInt ) THEN
          WRITE (*,'(A,I3)') 'FATAL: Too many spacing points:',
     &                       MInt+1
          STOP
        END IF

C       Move all following stations one position up.
        DO NMove = MInt,NInt+1,-1
          TInt(NMove+1) = TInt(NMove)
          RhoInt(NMove+1) = RhoInt(NMove)
        END DO
        MInt = MInt+1

C       Insert the new station.
        TInt(NInt+1) = TStation
        RhoInt(NInt+1) = RhoStation
        
C       Update the spacing at the lower limit.
        RhoInt(0) = RhoInt(1)

C       Create a new dummy as upper limit.
        TInt(MInt+1) = 2.
        RhoInt(MInt+1) = RhoInt(MInt)
        
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION SPCINT (MInt,NInt,TInt,RhoInt,TNew)
C
C     Interpolate the spacing.
C
C     Input:
C     MInt: Number of stations.
C     NInt: Guess for the number of the interval TInt lies in.
C     TInt, RhoInt: Ordered list of arclenghts and corr. values.
C     TNew: Arclength for which the value is sought.
C
C     Changes to:
C     NInt: the interval Tint lies in.
C
C     Returns:
C     SpcInt: the value at Tint.
C     
C     789 123456789 123456789 123456789 123456789 123456789 123456789 12
C     7  1         2         3         4         5         6         7
C
      IMPLICIT NONE
      REAL TInt(0:*), RhoInt(0:*)
      DOUBLE PRECISION TNew
      INTEGER MInt, NInt

C     Walk up along intervals.
      DO WHILE (TInt(NInt).LE.TNew)
        IF (NInt.GT.MInt) THEN
          WRITE (*,'(/A)') ' FATAL: Beyond bounds in SPCINT.'
          STOP
        END IF
        NInt = NInt+1
      END DO

C     Walk down along intervals.
      DO WHILE (TInt(NInt-1).GE.TNew)
        IF (NInt.LT.0) THEN
          WRITE (*,'(/A)') ' FATAL: Below bounds in SPCINT.'
          STOP
        END IF
        NInt = NInt-1
      END DO
C
C     Interpolate on the inverval [NInt-1,NInt].
      SPCINT = (RhoInt(NInt)-RhoInt(NInt-1))/(TInt(NInt)-TInt(NInt-1))*
     &         (TNew-TInt(NInt-1))+RhoInt(NInt-1)
C
      RETURN
      END
C
