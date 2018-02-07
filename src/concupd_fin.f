C
C**************************  CONCUPD ***********************************
C
C  update surface CONCENTRATION FOR COUPLING SURFACE AND SUBSURFACE TRANSPORT
C
C***********************************************************************
C
      SUBROUTINE CONCUPD_FIN(NNOD,IFATM,IFATMP,DELTAT,PONDNOD,
     1     PONDNODP,ARENOD,ATMPOT,ATMACT,CONCNOD,ATMCONC,
     2     CONCNODUPD,CNNEW,CONCNODBC)
C
      IMPLICIT NONE
      INTEGER  NNOD,I
      INTEGER  IFATM(*),IFATMP(*),cont(10)
      REAL*8   DELTAT
      REAL*8   PONDNOD(*),PONDNODP(*),ARENOD(*),ATMPOT(*),ATMACT(*)
      REAL*8   CONCNOD(*),ATMCONC(*),CONCNODUPD(*),CNNEW(*)
      REAL*8   CONCNODBC(*)
      REAL*8   VOLTOT(NNOD),MASSURF(NNOD),MASSPOT(NNOD),MASSACT(NNOD)
      INCLUDE  'IOUNITS.H'
C    
C      
c      CALL init0r(NNOD,CONCNODUPD)
c
      DO I = 1,NNOD
c      
        IF ((IFATM(I) .NE. 2).AND.(PONDNOD(I).EQ.0.0d0)) THEN
            CONCNODUPD(I) = 0.0d0
        ELSEIF ((IFATM(I) .EQ. 2).AND.(PONDNOD(I).NE.0.0d0)) THEN
            IF (ATMACT(I) .GE. 0.) THEN
                CONCNODUPD(I) = CONCNODBC(I)
            ELSE
                MASSURF(I) = CONCNODBC(I) * PONDNOD(i) * ARENOD(i)
                MASSPOT(I) = -ATMACT(I) * CNNEW(i) * DELTAT
                VOLTOT(I) = PONDNOD(i) * ARENOD(i) - ATMACT(I) * DELTAT
                CONCNODUPD(I) = (MASSURF(I) + MASSPOT(I)) / VOLTOT(I)
            END IF
        END IF

      END DO      
C         
      RETURN  
      END
