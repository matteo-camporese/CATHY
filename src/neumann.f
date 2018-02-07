C**********************************************************************
C                            SUBROUTINE NEUMANN
C compute vertically duplicated BCs and free-drainage fluxes
C**********************************************************************
      SUBROUTINE NEUMANN(TIME,NNEU,NNEUC,NNOD,NSTR,CKRW,PERMX,PERMZ,
     1                   ARENOD,ACONTQ,QINP,Q)
C
      IMPLICIT NONE
C
      INCLUDE 'CATHY.H'
C
      INTEGER I,J,K,NN
      INTEGER NNOD,NSTR,STR
      INTEGER NNEU(3),NNEUC(3),ACONTQ(NQMAX)
      REAL*8  CKRW(NMAX),ARENOD(NODMAX),Q(NQMAX),TIME,SUM1,SUM2
      REAL*8  PERMX(MAXSTR,MAXZON),PERMZ(MAXSTR,MAXZON),QINP(3,NQMAX)
C
      K = 0
      SUM1=0.0d0
      SUM2=0.0d0
      IF (NNEU(2) .LT. 0) THEN
          DO I=1,NNOD
             K = K + 1
             NN = ACONTQ(K)
             Q(K)=-1.0d0*ARENOD(I)*CKRW(NN)*PERMZ(NSTR,1)
             SUM1=SUM1+Q(K)
          END DO
          DO I = 1,NNEUC(2)
             K = K + 1
             NN = ACONTQ(K)
             STR = MIN(INT(REAL(NN-1)/NNOD)+1,NSTR)
CM           Q(K) = CKRW(NN)*PERMX(STR,1)*QINP(2,K) Panola only
             Q(K) = QINP(2,K)
             SUM2=SUM2+Q(K)
          END DO
          write(999,*)TIME,SUM1,SUM2
      END IF
C
      RETURN
C
      END
