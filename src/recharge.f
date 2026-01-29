C
C**************************  RECHARGE **********************************
C
C  calculates water table recharge in terms of both flux and volume
C
C***********************************************************************
C
      SUBROUTINE RECHARGE(NNOD,NSTR,WNOD,ARENOD,VOLNOD,PNEW,PTIMEP,Z,
     1                    SNODI,PNODI,TIME,DELTAT,RECFLOW,RECVOL,RECNOD,
     2                    TWS)
C
      IMPLICIT  NONE
      INCLUDE   'CATHY.H'
      INTEGER   I,J,INOD1,INOD2
      INTEGER   NNOD,NSTR
      INTEGER   FLAG(NNOD),NODWT(NNOD)
      REAL*8    TIME,DELTAT,ZERO,RC,FVGSE,ONE
      REAL*8    RECFLOW,RECVOL,SST,MWT,STOR,TSTOR
      REAL*8    WNOD(*),ARENOD(*),VOLNOD(*),PNEW(*),PTIMEP(*),Z(*)
      REAL*8    SWNEW(NMAX),SWTIMEP(NMAX),WTNEW(NODMAX),WTTIMEP(NODMAX)
      REAL*8    SNODI(*),PNODI(*),RECNOD(*),TWS(NODMAX)
      PARAMETER (ZERO=0.0d0,ONE=1.0d0)
      INCLUDE   'SOILCHAR.H'
C
      RECFLOW = ZERO
      TSTOR = ZERO
      IF (TIME.EQ.0.0D0) THEN
         RECVOL = ZERO
      END IF
      CALL INIT0R(NNOD,RECNOD)
      CALL INIT0I(NNOD,FLAG)
      DO I=1,NNOD
         NODWT(I)=I
      END DO
      CALL WTDEPTH(NNOD,NODWT,NSTR,NNOD,TIME,Z,PNEW,WTNEW)
      CALL WTDEPTH(NNOD,NODWT,NSTR,NNOD,TIME,Z,PTIMEP,WTTIMEP)
      SST = ZERO
      MWT = ZERO
      STOR = ZERO
C
      DO I=1,NNOD
         TWS(I) = ZERO
         MWT = MWT + PNODI(I)*(WTNEW(I)-WTTIMEP(I))*ARENOD(I)
         DO J=NSTR,1,-1
            INOD1=I+J*NNOD
            INOD2=I+(J-1)*NNOD
            TWS(I) = TWS(I) + SWNEW(INOD1)*VOLNOD(INOD1)*PNODI(INOD1)
            SWNEW(INOD1)=VGPNOT(INOD1)*FVGSE(PNEW(INOD1),INOD1)+
     &                   VGRMC(INOD1)/PNODI(INOD1)
            SWNEW(INOD2)=VGPNOT(INOD2)*FVGSE(PNEW(INOD2),INOD2)+
     &                   VGRMC(INOD2)/PNODI(INOD2)
            SWTIMEP(INOD1)=VGPNOT(INOD1)*FVGSE(PTIMEP(INOD1),INOD1)+
     &                   VGRMC(INOD1)/PNODI(INOD1)
            SWTIMEP(INOD2)=VGPNOT(INOD2)*FVGSE(PTIMEP(INOD2),INOD2)+
     &                   VGRMC(INOD2)/PNODI(INOD2)
            IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).GE.ZERO
     &         .AND.FLAG(I).EQ.0) THEN
               IF (J.EQ.1) THEN
CM Totally saturated soil column 
                  FLAG(I)=-1
CM                IF (WNOD(INOD2).LE.ZERO) THEN
CM                   RECNOD(I)=-1.0d0*WNOD(INOD2)*ARENOD(I)
CM                   RECNOD(I)=0.0d0
CM                END IF
                  STOR = STOR+VOLNOD(INOD2)*(SNODI(INOD2)*PNEW(INOD2)+
     &                   PNODI(INOD2)*SWNEW(INOD2))
                  SST = SST + SNODI(INOD2)*VOLNOD(INOD2)*
     &                     (PNEW(INOD2)-PTIMEP(INOD2)) 
CM   &                    +PNODI(INOD2)*VOLNOD(INOD2)*
CM   &                     (SWNEW(INOD2)-SWTIMEP(INOD2))
               ELSE
                  STOR = STOR+VOLNOD(INOD1)*(SNODI(INOD1)*PNEW(INOD1)+
     &                   PNODI(INOD1)*SWNEW(INOD1))
                  SST = SST + SNODI(INOD1)*VOLNOD(INOD1)*
     &                     (PNEW(INOD1)-PTIMEP(INOD1)) 
CM   &                    +PNODI(INOD1)*VOLNOD(INOD1)*
CM   &                     (SWNEW(INOD1)-SWTIMEP(INOD1))
               END IF
            ELSE IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).LT.ZERO
     &              .AND.FLAG(I).EQ.0) THEN
               FLAG(I)=1
CM             RC=(Z(INOD1)-Z(INOD2))/(PNEW(INOD1)-PNEW(INOD2))
               RC=ONE
               SST = SST + SNODI(INOD1)*VOLNOD(INOD1)*RC*
     &                   (PNEW(INOD1)-PTIMEP(INOD1))  
CM   &                  +PNODI(INOD1)*VOLNOD(INOD1)*RC*
CM   &                   (SWNEW(INOD1)-SWTIMEP(INOD1))
CM             MWT = MWT + PNODI(INOD1)*
CM   &                   (PNEW(INOD1)-PTIMEP(INOD1))*ARENOD(I)
               STOR = STOR+RC*VOLNOD(INOD1)*(SNODI(INOD1)*PNEW(INOD1)+
     &                  PNODI(INOD1)*SWNEW(INOD1))
CM             IF (WNOD(INOD1).LE.ZERO) THEN
                  RECNOD(I)=-1.0d0*WNOD(INOD1)*ARENOD(I)
CM                RECFLOW=RECFLOW+RECNOD(I)
CM             END IF
CM Perched water table
            ELSE IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).LT.ZERO
     &         .AND.FLAG(I).GT.0) THEN
               FLAG(I)=FLAG(I)+1
CM Totally unsaturated soil column 
            ELSE IF (J.EQ.1.AND.FLAG(I).EQ.0) THEN
               FLAG(I)=-2
            END IF
         END DO
         TWS(I) = TWS(I) + SWNEW(INOD2)*VOLNOD(INOD2)*PNODI(INOD2)
         RECFLOW=RECFLOW+RECNOD(I)
         TSTOR = TSTOR + TWS(I)
      END DO
      RECVOL=RECVOL+RECFLOW*DELTAT
      SST=SST/DELTAT
      MWT=MWT/DELTAT
      IF (TIME.NE.0.0d0) WRITE(333,*)TIME,SST,MWT,STOR,TSTOR
      RETURN
      END
