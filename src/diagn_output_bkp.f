C
C******************************* DIAGN_OUTPUT ********************************
C
C     Write diagnostic output: subsurface storage (total, saturated, and
C     unsaturated) and ponding storage in a netCDF files
C
C*****************************************************************************
C

      SUBROUTINE DIAGN_OUTPUT(NSTEP,TIME,N,NNOD,NSTR,BASE,ZRATIO,Z,PNODI
     1           ,ARENOD,VOLNOD,SW,PONDNOD,PNEW)

      IMPLICIT NONE
      INTEGER  I,J
      INTEGER  N,NNOD,NSTR,NSTEP
      INTEGER  INOD1,INOD2,FLAG(NNOD)
      INTEGER  STATUS,FID,DIMIDS(1),NCVARID(5)
      REAL*8   BASE,RC,TIME,ZERO
      REAL*8   TOT_STOR,SAT_STOR,UNSAT_STOR,POND_STOR
      REAL*8   WT(NNOD),ZCUM(NSTR + 1)
      REAL*8   ZRATIO(*),Z(*),PNODI(*),ARENOD(*),VOLNOD(*)
      REAL*8   SW(*),PONDNOD(*),PNEW(*)

      INCLUDE   'CATHY.H'
      INCLUDE   'netcdf.inc'

!CMS  Calculate the cumulative z from the ground
!CMS      ZCUM(0) = 0
!CMS      DO I = 1,NSTR
!CMS        ZCUM(I) = ZCUM(I-1) + ZRATIO(I)*BASE
!CMS      END DO

!CMS  Calculate the total storage      
      TOT_STOR = 0.0D0
      DO I=1,N
         TOT_STOR = TOT_STOR + SW(I)*VOLNOD(I)*PNODI(I)
      END DO

!CMS  Calculate the water table depth
      ZERO=0.0d0
      DO I=1,NNOD
         FLAG(I)= 0
         WT(I)  = ZERO
      END DO

      DO I=1,NNOD
        DO J=NSTR,1,-1
           INOD1=I+J*NNOD
           INOD2=I+(J-1)*NNOD
        IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).LT.ZERO
     &     .AND. FLAG(I).EQ. 0) THEN
           RC=(Z(INOD1)-Z(INOD2))/(PNEW(INOD1)-PNEW(INOD2))
           WT(I)=(Z(I)-Z(INOD1))+RC*PNEW(INOD1)
           FLAG(I)=1
        ELSE IF (PNEW(INOD1).GE.ZERO.AND.PNEW(INOD2).LT.ZERO
     &     .AND. FLAG(I).EQ. 1) THEN
           FLAG(I)=2
        ELSE IF (J.EQ.1.AND.PNEW(INOD2).GE.ZERO
     &     .AND.FLAG(I).EQ.0) THEN
           FLAG(I)=3
           WT(I)= Z(I)-Z(INOD2)
        ELSE IF (J.EQ.1.AND.FLAG(I).EQ.0) THEN
           FLAG(I)=4
           WT(I)=BASE
        END IF
        END DO
      END DO

!CMS  Calculate the saturated and unsaturated storage
!CMS  I assume homogeneity in the porosity for now!
      DO I = 1,NNOD
          SAT_STOR = SAT_STOR + (BASE - WT(I))*PNODI(I)*ARENOD(I)
      END DO
      
      UNSAT_STOR  = TOT_STOR - SAT_STOR

!CMS  Calculate the ponding storage
      DO I = 1,NNOD
          POND_STOR = POND_STOR + PONDNOD(I)*ARENOD(I)
      END DO

!CMS  Write NetCDF file
      IF (NSTEP .EQ. 1) THEN
         STATUS = NF_CREATE("cathy_netcdf_diagn.nc",NF_WRITE,FID)
         STATUS = NF_DEF_DIM(FID,"NSTEP",NF_UNLIMITED,DIMIDS(1))
         STATUS = NF_DEF_VAR(FID,"TIME",NF_DOUBLE,1,DIMIDS(1),
     1            NCVARID(1))
         STATUS = NF_DEF_VAR(FID,"TOT_STOR",NF_DOUBLE,1,DIMIDS(1),
     1            NCVARID(2))
         STATUS = NF_DEF_VAR(FID,"SAT_STOR",NF_DOUBLE,1,DIMIDS(1),
     1            NCVARID(3))
         STATUS = NF_DEF_VAR(FID,"UNSAT_STOR",NF_DOUBLE,1,DIMIDS(1),
     1            NCVARID(4))
         STATUS = NF_DEF_VAR(FID,"POND_STOR",NF_DOUBLE,1,DIMIDS(1),
     1            NCVARID(5))
         STATUS = NF_ENDDEF(FID)
         STATUS = NF_CLOSE(FID)
      END IF

      STATUS = NF_OPEN("cathy_netcdf_diagn.nc",NF_WRITE,FID)
      STATUS = NF_INQ_VARID(FID,"TIME",NCVARID(1))
      STATUS = NF_INQ_VARID(FID,"TOT_STOR",NCVARID(2))
      STATUS = NF_INQ_VARID(FID,"SAT_STOR",NCVARID(3))
      STATUS = NF_INQ_VARID(FID,"UNSAT_STOR",NCVARID(4))
      STATUS = NF_INQ_VARID(FID,"POND_STOR",NCVARID(5))

      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(1),(/NSTEP/),
     1          (/1/),TIME)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(2),(/NSTEP/),
     1          (/1/),TOT_STOR)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(3),(/NSTEP/),
     1          (/1/),SAT_STOR)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(4),(/NSTEP/),
     1          (/1/),UNSAT_STOR)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(5),(/NSTEP/),
     1          (/1/),POND_STOR)
      STATUS = NF_CLOSE(FID)

      RETURN

      END
