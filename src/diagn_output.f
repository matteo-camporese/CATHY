C
C******************************* DIAGN_OUTPUT ********************************
C
C     Write diagnostic output: subsurface storage (total, saturated, and
C     unsaturated) and ponding storage in a netCDF files
C
C*****************************************************************************
C

      SUBROUTINE DIAGN_OUTPUT(KPRT,NPRT,TIME,N,NNOD,NSTR,BASE,ZRATIO,Z,
     1           PNODI,ARENOD,VOLNOD,SW,PONDNOD,PNEW)

      IMPLICIT NONE
      INTEGER  I,J
      INTEGER  N,NNOD,NSTR,NSTEP,NPRT,KPRT
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

!CMS  Calculate the saturated and unsaturated storage
      ZERO = 0.0d0
      SAT_STOR   = ZERO
      UNSAT_STOR = ZERO
      DO I=1,N
           IF (PNEW(I).GE.ZERO) THEN
             SAT_STOR = SAT_STOR + SW(I)*VOLNOD(I)*PNODI(I)
           ELSE IF (PNEW(I).LT.ZERO) THEN
             UNSAT_STOR = UNSAT_STOR + SW(I)*VOLNOD(I)*PNODI(I)
           END IF
      END DO

!CMS  Calculate the ponding storage
      POND_STOR = ZERO
      DO I = 1,NNOD
          POND_STOR = POND_STOR + PONDNOD(I)*ARENOD(I)
      END DO

!CMS  Write NetCDF file
      IF (KPRT .EQ. 0) THEN
         STATUS = NF_CREATE("cathy_netcdf_diagn.nc",NF_WRITE,FID)
         STATUS = NF_DEF_DIM(FID,"T",NPRT+1,DIMIDS(1))
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

      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(1),(/KPRT+1/),
     1          (/1/),TIME)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(2),(/KPRT+1/),
     1          (/1/),TOT_STOR)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(3),(/KPRT+1/),
     1          (/1/),SAT_STOR)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(4),(/KPRT+1/),
     1          (/1/),UNSAT_STOR)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(5),(/KPRT+1/),
     1          (/1/),POND_STOR)
      STATUS = NF_CLOSE(FID)

      RETURN

      END
