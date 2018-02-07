C
C************************** DETOUT_NETCDF ***********************************
C
C  Write PSI, SW, and PONDNOD output variables in a netCDF files
C
C****************************************************************************
C
      SUBROUTINE DETOUT_NETCDF(KPRT,NPRT,TIME,NROW,NCOL,NNOD,NSTR,PNEW,
     1                          SW,PONDNOD)

      IMPLICIT NONE

      INCLUDE   'CATHY.H'
      INCLUDE   'netcdf.inc'
      INTEGER   I,J,K,KK
      INTEGER   NROW,NCOL,NX,NY,STATUS
      INTEGER   KPRT,N,NNOD,NPRT,NSTR
      INTEGER   FID,DIMIDS(4),NCVARID(4)
      REAL*8    TIME
      REAL*8    PNEW(*),SW(*),PONDNOD(*)
      REAL*8    PSI3D(NCOL+1,NROW+1,NSTR+1)
      REAL*8    SW3D(NCOL+1,NROW+1,NSTR+1)
      REAL*8    PSI2D(NCOL+1,NROW+1)

      NX = NCOL + 1
      NY = NROW + 1

      IF (KPRT .EQ. 0) THEN
      STATUS = NF_CREATE("cathy_netcdf_out.nc",NF_WRITE,FID)
      STATUS = NF_DEF_DIM(FID,"X",NX,DIMIDS(1))
      STATUS = NF_DEF_DIM(FID,"Y",NY,DIMIDS(2))
      STATUS = NF_DEF_DIM(FID,"Z",NSTR+1,DIMIDS(3))
      STATUS = NF_DEF_DIM(FID,"T",NPRT+1,DIMIDS(4))
      STATUS = NF_DEF_VAR(FID,"TIME",NF_DOUBLE,1,DIMIDS(4),NCVARID(1))
      STATUS = NF_DEF_VAR(FID,"PSI",NF_DOUBLE,4,DIMIDS,NCVARID(2))
      STATUS = NF_DEF_VAR(FID,"SW",NF_DOUBLE,4,DIMIDS,NCVARID(3))
      STATUS = NF_DEF_VAR(FID,"PONDNOD",NF_DOUBLE,3,(/DIMIDS(1),
     1                          DIMIDS(2),DIMIDS(4)/),NCVARID(4))
      STATUS = NF_ENDDEF(FID)
      STATUS = NF_CLOSE(FID)
      END IF

      KK = 0
      DO K = 1,NSTR+1
         DO J = NY,1,-1
            DO I = 1,NX
               KK = KK + 1
               PSI3D(I,J,K) = PNEW(KK)
               SW3D(I,J,K)  = SW(KK)
            END DO
         END DO
      END DO

      KK = 0
      DO J = NY,1,-1
         DO I = 1,NX
            KK = KK + 1
            PSI2D(I,J) = PONDNOD(KK)
         END DO
      END DO

      STATUS = NF_OPEN("cathy_netcdf_out.nc",NF_WRITE,FID)
      STATUS = NF_INQ_VARID(FID,"TIME",NCVARID(1))
      STATUS = NF_INQ_VARID(FID,"PSI",NCVARID(2))
      STATUS = NF_INQ_VARID(FID,"SW",NCVARID(3))
      STATUS = NF_INQ_VARID(FID,"PONDNOD",NCVARID(4))
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(1),(/KPRT+1/),
     1          (/1/),TIME)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(2),(/1, 1, 1, KPRT + 1/),
     1          (/NX, NY, NSTR+1, 1/),PSI3D(:,:,:)) 
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(3),(/1, 1, 1, KPRT + 1/),
     1          (/NX, NY, NSTR+1, 1/),SW3D(:,:,:))
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(4),(/1, 1, KPRT + 1/),
     1          (/NX, NY, 1/),PSI2D(:,:))
      STATUS = NF_CLOSE(FID)

      END
