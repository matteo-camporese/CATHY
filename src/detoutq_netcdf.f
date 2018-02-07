C
C************************** DETOUTQ_NETCDF *****************************
C 
C  netCDF OUTPUT OF THE DISCHARGE AT THE OUTLET OF THE BASIN AND IN THE 
C  OTHER CELLS SUCH AS SELECTED IN PARM INPUT FILE 
C
C***********************************************************************
C
      SUBROUTINE DETOUTQ_NETCDF(NCELNL,NUM_QOUT,NSTEP,TIME,ID_QOUT,
     1     QOI_SN,Q_IN_KKP1_SN,Q_OUT_KKP1_SN_1)

      IMPLICIT NONE   
      INCLUDE 'CATHY.H'
      INCLUDE 'netcdf.inc'
      CHARACTER(LEN=5) CHOUT
      CHARACTER(LEN=100) STRNOUT
      INTEGER I,NUM_QOUT,NCELNL,NSTEP
      INTEGER STATUS,FID,DIMIDS(2),NCVARID(NUM_QOUT+2)
      INTEGER ID_QOUT(MAXQOUT),QOI_SN(MAXCEL)
      REAL*8  TIME
      REAL*8  Q_OUTPUT(MAXQOUT)
      REAL*8  Q_IN_KKP1_SN(MAXCEL)
      REAL*8  Q_OUT_KKP1_SN_1(MAXCEL)

C     WRITING STATEMENT DIFFERENT IF THE CONSIDERED CELL IS THE OUTLET OR NOT.
C     FOR THE INTERNAL CELLS OF THE BASIN WE'RE 'OBLIGED' TO CONSDER 
C     THE Q_IN_KKP1_SN IN CASE THERE IS DISPERSION IN THE FLUXES        

      DO I=1,NUM_QOUT
             Q_OUTPUT(I)=Q_OUT_KKP1_SN_1(ID_QOUT(I))
      END DO

      IF (NSTEP .EQ. 1) THEN
          STATUS = NF_CREATE("cathy_netcdf_qout.nc",NF_WRITE,FID)
          STATUS = NF_DEF_DIM(FID,"NSTEP",NF_UNLIMITED,DIMIDS(1))
          STATUS = NF_DEF_DIM(FID,"NQOUT",NUM_QOUT,DIMIDS(2))
          STATUS = NF_DEF_VAR(FID,"TIME",NF_DOUBLE,1,DIMIDS(1),
     1             NCVARID(1))
          STATUS = NF_DEF_VAR(FID,"QOUTLET",NF_DOUBLE,1,DIMIDS(1),
     1             NCVARID(2))
          DO I = 1,NUM_QOUT
               WRITE(CHOUT,'(I4)') ID_QOUT(I) !CMS This write statement
                                              ! has to be improved
               STRNOUT = "QOUT_ID"//"_"//TRIM(CHOUT)
               STATUS = NF_DEF_VAR(FID,TRIM(STRNOUT),NF_DOUBLE,1,
     1                  DIMIDS(1),NCVARID(2+I))
          END DO
          STATUS = NF_ENDDEF(FID)
          STATUS = NF_CLOSE(FID)
      END IF

      STATUS = NF_OPEN("cathy_netcdf_qout.nc",NF_WRITE,FID)
      STATUS = NF_INQ_VARID(FID,"TIME",NCVARID(1))
      STATUS = NF_INQ_VARID(FID,"QOUTLET",NCVARID(2))
      DO I = 1,NUM_QOUT
           WRITE(CHOUT,'(I4)') ID_QOUT(I) !CMS This write statement
                                          ! has to be improved
           STRNOUT = "QOUT_ID"//"_"//TRIM(CHOUT)
           STATUS = NF_INQ_VARID(FID,TRIM(STRNOUT),NCVARID(2+I))
      END DO

      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(1),(/NSTEP/),
     1          (/1/),TIME)
      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(2),(/NSTEP/),
     1          (/1/),Q_OUT_KKP1_SN_1(QOI_SN(NCELNL)))

!      STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(3),(/NSTEP/),
!     1          (/1/),Q_OUT_KKP1_SN_1(QOI_SN(NCELNL-1)))
      DO I = 1,NUM_QOUT
         STATUS = NF_PUT_VARA_DOUBLE(FID,NCVARID(2+I),(/NSTEP/),
     1          (/1/),Q_OUTPUT(I))
      END DO
      STATUS = NF_CLOSE(FID)


      RETURN
      END
