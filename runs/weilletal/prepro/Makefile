#
# $Id: Makefile,v 1.3 2006/06/16 15:25:50 ghattas Exp $
# #
#
# NETCDF library
NETCDF          = $(HOME)/utils/netcdf-4.0
NETCDF_INCLUDE  = $(NETCDF)/include
NETCDF_LIBRARY  = -L$(NETCDF)/lib -lnetcdf -lnetcdff
# Fortran compiler
F90             = gfortran
LD              = gfortran
F90FLAGS        = -g -fno-align-commons -fdefault-real-8 -I$(NETCDF_INCLUDE)
FLIBS     = $(NETCDF_LIBRARY)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#
#
OBJ_M1 = mpar.o

OBJ_M2 = mbbio.o

OBJ_M3 = wbb_sr.o

OBJ_M4 = csort.o

OBJ_M5 = qsort.o

OBJ_M6 = wbb_sr.o

OBJ_M7 = dsf.o

OBJ_M8 = facet.o

OBJ_M7 = hg.o

OBJ_M8 = mrbb_sr.o

OBJ_M9 = bb2ncdf_sr.o

#OBJ_M10 = bb2shp_sr.o

#OBJ_M11 = shape.o

#OBJ_M12 = dbase.o

#OBJ_M13 = streamer.o
#
#
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: cppp_test
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
cppp_test:  $(OBJ_M1) $(OBJ_M2) $(OBJ_M3) $(OBJ_M4) $(OBJ_M5) $(OBJ_M6) $(OBJ_M7) $(OBJ_M8) $(OBJ_M9) cppp.o Makefile 
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M1) $(OBJ_M2) $(OBJ_M3) $(OBJ_M4) $(OBJ_M5) $(OBJ_M6) $(OBJ_M7) $(OBJ_M8) $(OBJ_M9) cppp.o $(FLIBS)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
mpar.o :	mpar.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c mpar.f90
mbbio.o :	mbbio.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c mbbio.f90
wbb_sr.o :	wbb_sr.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c wbb_sr.f90
csort.o :	csort.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c csort.f90
qsort.o :	qsort.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c qsort.f90
dsf.o :	dsf.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c dsf.f90
facet.o :	facet.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c facet.f90
hg.o :	hg.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c hg.f90
mrbb_sr.o :	mrbb_sr.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c mrbb_sr.f90
bb2ncdf_sr.o :	bb2ncdf_sr.f90 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c bb2ncdf_sr.f90
#bb2shp_sr.o :	bb2shp_sr.f90 
#	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c bb2shp_sr.f90
#shape.o :	shape.f90 
#	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c shape.f90
#dbase.o :	dbase.f90 
#	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c dbase.f90
#streamer.o :	streamer.f90 
#	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c streamer.f90
cppp.o :	cppp.f90 Makefile 
	$(F90) $(F90FLAGS) $(NETCDF_LIB) -c cppp.f90
#
#-------------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------------
#
help: 
	more Make.help
#
# Clean directory
#
clean:
	-rm -f *.o *.x *.mod *.MOD 
	-rm -f i.*.F90 *.L 
	-rm -f bb2ncdf bb2shp cbb ha mrbb rbb rn rrbb sa test_ncdf wbb wpar
	-rm -f core core.*
