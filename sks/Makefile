SHELL=/bin/csh

# Makefile for SKSanomaly.F
##############################################################
FC      = ifort 
FFLAGS  = -assume byterecl -mcmodel medium -shared-intel -I/usr/hdf5/1.10.5_intel19/include -I/usr/zlib/1.2.11/include -I/usr/netcdf/4.6.1_intel19/include -I/usr/intel/compilers_and_libraries_2020.4.304/linux/compiler/include
LIBS    = -L/usr/zlib/1.2.11/lib -lm -lz -L/usr/hdf5/1.10.5_intel19/lib -lhdf5 -lhdf5_hl -L/usr/netcdf/4.6.1_intel19/lib -lnetcdf -lnetcdff -L/usr/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin
##############################################################
CMD  =  SKSanomaly.exe
HDRS =  qq.h 
OBJS =  SKSanomaly.o datetrack.o readnetcdf.o
##############################################################
$(CMD):         $(OBJS) $(HDRS)
	$(FC) -o $@ $(FFLAGS) -Wl,-rpath=/usr/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin/ $(OBJS) $(LIBS)
##############################################################
SKSanomaly.o: $(HDRS)
datetrack.o: $(HDRS)
readnetcdf.o: $(HDRS)
##############################################################
clean:
	$(RM) $(OBJS)
