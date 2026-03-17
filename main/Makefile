SHELL=/bin/csh

# Makefile for e.bindex.F
##############################################################
FC      = ifort 
FFLAGS  = -assume byterecl -mcmodel medium -shared-intel -I/usr/hdf5/1.10.5_intel19/include -I/usr/zlib/1.2.11/include -I/usr/netcdf/4.6.1_intel19/include -I/usr/intel/compilers_and_libraries_2020.4.304/linux/compiler/include
LIBS    = -L/usr/zlib/1.2.11/lib -lm -lz -L/usr/hdf5/1.10.5_intel19/lib -lhdf5 -lhdf5_hl -L/usr/netcdf/4.6.1_intel19/lib -lnetcdf -lnetcdff -L/usr/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin
##############################################################
CMD  =  bindex.exe
HDRS =  qq.h 
OBJS =  e.bindex.o areacalc.o readnetcdf.o \
        read_threshold.o datetrack.o anomaly.o ccl.o  \
				spatialthresh.o labelreorder.o GHG.o overlap.o \
				nlabelreorder.o stat3.o durthresh.o \
				smfdist.o sfdist.o mfdist.o ddist.o wddist.o sddist.o \
				bdate.o wbdate.o center.o wcenter.o scenter.o \
				center_dur.o wcenter_dur.o scenter_dur.o center_dur_date.o \
				center_whole.o center_whole_date.o wavebreak.o wwavebreak.o 
##############################################################
$(CMD):         $(OBJS) $(HDRS)
	$(FC) -o $@ $(FFLAGS) -Wl,-rpath=/usr/intel/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin/ $(OBJS) $(LIBS)
##############################################################
e.bindex.o: $(HDRS)
areacalc.o: $(HDRS)
readnetcdf.o: $(HDRS)
read_threshold.o: $(HDRS)
datetrack.o: $(HDRS)
anomaly.o: $(HDRS)
ccl.o: $(HDRS)
spatialthresh.o: $(HDRS)
labelreorder.o: $(HDRS)
GHG.o: $(HDRS)
overlap.o: $(HDRS)
nlabelreorder.o: $(HDRS)
stat3.o: $(HDRS)
durthresh.o: $(HDRS)
smfdist.o: $(HDRS)
sfdist.o: $(HDRS)
mfdist.o: $(HDRS)
ddist.o: $(HDRS)
wddist.o: $(HDRS)
sddist.o: $(HDRS)
bdate.o: $(HDRS)
wbdate.o: $(HDRS)
center.o: $(HDRS)
wcenter.o: $(HDRS)
scenter.o: $(HDRS)
center_dur.o: $(HDRS)
wcenter_dur.o: $(HDRS)
scenter_dur.o: $(HDRS)
center_dur_date.o: $(HDRS)
center_whole.o: $(HDRS)
center_whole_date.o: $(HDRS)
wavebreak.o: $(HDRS)
wwavebreak.o: $(HDRS)
##############################################################
clean:
	$(RM) $(OBJS)
