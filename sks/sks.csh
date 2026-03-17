#!/bin/csh -f
# 1) Run SKSanomaly.F
#    • output: anomaly(.bin) , HYB threshold(.txt)
#
# 2) Post-processing
#    • Convert Binary → NetCDF via CTL info
#    • Generate RHYB threshold file using CDO


set infile1 = z500.day.19790101.20250228.noleap

# Anomaly data ////////////////////////////////////////

set outfile1a = SKS.z500
set outfile1b = thresholds.SKS.z500
set outfile2  = zbar
set outfile3  = zstar
set outfile4  = zhat

# Directories ////////////////////////////////////////////
set InDir   =  /data03/share/art_midlat/blocking_code/hynoh_202506
set DatDir  =  /data03/share/art_midlat/blocking_code/hynoh_202506/sks
set SrcDir  =  /data03/share/art_midlat/blocking_code/hynoh_202506/sks
cd $DatDir
rm -Rf ${outfile1a}.bin
rm -Rf ${outfile1b}.txt
rm -Rf ${outfile2}.bin
rm -Rf ${outfile3}.bin
rm -Rf ${outfile4}.bin
cd $SrcDir
rm -Rf data*
rm -Rf fort.*
rm -Rf *.exe
#######################################################
ln -fs ${InDir}/${infile1}.nc       data1.nc
#Output
ln -fs ${DatDir}/${outfile1a}.bin    fort.11
ln -fs ${DatDir}/${outfile1b}.txt    fort.31
#
# ln -fs ${DatDir}/${outfile2}.bin     fort.41
#
# ln -fs ${DatDir}/${outfile3}.bin     fort.51
#
# ln -fs ${DatDir}/${outfile4}.bin     fort.61
#######################################################
#linking subroutines and compiling
make
# run the program
./SKSanomaly.exe
#
make clean
rm -f fort.* data*.nc *.exe
echo ">>> Finished SKSanomay.exe"

# ===== Convert SKS.z500.bin to NetCDF =====
cd $DatDir
set ix        = 360
set il        = 181
set latres    = 1.0
set lonres    = 1.0
set lowlat    = -90.0
set syear     = 1979
set maxday    = 16849

set ctlfile = SKS.z500.ctl

cat >! $ctlfile << EOF
DSET ^${outfile1a}.bin
OPTIONS 365_day_calendar
TITLE blocking
UNDEF 9999999
XDEF ${ix} LINEAR 0 ${lonres}
YDEF ${il} LINEAR ${lowlat} ${latres}
ZDEF 1 LEVELS  500
TDEF ${maxday} LINEAR 00Z01jan${syear} 1DY
VARS 1
hgt 1 0 hgt_test
ENDVARS
EOF

cdo -b F64 -f  nc import_binary $ctlfile ${outfile1a}.nc
cdo monmean ${outfile1a}.nc imsi.nc 
cdo ymonstd imsi.nc cdo.monmean.ymonstd.nc 
rm -f imsi.nc 
echo ">>> Finished Post-processing"

