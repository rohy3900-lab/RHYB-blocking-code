#!/bin/csh -f
# 1) Run bindex.exe
#
# 2) Post-processing
#    • Convert Binary → NetCDF via CTL info

set infile1a = SKS.z500

set infile1c = z500.day.19790101.20250228.noleap
set infile1d = cdo.monmean.ymonstd
# Blocking Frequency distributions ////////////////////////////
#freq 40
set outfile1a = bf.season
set outfile2a = bf.season.yr
set outfile3a = onset.bf.djf
set outfile4a = bf.mon
set outfile5a = bf.mon.yr
set outfile6a = bf.ann.yr
set outfile7a = bf.jja
#label 60
set outfile1c = label
set outfile2c = label.djf
set outfile3c = onset.label.djf
set outfile4c = label.jja
#pos 70
set outfile1d = relabel
set outfile2d = allpos
set outfile3d = center
set outfile4d = relabel.NP
set outfile5d = allpos.NP
set outfile6d = center.NP
set outfile7d = relabel.NP.whole
set outfile8d = allpos.NP.whole
set outfile9d = center.NP.whole
#label by wbi 80
set outfile1e = c.label
set outfile2e = a.label
set outfile3e = n.label
set outfile4e = c.label.djf
set outfile5e = a.label.djf
set outfile6e = n.label.djf
#freq by wbi 90
set outfile1f = c.bf.ss
set outfile2f = a.bf.ss
set outfile3f = n.bf.ss
set outfile4f = c.bf.djf
set outfile5f = a.bf.djf
set outfile6f = n.bf.djf
#dur 100
set outfile1g  = dur
set outfile2g  = dur.NP.whole
set outfile3g  = dur.NP
set outfile4g  = bday.NP.whole
set outfile5g  = bdate.NP.whole
set outfile6g  = bday.NP
set outfile7g  = bdate.NP
set outfile8g  = Lag.BAI.NP
set outfile9g  = Eu.BAI.NP
set outfile10g = bday
set outfile11g = bdate
#onsetdate 110
set outfile1h = onsetdate
set outfile2h = enddate
set outfile3h = onsetdate.NP.whole
set outfile4h = enddate.NP.whole
set outfile5h = onsetdate.NP
set outfile6h = enddate.NP
# Directories ////////////////////////////////////////////////
set InDir   =  /data03/share/art_midlat/blocking_code/hynoh_202506
set InDir2  =  /data03/share/art_midlat/blocking_code/hynoh_202506/sks
set DatDir  =  /data03/share/art_midlat/blocking_code/hynoh_202506/RHYB/output
set SrcDir  =  /data03/share/art_midlat/blocking_code/hynoh_202506/RHYB

        # Ensure Data directory exists
        if ( ! -d $DatDir ) then
            echo "Directory $DatDir not found — creating it."
            mkdir -p $DatDir
        endif

cd $DatDir
rm -Rf ${outfile1a}.bin
rm -Rf ${outfile2a}.bin
rm -Rf ${outfile3a}.bin
rm -Rf ${outfile4a}.bin
rm -Rf ${outfile5a}.bin
rm -Rf ${outfile6a}.bin
rm -Rf ${outfile7a}.bin
rm -Rf ${outfile1c}.bin
rm -Rf ${outfile2c}.bin
rm -Rf ${outfile3c}.bin
rm -Rf ${outfile4c}.bin
rm -Rf ${outfile1d}.bin
rm -Rf ${outfile2d}.bin
rm -Rf ${outfile3d}.bin
rm -Rf ${outfile4d}.bin
rm -Rf ${outfile5d}.bin
rm -Rf ${outfile6d}.bin
rm -Rf ${outfile7d}.bin
rm -Rf ${outfile8d}.bin
rm -Rf ${outfile9d}.bin
rm -Rf ${outfile1e}.bin
rm -Rf ${outfile2e}.bin
rm -Rf ${outfile3e}.bin
rm -Rf ${outfile4e}.bin
rm -Rf ${outfile5e}.bin
rm -Rf ${outfile6e}.bin
rm -Rf ${outfile1f}.bin
rm -Rf ${outfile2f}.bin
rm -Rf ${outfile3f}.bin
rm -Rf ${outfile4f}.bin
rm -Rf ${outfile5f}.bin
rm -Rf ${outfile6f}.bin
rm -Rf ${outfile1g}.txt
rm -Rf ${outfile2g}.txt
rm -Rf ${outfile3g}.txt
rm -Rf ${outfile4g}.txt
rm -Rf ${outfile5g}.txt
rm -Rf ${outfile6g}.txt
rm -Rf ${outfile7g}.txt
rm -Rf ${outfile8g}.bin
rm -Rf ${outfile9g}.bin
rm -Rf ${outfile10g}.txt
rm -Rf ${outfile11g}.txt
rm -Rf ${outfile1h}.txt
rm -Rf ${outfile2h}.txt
rm -Rf ${outfile3h}.txt
rm -Rf ${outfile4h}.txt
rm -Rf ${outfile5h}.txt
rm -Rf ${outfile6h}.txt
cd $SrcDir
rm -Rf *.nc
rm -Rf fort.*
rm -Rf *.exe
#######################################################
ln -fs ${InDir2}/${infile1a}.bin     fort.11
ln -fs ${InDir}/${infile1c}.nc      data1.nc
ln -fs ${InDir2}/${infile1d}.nc      std1.nc
#Output
# 40
# ln -fs ${DatDir}/${outfile1a}.bin    fort.51
# ln -fs ${DatDir}/${outfile2a}.bin    fort.52
# ln -fs ${DatDir}/${outfile3a}.bin    fort.53
# ln -fs ${DatDir}/${outfile4a}.bin    fort.54
ln -fs ${DatDir}/${outfile5a}.bin    fort.55
# ln -fs ${DatDir}/${outfile6a}.bin    fort.56
# ln -fs ${DatDir}/${outfile7a}.bin    fort.57
# 60
# ln -fs ${DatDir}/${outfile1c}.bin    fort.71
# ln -fs ${DatDir}/${outfile2c}.bin    fort.72
# ln -fs ${DatDir}/${outfile3c}.bin    fort.73
# ln -fs ${DatDir}/${outfile4c}.bin    fort.74
# 70
ln -fs ${DatDir}/${outfile1d}.bin    fort.81
# ln -fs ${DatDir}/${outfile2d}.bin    fort.82
# ln -fs ${DatDir}/${outfile3d}.bin    fort.83
ln -fs ${DatDir}/${outfile4d}.bin    fort.84
# ln -fs ${DatDir}/${outfile5d}.bin    fort.85
# ln -fs ${DatDir}/${outfile6d}.bin    fort.86
# ln -fs ${DatDir}/${outfile7d}.bin    fort.87
# ln -fs ${DatDir}/${outfile8d}.bin    fort.88
# ln -fs ${DatDir}/${outfile9d}.bin    fort.89
#80
# ln -fs ${DatDir}/${outfile1e}.bin    fort.91
# ln -fs ${DatDir}/${outfile2e}.bin    fort.92
# ln -fs ${DatDir}/${outfile3e}.bin    fort.93
# ln -fs ${DatDir}/${outfile4e}.bin    fort.94
# ln -fs ${DatDir}/${outfile5e}.bin    fort.95
# ln -fs ${DatDir}/${outfile6e}.bin    fort.96
#90
# ln -fs ${DatDir}/${outfile1f}.bin    fort.101
# ln -fs ${DatDir}/${outfile2f}.bin    fort.102
# ln -fs ${DatDir}/${outfile3f}.bin    fort.103
# ln -fs ${DatDir}/${outfile4f}.bin    fort.104
# ln -fs ${DatDir}/${outfile5f}.bin    fort.105
# ln -fs ${DatDir}/${outfile6f}.bin    fort.106
# 100
# ln -fs ${DatDir}/${outfile1g}.txt    fort.111
# ln -fs ${DatDir}/${outfile2g}.txt    fort.112
# ln -fs ${DatDir}/${outfile3g}.txt    fort.113
# ln -fs ${DatDir}/${outfile4g}.txt    fort.114
# ln -fs ${DatDir}/${outfile5g}.txt    fort.115
ln -fs ${DatDir}/${outfile6g}.txt    fort.116
ln -fs ${DatDir}/${outfile7g}.txt    fort.117
# ln -fs ${DatDir}/${outfile8g}.bin    fort.118
# ln -fs ${DatDir}/${outfile9g}.bin    fort.119
ln -fs ${DatDir}/${outfile10g}.txt   fort.120
ln -fs ${DatDir}/${outfile11g}.txt   fort.921
# 110
ln -fs ${DatDir}/${outfile1h}.txt    fort.121
# ln -fs ${DatDir}/${outfile2h}.txt    fort.122
# ln -fs ${DatDir}/${outfile3h}.txt    fort.123
# ln -fs ${DatDir}/${outfile4h}.txt    fort.124
ln -fs ${DatDir}/${outfile5h}.txt    fort.125
# ln -fs ${DatDir}/${outfile6h}.txt    fort.126

#######################################################
#linking subroutines
make
# run the program
./bindex.exe
#
make clean
rm -f fort.* *.nc *.exe

# ===== Convert binaries to NetCDF ===============================
cd $DatDir
set ix        = 360
set il        = 181
set latres    = 1.0
set lonres    = 1.0
set lowlat    = -90.0
set syear     = 1979
set nmon      = 554
set maxday    = 16849

# 1) outfile5a = bf.mon.yr
set ctlfile1 = ${outfile5a}.ctl

cat >! $ctlfile1 << EOF
DSET ^${outfile5a}.bin
TITLE monthly blocking frequency
UNDEF 9999999
XDEF ${ix} LINEAR 0 ${lonres}
YDEF ${il} LINEAR ${lowlat} ${latres}
ZDEF 1   LEVELS 500
TDEF ${nmon} LINEAR 00Z01jan${syear} 1MO
VARS 1
dfreq 1 0 Avreage frequency distribution
ENDVARS
EOF

cdo -f nc import_binary $ctlfile1 ${outfile5a}.nc

# 2) outfile4d = relabel.NP
set ctlfile2 = ${outfile4d}.ctl

cat >! $ctlfile2 << EOF
DSET ^${outfile4d}.bin
TITLE blocking 
UNDEF 9999999
XDEF ${ix} LINEAR 0 ${lonres}
YDEF ${il} LINEAR ${lowlat} ${latres}
ZDEF 1   LEVELS 500
TDEF ${maxday} LINEAR 00Z01jan${syear} 1DY
VARS 1
hgt 1 0 hgt_test
ENDVARS
EOF

cdo -f nc import_binary $ctlfile2 ${outfile4d}.nc