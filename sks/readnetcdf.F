      subroutine readnetcdf(f,maxday,yrtype,lmonth,z)

      ! Reads the netcdf file belonging to the file number

#include "qq.h"
#include "netcdf.inc"

      integer:: test,ncid,zid
      integer:: f,maxday,yrtype
      integer:: t,i,j
      integer, dimension(12):: lmonth
      real, dimension(ix,il,maxdaylim):: z

      if (f.EQ.11) then
       test = nf_open('data1.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
      ! If you use the file format of 'netCDF' or 'classic', 
      ! it would be good to revise the variable of z as follows.  
      ! test = nf_get_att_real(ncid,zid,'add_offset',offset)
      ! test = nf_get_att_real(ncid,zid,'scale_factor',factor)
      ! print*,offset, factor
      ! z = z*factor+offset
       print*,z(1:5,56,1)
       maxday=maxday1
       yrtype=yrtype1
       !print*,maxday,maxdaylim,ix,il
      elseif (f.EQ.12) then
       test = nf_open('data2.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'z',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday2
       yrtype=yrtype2
      elseif (f.EQ.13) then
       test = nf_open('data3.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday3
       yrtype=yrtype3
      elseif (f.EQ.14) then
       test = nf_open('data4.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday4
       yrtype=yrtype4
      elseif (f.EQ.15) then
       test = nf_open('data5.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday5
       yrtype=yrtype5
      elseif (f.EQ.16) then
       test = nf_open('data6.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday6
       yrtype=yrtype6
      elseif (f.EQ.17) then
       test = nf_open('data7.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday7
       yrtype=yrtype7
      elseif (f.EQ.18) then
       test = nf_open('data8.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday8
       yrtype=yrtype8
      elseif (f.EQ.19) then
       test = nf_open('data9.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday9
       yrtype=yrtype9
      elseif (f.EQ.20) then
       test = nf_open('data10.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday10
       yrtype=yrtype10
      elseif (f.EQ.21) then
       test = nf_open('data11.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday11
       yrtype=yrtype11
      elseif (f.EQ.22) then
       test = nf_open('data12.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday12
       yrtype=yrtype12
      elseif (f.EQ.23) then
       test = nf_open('data13.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday13
       yrtype=yrtype13
      elseif (f.EQ.24) then
       test = nf_open('data14.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday14
       yrtype=yrtype14
      elseif (f.EQ.25) then
       test = nf_open('data15.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday15
       yrtype=yrtype15
      elseif (f.EQ.26) then
       test = nf_open('data16.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday16
       yrtype=yrtype16
      elseif (f.EQ.27) then
       test = nf_open('data17.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday17
       yrtype=yrtype17
      elseif (f.EQ.28) then
       test = nf_open('data18.nc',nf_nowrite, ncid) ! data file id
       test = nf_inq_varid(ncid,'hgt',zid) ! get variable id
       test = nf_get_var_real(ncid,zid,z) ! get variable in file
       maxday=maxday18
       yrtype=yrtype18
      endif

      ! Initializing lmonth
      if (yrtype.EQ.3) then
       ! Note: Some files have 360 day calendar, 
       ! hence months are 30 days long
       do i=1,12
        lmonth(i)=30 
       enddo
      else ! if 365 day calendar with or without lpyr
      lmonth(1)=31
      lmonth(2)=28
      lmonth(3)=31
      lmonth(4)=30
      lmonth(5)=31
      lmonth(6)=30
      lmonth(7)=31
      lmonth(8)=31
      lmonth(9)=30
      lmonth(10)=31
      lmonth(11)=30
      lmonth(12)=31
      endif

      return
      end subroutine readnetcdf
