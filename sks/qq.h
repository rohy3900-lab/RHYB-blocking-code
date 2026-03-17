      ! Global model parameters
      parameter (ix=360,iy=91,il=181,lim=16849,maxdaylim=16849) 
      parameter (nyr=47,nyear=1979,nmon=nyr*12)
      parameter (llim=121,ulim=181) ! threshold limits
      real:: latres,lonres,lowlat         
      parameter (latres=1.0,lonres=1.0,lowlat=-90.0)
      ! Model Specific parameters
      ! maxday= length of data in days
      ! yrtype=1 => lpyr,  yrtype=2 => no lpyr,  yrtype=3 => 360 day calendar 
      parameter (maxday1=16849,yrtype1=2)
      parameter (maxday2=14610,yrtype2=1)
      parameter (maxday3=14600,yrtype3=2)
      parameter (maxday4=14600,yrtype4=2)
      parameter (maxday5=14363,yrtype5=3)
      parameter (maxday6=14610,yrtype6=1)
      parameter (maxday7=14600,yrtype7=2)
      parameter (maxday8=14600,yrtype8=2)
      parameter (maxday9=14600,yrtype9=2)
      parameter (maxday10=14610,yrtype10=1)
      parameter (maxday11=14610,yrtype11=1)
      parameter (maxday12=14600,yrtype12=2)
      parameter (maxday13=14600,yrtype13=2)
      parameter (maxday14=14610,yrtype14=1)
      parameter (maxday15=14600,yrtype15=2)
      parameter (maxday16=14610,yrtype16=1)
      parameter (maxday17=14600,yrtype17=2)
      parameter (maxday18=14610,yrtype18=1)
