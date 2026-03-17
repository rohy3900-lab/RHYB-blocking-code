      program SKSanomaly

#include "qq.h"
#include "netcdf.inc"

      parameter (M=30,N=366) ! Days in a year and month
      
      ! Variables as in Sausen et al. 1995
      real, dimension(ix,il,maxdaylim):: z
      real, dimension(maxdaylim):: zbar
      real, dimension(maxdaylim):: zstar
      real, dimension(N):: zhat
      real, dimension(ix,il,maxdaylim):: zprime
      real, dimension(ix,il,maxdaylim):: zbar_2
      real, dimension(ix,il,maxdaylim):: zstar_2
      real, dimension(ix,il,N):: zhat_2
      integer:: c ! Leap year counter
      integer:: tau,nday
      integer:: f,i,j,t

      ! Date track variables
      integer:: iyear,iday,im,maxday,yrtype
      integer, dimension(12):: lmonth

      
      do f=11,11 ! File number
      
      ! Initilize for every file
      do t=1,maxdaylim
       do i=1,ix
        do j=1,il
         z(i,j,t)=0
         zprime(i,j,t)=0
         zbar_2(i,j,t)=0
         zstar_2(i,j,t)=0
        enddo
       enddo
      enddo
      zhat_2=0 

      ! Input
      call readnetcdf(f,maxday,yrtype,lmonth,z)


      ! Output
      open(f,form='unformatted',access='direct',recl=ix*il*4)
      open(f+30,form='unformatted',access='direct',recl=ix*il*4)
      open(f+40,form='unformatted',access='direct',recl=ix*il*4)
      open(f+50,form='unformatted',access='direct',recl=ix*il*4)


      ! *******************************************************************
      ! ANOMALY CALCULATION ***********************************************
      ! *******************************************************************
      do i=1,ix
       do j=iy,il ! equator to lat 90 deg

       ! Initialize for every new grid point
       do t=1,maxdaylim
        zbar(t)=0
        zstar(t)=0
       enddo
       do t=1,N
        zhat(t)=0
       enddo
 
c     Part(1): Calculating a running annual mean with special 
      ! treatment of the ends of the time time series
      do nday=(N/2)+1,maxday-(N/2)-1
        do k=nday-N/2,nday+N/2 
          zbar(nday)=zbar(nday)+ z(i,j,k)/(N+1)
        enddo 
      enddo

      ! End of time series points
      do nday=1,N/2            
         zbar(nday)=zbar((N/2)+1)
      enddo

      do nday=maxday-N/2,maxday
       zbar(nday)= zbar(nday-(N/2)-1)      
      enddo

c     Part(2): Calculating a running monthly mean of the anomaly
      !  relative to the annual mean found above 
      do nday=(M/2)+1,maxday-(M/2)-1
       do k=nday-M/2,nday+M/2
         zstar(nday)=zstar(nday)+ (z(i,j,k)-zbar(k))/(M+1)
       enddo
      enddo

      ! End of time series points
      do nday=1,M/2
        zstar(nday)=zstar(M/2+1)
      enddo

      do nday= maxday-M/2,maxday
        zstar(nday)=zstar(maxday-(M/2)-1)
      enddo

c     Part(3): Calculating a seasonal mean cycle using monthly 
      ! running mean 
      ! reintializing date track variables
      iyear=nyear
      iday=0 
      im=1
      c=0 ! Initializing leap year counter

      do nday=1,maxday
        call datetrack(lmonth,iday,iyear,im,yrtype)
        if ((i.eq.50).and.(j.eq.iy)) print*,nday,iyear,im,iday,yrtype
       ! recall iday = date (eg iday=22 => jan22)
       ! Using Julian date instead of calendar date for extra day in leap year calculation     
        if (iday.EQ.N) c=c+1  ! (if iday=366, count leap year day)
        zhat(iday)=zhat(iday)+zstar(nday)
      enddo


      do tau=1,N
       if ((tau.EQ.N).AND.(yrtype.EQ.1)) then ! (if iday=366)
        zhat(tau)=zhat(tau)/c
       else ! (if iday < 366)
        zhat(tau)=zhat(tau)/(iyear-nyear+1)
       endif
      enddo

c     Part(4): Calculating anomaly by extracting the annual mean
      !  and the seasonal cycle from the total in situ flow 
      ! reintializing datetrack variables
      iyear=nyear
      iday=0 
      im=1
         
      do nday=1,maxday
       call datetrack(lmonth,iday,iyear,im,yrtype)   
       zprime(i,j,nday)=z(i,j,nday)-zbar(nday)-zhat(iday)
       call normalize(zprime(i,j,nday),j) ! Normalize anomalies
      enddo


      ! Changing into 3-dimension of variables
      do nday=1,maxday
        zbar_2(i,j,nday)  = zbar(nday)
        zstar_2(i,j,nday) = zstar(nday)
      enddo 

      do tau=1,N
        zhat_2(i,j,tau) = zhat(tau)
      enddo


        enddo ! i,j loop ends
      enddo


      ! ***********************************************************************
      ! ***********************************************************************
      ! ***********************************************************************
      ! Write anomalies to file
      do nday=1,maxday
        write(f,rec=nday) ((zprime(i,j,nday),i=1,ix),j=1,il)
        write(f+30,rec=nday) ((zbar_2(i,j,nday),i=1,ix),j=1,il)
        write(f+40,rec=nday) ((zstar_2(i,j,nday),i=1,ix),j=1,il)
      enddo

      do tau=1,N
        write(f+50,rec=tau) ((zhat_2(i,j,tau),i=1,ix),j=1,il)
      enddo

      ! Calculate 1sigma thresholds
      call threshold(f,zprime,maxday,yrtype,lmonth)

      close(f)
      close(f+30)
      close(f+40)
      close(f+50)

      enddo ! File number end

      end program SKSanomaly
       


      subroutine normalize(z,j)

      ! Subroutine normalizes anomalies
      ! to account for convergence of longitude
      ! lines
#include "qq.h"

      parameter (pi = 3.14159)
      real:: z
      integer:: j
      real::jgrid
      real:: lat

      jgrid=latres
      if (z.NE.0) then
       lat = ((j-1)*jgrid+lowlat)*(pi/180)
       z = (1/sin(lat))*z
      endif

      return
      end subroutine normalize
     



      subroutine threshold(f,zprime,maxday,yrtype,lmonth)

#include "qq.h"

      integer:: mstar,maxday,m,i,j,nday,f,yrtype
      real, dimension(ix,il,maxday):: zprime
      ! average, standard deviation and counter of anomalies
      doubleprecision, dimension(12):: avz, std, c 
      ! Date track variables
      integer:: iyear=nyear,iday=0,im=1
      integer, dimension(12):: lmonth
 
      ! Initialize
      do m=1,12
       avz(m)=0
       std(m)=0
       c(m)=0
      enddo


      do nday=1,maxday
       call datetrack(lmonth,iday,iyear,im,yrtype)
        do i=1,ix
         do j=llim,ulim
          ! making a 3 month average centered about that month
          avz(im)=avz(im)+zprime(i,j,nday)
          c(im)=c(im)+1
          call wrap(im+1,mstar)
          avz(mstar)=avz(mstar)+zprime(i,j,nday)
          c(mstar)=c(mstar)+1 
          call wrap(im-1,mstar)
          avz(mstar)=avz(mstar)+zprime(i,j,nday)
          c(mstar)=c(mstar)+1
         enddo
        enddo
      enddo


      ! Calculating average
      do m=1,12
        avz(m)=avz(m)/real(c(m))
      enddo

      ! Reinitialize time track variables
      iyear=nyear
      iday=0
      im=1

      do nday=1,maxday
       call datetrack(lmonth,iday,iyear,im,yrtype)
        do i=1,ix
         do j=llim,ulim
          ! making a 3 month average centered about that month
          std(im)=std(im)+(avz(im)-zprime(i,j,nday))**2 
          call wrap(im+1,mstar)
          std(mstar)=std(mstar)+(avz(mstar)-zprime(i,j,nday))**2 
          call wrap(im-1,mstar)
          std(mstar)=std(mstar)+(avz(mstar)-zprime(i,j,nday))**2 
         enddo
        enddo
      enddo


      ! Calculating standard deviation
      do m=1,12
       std(m)=sqrt(std(m)/real(c(m)))
      enddo

      ! Write 1sigma thresholds to file
      open(f+20)
      do m=1,12
       write(f+20,99) std(m)
 99    format(1f10.5)
      enddo
      close(f+20)
       
      ! Outputting to screen std threshold values
      print*
      print*,'File',f,'1sigma thresholds for 30-90deg'
      print*, std(:)
      print*


      return
      end subroutine threshold



      subroutine wrap(m,mstar)

      integer::m,mstar

      if (m.GT.12) then
       mstar=1
      elseif (m.LT.1) then
       mstar=12
      else
       mstar=m
      endif 

      end subroutine wrap

      
