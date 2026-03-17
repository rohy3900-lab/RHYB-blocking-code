      subroutine center_whole_date(lmonth,f,maxday,yrtype,bdayc,bdayw,
     & bday)
#include "qq.h"
      ! Author : Seon-Hwa Kim (PKNU) 
      ! Date   : 2022-05-24
      ! To find blocking date
      integer::f,maxday,yrtype,bdayc,bdayw,bday
      integer::i,j,k,ibe
      integer::nday
      real, dimension(7,bdayw)  :: dat
      integer, dimension(bdayw) :: ondat
      real, dimension(7,bdayc)  :: dat2
      integer, dimension(bdayc) :: ondat2
      real, dimension(7,bday)   :: dat3
      integer, dimension(bday)  :: ondat3
      ! Date tracking variables
      integer:: iyear,imon,iday,m,cday
      integer, dimension(12):: lmonth

      ! OUPUT:
      ! bdate WNP whole
      open(f+104)
      write(f+104,*)'===================================================
     &============================'
      write(f+104,*)'bevent#  year     mon       day     label# 
     &   lon       lat       amp      cday'
      write(f+104,*)'===================================================
     &============================'
      ! bdate WNP
      open(f+106)
      write(f+106,*)'===================================================
     &============================'
      write(f+106,*)'bevent#  year     mon       day     label# 
     &   lon       lat       amp      cday'
      write(f+106,*)'===================================================
     &============================'
      ! bdate 
      open(f+910)
      write(f+910,*)'===================================================
     &============================'
      write(f+910,*)'bevent#  year     mon       day     label# 
     &   lon       lat       amp      cday'
      write(f+910,*)'===================================================
     &============================'


      ! INPUT:
      ! bdayw WNP whole
      open(f+103)
      do i=1,3    ; read(f+103,*) ; enddo
      do j=1,bdayw ; read(f+103,*) (dat(i,j),i=1,7) ; enddo
      ondat=nint(dat(2,:))  

      do ibe=1,bdayw
         k=ondat(ibe)
         ! Initializing
         iyear=nyear
         iday=0
         imon=1
         m=1
         do nday=1,maxday
          call datetrack(lmonth,iday,imon,iyear,m,yrtype)
          if (m.ge.2)then
            if(iday.gt.sum(lmonth(1:m-1))) then
            cday=iday-sum(lmonth(1:m-1))
            endif
          else
          cday=iday
          endif
          if (k.eq.nday) then
            !print*,ibe,iyear,m,cday,iday,k
            write(f+104,1110) nint(dat(1,ibe)),iyear,m,cday,
     &             nint(dat(3,ibe)),lonres*(nint(dat(4,ibe))-1),
     &             (latres*(nint(dat(5,ibe))-1))+lowlat,
     &             dat(6,ibe),nint(dat(7,ibe))
1110        format(3i7,i10,i10,3f10.1,i10)
          endif
         enddo !nday enddo
      enddo !ibe enddo
   



      ! INPUT:
      ! bdayc WNP 
      open(f+105)
      do i=1,3     ; read(f+105,*) ; enddo
      do j=1,bdayc ; read(f+105,*) (dat2(i,j),i=1,7) ; enddo
      ondat2=nint(dat2(2,:))  

      do ibe=1,bdayc
         k=ondat2(ibe)
         ! Initializing
         iyear=nyear
         iday=0
         imon=1
         m=1
         do nday=1,maxday
          call datetrack(lmonth,iday,imon,iyear,m,yrtype)
          if (m.ge.2)then
            if(iday.gt.sum(lmonth(1:m-1))) then
            cday=iday-sum(lmonth(1:m-1))
            endif
          else
          cday=iday
          endif
          if (k.eq.nday) then
            !print*,ibe,iyear,m,cday,iday,k
            write(f+106,1110) nint(dat2(1,ibe)),iyear,m,cday,
     &             nint(dat2(3,ibe)),lonres*(nint(dat2(4,ibe))-1),
     &             (latres*(nint(dat2(5,ibe))-1))+lowlat,
     &             dat2(6,ibe),nint(dat2(7,ibe))
          endif
         enddo !nday enddo
      enddo !ibe enddo
   




      ! INPUT:
      ! bday
      open(f+109)
      do i=1,3     ; read(f+109,*) ; enddo
      do j=1,bday  ; read(f+109,*) (dat3(i,j),i=1,7) ; enddo
      ondat3=nint(dat3(2,:))  

      do ibe=1,bday
         k=ondat3(ibe)
         ! Initializing
         iyear=nyear
         iday=0
         imon=1
         m=1
         do nday=1,maxday
          call datetrack(lmonth,iday,imon,iyear,m,yrtype)
          if (m.ge.2)then
            if(iday.gt.sum(lmonth(1:m-1))) then
            cday=iday-sum(lmonth(1:m-1))
            endif
          else
          cday=iday
          endif
          if (k.eq.nday) then
            !print*,ibe,iyear,m,cday,iday,k
            write(f+910,1110) nint(dat3(1,ibe)),iyear,m,cday,
     &             nint(dat3(3,ibe)),lonres*(nint(dat3(4,ibe))-1),
     &             (latres*(nint(dat3(5,ibe))-1))+lowlat,
     &             dat3(6,ibe),nint(dat3(7,ibe))
          endif
         enddo !nday enddo
      enddo !ibe enddo





      close(f+103)
      close(f+104)
      close(f+105)
      close(f+106)
      close(f+109)
      close(f+910)

      return
      end subroutine


      
      
