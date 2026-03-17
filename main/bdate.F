      subroutine bdate(lmonth,f,maxday,yrtype,be)

#include "qq.h"
      ! seonhwa12@kopri.re.kr
      ! Find blocking date
      integer::f,maxday,yrtype,be
      integer::i,j,k,ibe
      integer::nday
      integer, dimension(6,be) :: dat
      integer, dimension(be) :: ondat,endat,dur
      ! Date tracking variables
      integer:: iyear,imon,iday,m,cday
      integer, dimension(12):: lmonth

      ! OUPUT:
      !onset
      open(f+110)
      !end
      open(f+111)
      write(f+110,*)'===================================================
     &======================='
      write(f+110,*)'bevent#  year     mon         day     duration 
     &  onset day    end day'
      write(f+110,*)'===================================================
     &======================='
      write(f+111,*)'===================================================
     &======================='
      write(f+111,*)'bevent#  year     mon         day  ' 
      write(f+111,*)'===================================================
     &======================='



      ! INPUT:
      open(f+100)
      do i=1,3  ; read(f+100,*) ; enddo
      do j=1,be ; read(f+100,*) (dat(i,j),i=1,6) ; enddo
      ondat=dat(2,:)  ; endat=dat(3,:) ; dur=dat(4,:)

      do ibe=1,be
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
            write(f+110,1110) ibe,iyear,m,cday,dur(ibe),k,endat(ibe)
1110        format(i6,2i8,i11,3i12)
          endif
         enddo !nday enddo
      enddo !ibe enddo
   
      do ibe=1,be
         k=endat(ibe)
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
           write(f+111,*) ibe,iyear,m,cday
          endif
         enddo !nday enddo
      enddo !ibe enddo







      close(f+100)
      close(f+110)
      close(f+111)

      return
      end subroutine


      
      
