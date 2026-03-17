      subroutine wcenter_dur_date(lmonth,f,maxday,yrtype,bec_djf)

#include "qq.h"
      ! seonhwa12@kopri.re.kr
      ! Find winter blocking date
      integer::f,maxday,yrtype,bec_djf
      integer::i,j,k,ibe
      integer::nday
      integer, dimension(7,bec_djf) :: dat
      integer, dimension(bec_djf) :: ondat,endat,dur
      ! Date tracking variables
      integer:: iyear,imon,iday,m,cday
      integer, dimension(12):: lmonth

      ! OUPUT:
      !onset
      open(f+114)
      !end
      open(f+115)
      write(f+114,*)'===================================================
     &======================='
      write(f+114,*)'bevent#  year     mon         day     duration 
     &  onset day    end day'
      write(f+114,*)'===================================================
     &======================='
      write(f+115,*)'===================================================
     &======================='
      write(f+115,*)'bevent#  year     mon         day  ' 
      write(f+115,*)'===================================================
     &======================='



      ! INPUT:
      open(f+105)
      do i=1,3 ; read(f+105,*) ; enddo
      do j=1,bec_djf ; read(f+105,*) (dat(i,j),i=1,7) ; enddo
      ondat=dat(2,:)  ; endat=dat(3,:) ; dur=dat(4,:)

      do ibe=1,bec_djf
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
            write(f+114,1110) ibe,iyear,m,cday,dur(ibe),k,endat(ibe)
1110        format(i6,2i8,i11,3i12)
          endif
         enddo !nday enddo
      enddo !ibe enddo
   
      do ibe=1,bec_djf
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
           write(f+115,*) ibe,iyear,m,cday
          endif
         enddo !nday enddo
      enddo !ibe enddo







      close(f+105)
      close(f+114)
      close(f+115)

      return
      end subroutine


      
      
