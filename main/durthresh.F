      subroutine durthresh(dur,onset,nlabel,D,f,yrtype,lmonth,maxday)

#include "qq.h"
      ! seonhwa12@kopri.re.kr
      ! 1. Links dur from last week of DEC to first week of JAN
      ! 2. Removes label data that last <= (D)


      integer:: D,f,yrtype,maxday
      integer:: nlabel
      real, dimension(ix,il):: label
      integer, dimension(lim,nyr):: dur
      integer, dimension(lim,nyr):: onset
      real, dimension(1):: minlabel,maxlabel
      ! Date tracking variables
      integer:: iyear,imon,iday,m
      integer, dimension(12):: lmonth
      integer::iyr,k

      
      !print*,'  durthresh .. '
      print*, 'Total number of blocks = ',count(mask=dur.GE.D)


      ! If blocking occurs from last week Dec. to first week Jan.,
      ! it replaces final duration
      do iyr=1,nyr-1
       do k=1,nlabel
        if ((dur(k,iyr+1).GE.D).and.(onset(k,iyr+1).NE.0)) then
          if (onset(k,iyr).eq.onset(k,iyr+1)) then
           dur(k,iyr)=dur(k,iyr+1)
        !print*,iyr,k,onset(k,iyr),onset(k,iyr+1),dur(k,iyr),dur(k,iyr+1)
          endif
        endif
       enddo
      enddo



      ! ************************************************************
      ! Label data
      open(f+60,form='unformatted',access='direct',recl=ix*il*4)

      iyear=nyear
      iday=0
      imon=1
      m=1

      do nday=1,maxday
      
       call datetrack(lmonth,iday,imon,iyear,m,yrtype)

       read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il) 

       if (sum(label).GT.0) then
        minlabel= minval(label,mask=label.GT.0)
        maxlabel= maxval(label,mask=label.GT.0)

        do k=minlabel(1),maxlabel(1)
         if (dur(k,iyear-nyear+1).LT.D) then
          do i=1,ix
           do j=iy,il         
            if (int(label(i,j)).EQ.k) label(i,j)=0
           enddo
          enddo
         endif 
        enddo
       endif

       write(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)

      enddo

      close(f+60)
      ! ************************************************************



      ! If blocking occurs from last week Dec. to first week Jan.,
      ! it remains only first year which blocks start.
      ! For exactly counting # of blocking events
      do iyr=1,nyr
       do k=1,nlabel
        if ((dur(k,iyr).GE.D).and.(onset(k,iyr).NE.0)) then
         if(onset(k,iyr).eq.onset(k,iyr+1)) then
           dur(k,iyr+1)=0
        print*,iyr,k,onset(k,iyr),onset(k,iyr+1),dur(k,iyr),dur(k,iyr+1)
         endif
        endif
       enddo
      enddo



      return
      end subroutine durthresh
