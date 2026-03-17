      subroutine center_dur_date(lmonth,f,maxday,yrtype,bec,bew)

#include "qq.h"
      ! Author : Seon-Hwa Kim, PKNU
      ! To find blocking date
      integer::f,maxday,yrtype,bec,bew
      integer::i,j,k,ibe
      integer::nday
      integer, dimension(7,bew) :: dat
      integer, dimension(bew) :: ondat,endat,dur,kdat,cdat
      integer, dimension(6,bec) :: dat2
      integer, dimension(bec) :: ondat2,endat2,dur2,kdat2,cdat2
      ! Date tracking variables
      integer:: iyear,imon,iday,m,cday
      integer, dimension(12):: lmonth

      ! OUPUT:
      !onset whole
      open(f+112)
      !end whole
      open(f+113)
      !onset 
      open(f+114)
      !end 
      open(f+115)

      write(f+112,*)'===================================================
     &======================='
      write(f+112,*)'bevent#  year     mon         day     duration 
     &  label#    center dr'
      write(f+112,*)'===================================================
     &======================='
      write(f+113,*)'===================================================
     &======================='
      write(f+113,*)'bevent#  year     mon         day  ' 
      write(f+113,*)'===================================================
     &======================='

      write(f+114,*)'===================================================
     &======================='
      write(f+114,*)'bevent#  year     mon         day     center dr 
     &  label#    '
      write(f+114,*)'===================================================
     &======================='
      write(f+115,*)'===================================================
     &======================='
      write(f+115,*)'bevent#  year     mon         day  ' 
      write(f+115,*)'===================================================
     &======================='



c *************************************************************
c
c   Part1 : Lagrangian type
c
c *************************************************************

      ! INPUT:
      open(f+101)
      do i=1,3 ; read(f+101,*) ; enddo
      do j=1,bew ; read(f+101,*) (dat(i,j),i=1,7) ; enddo
      ondat=dat(2,:)  ; endat=dat(3,:) ; dur=dat(4,:)
       kdat=dat(5,:)  ;  cdat=dat(6,:) 

      do ibe=1,bew
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
            write(f+112,1110) ibe,iyear,m,cday,dur(ibe),
     &                                    kdat(ibe),cdat(ibe)
1110        format(i6,2i8,i11,3i12)
          endif
         enddo !nday enddo
      enddo !ibe enddo
   
      do ibe=1,bew
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
           write(f+113,*) ibe,iyear,m,cday
          endif
         enddo !nday enddo
      enddo !ibe enddo



c *************************************************************
c
c   Part2 : Eulerian type 
c
c *************************************************************

      ! INPUT:
      open(f+102)
      do i=1,3 ; read(f+102,*) ; enddo
      do j=1,bec ; read(f+102,*) (dat2(i,j),i=1,6) ; enddo
      ondat2=dat2(2,:)  ; endat2=dat2(3,:) ; dur2=dat2(4,:)
       kdat2=dat2(5,:)  ;  

      do ibe=1,bec
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
            write(f+114,1111) ibe,iyear,m,cday,dur2(ibe),
     &                                    kdat2(ibe)
1111        format(i6,2i8,i11,2i12)
          endif
         enddo !nday enddo
      enddo !ibe enddo
   
      do ibe=1,bec
         k=endat2(ibe)
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





      close(f+101)
      close(f+102)
      close(f+112)
      close(f+113)
      close(f+114)
      close(f+115)

      return
      end subroutine


      
      
