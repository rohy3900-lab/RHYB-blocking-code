       subroutine mfdist(lmonth,f,maxday,yrtype)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! monthly-mean & annual-mean frequency
       ! ex) if you have a data period of 19500101~19891231 (40yrs)
       !     output period is for 195001~198912 (40yrs*12mons)
       
       integer:: i,j,k,l,f,maxday,nday
       real, dimension(ix,il):: label
       ! Monthly mean Frequency distribution
       real, dimension(ix,il,12)::dfreq
       ! Monthly mean Frequency distribution by year
       real, dimension(ix,il,12,nyr)::adfreq
       ! Monthly std  Frequency distribtuions
       real, dimension(ix,il,12)::stdfreq
       ! Annual
       real, dimension(ix,il,nyr)::annfreq
       ! Number of days in month per year
       integer, dimension(12,nyr):: c 
       
       ! Date tracking variables
       integer:: dyear,iyear,imon,iday,m,s,yrtype ! "s" for season
       integer, dimension(12):: lmonth
 
       ! OUTPUT:
       ! monthly climatology and monthly/annual file       
       open(f+43,form='unformatted',access='direct',recl=ix*il*4)
       open(f+44,form='unformatted',access='direct',recl=ix*il*12*nyr*4)
       open(f+45,form='unformatted',access='direct',recl=ix*il*nyr*4)
       
       ! INPUT:
       ! Label data
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)

       ! Initializing
       do i=1,ix
        do j=1,il
         do s=1,12
          dfreq(i,j,s)=0
          stdfreq(i,j,s)=0
          do k=1,nyr
           adfreq(i,j,s,k)=0
           annfreq(i,j,k)=0
           c(s,k)=0
          enddo
         enddo
        enddo
       enddo
       iyear=nyear
       iday=0
       imon=1
       m=1
       

      do nday=1,maxday
        call datetrack(lmonth,iday,imon,iyear,m,yrtype)
        if (m.EQ.1)  s=1
        if (m.EQ.2)  s=2
        if (m.EQ.3)  s=3
        if (m.EQ.4)  s=4
        if (m.EQ.5)  s=5
        if (m.EQ.6)  s=6
        if (m.EQ.7)  s=7
        if (m.EQ.8)  s=8
        if (m.EQ.9)  s=9
        if (m.EQ.10) s=10
        if (m.EQ.11) s=11
        if (m.EQ.12) s=12

        read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)

        ! finding the number of days per season per year
        c(s,iyear-nyear+1)=c(s,iyear-nyear+1)+1
        
        ! blocks occur
        if (sum(label).GT.0) then
         do i=1,ix
          do j=iy,il
             if (label(i,j).GT.0) adfreq(i,j,s,iyear-nyear+1)
     &                          = adfreq(i,j,s,iyear-nyear+1)+1 
          enddo
         enddo
        endif
      enddo ! nday enddo

      !do k=1,12
      ! print*,'c = ',k,c(k,1),c(k,40)
      !enddo 

       ! Dividing dfreq by # days for dfreq per season per year
       do i=1,ix
        do j=iy,il
         do l=1,nyr
            
           annfreq(i,j,l) = sum(adfreq(i,j,:,l))

           do k=1,12 ! season              
            adfreq(i,j,k,l)=(adfreq(i,j,k,l)/c(k,l))*100
           enddo
           !if((i.eq.60).and.(j.eq.60))
         !&          print*,l,sum(c(:,l)),annfreq(i,j,l)
           annfreq(i,j,l)=(annfreq(i,j,l)/sum(c(:,l)))*100

         enddo
        enddo
       enddo


       ! Finding the seasonal mean dfreq's
       do i=1,ix
        do j=iy,il
         do k=1,12
          dfreq(i,j,k)=dfreq(i,j,k)+
     &            sum(adfreq(i,j,k,:))/(nyr)
         enddo
        enddo 
       enddo


       ! Calculating the std of dfreq fields
       do i=1,ix
        do j=iy,il 
         do k=1,12
          do l=1,nyr 
           stdfreq(i,j,k)=stdfreq(i,j,k)+
     &         (adfreq(i,j,k,l)-dfreq(i,j,k))**2
          enddo
         enddo
        enddo
       enddo
 
       do i=1,ix
        do j=iy,il
         do k=1,12
           stdfreq(i,j,k)=stdfreq(i,j,k)/(nyr)
           stdfreq(i,j,k)=sqrt(stdfreq(i,j,k))
         enddo
        enddo
       enddo
     
       ! write to f+40
       write(f+43,rec=1)  dfreq(:,:,1)  
       write(f+43,rec=2)  dfreq(:,:,2)
       write(f+43,rec=3)  dfreq(:,:,3)
       write(f+43,rec=4)  dfreq(:,:,4)
       write(f+43,rec=5)  dfreq(:,:,5)  
       write(f+43,rec=6)  dfreq(:,:,6)
       write(f+43,rec=7)  dfreq(:,:,7)
       write(f+43,rec=8)  dfreq(:,:,8)
       write(f+43,rec=9)  dfreq(:,:,9)
       write(f+43,rec=10) dfreq(:,:,10)
       write(f+43,rec=11) dfreq(:,:,11)
       write(f+43,rec=12) dfreq(:,:,12)

       write(f+43,rec=13)  stdfreq(:,:,1)
       write(f+43,rec=14)  stdfreq(:,:,2)
       write(f+43,rec=15)  stdfreq(:,:,3)
       write(f+43,rec=16)  stdfreq(:,:,4)
       write(f+43,rec=17)  stdfreq(:,:,5)
       write(f+43,rec=18)  stdfreq(:,:,6)
       write(f+43,rec=19)  stdfreq(:,:,7)
       write(f+43,rec=20)  stdfreq(:,:,8)
       write(f+43,rec=21)  stdfreq(:,:,9)
       write(f+43,rec=22)  stdfreq(:,:,10)
       write(f+43,rec=23)  stdfreq(:,:,11)
       write(f+43,rec=24)  stdfreq(:,:,12)

       write(f+44,rec=1) adfreq

       write(f+45,rec=1) annfreq


       close(f+43)
       close(f+44)
       close(f+45)
       close(f+60)

       return
       end subroutine mfdist
