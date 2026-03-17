       subroutine smfdist(lmonth,f,maxday,yrtype)

#include "qq.h"
       ! Author : Seon-Hwa Kim (PKNU) 
       ! season-mean frequency : MAM,JJA,SON,DJF 
       ! ex) 1950 3,4,5   : 1950-MAM
       !     1950 6,7,8   : 1950-JJA 
       !     1950 9,10,11 : 1950-SON
       !     1950 12 & 1951 1,2 : 1950-DJF
       ! ex) if you have a data period of 19500101~19891231 (40yrs)
       !     output period is for 19500301~19890228 (39yrs)
       
       integer:: i,j,k,f,maxday
       real, dimension(ix,il):: label
       ! Average frequency distribution
       real, dimension(ix,il,4)::dfreq
       ! Frequency distribution by year
       real, dimension(ix,il,4,nyr)::adfreq
       ! Interannual variability of frequency distribtuions
       real, dimension(ix,il,4)::stdfreq
       ! Number of days in season per year
       integer, dimension(4,nyr):: c 
       
       ! Date tracking variables
       integer:: dyear,iyear,imon,iday,m,s,yrtype ! "s" for season
       integer, dimension(12):: lmonth
 
       ! OUTPUT:
       ! 4 season dfreq and stdfreq      
       open(f+40,form='unformatted',access='direct',recl=ix*il*4)
       open(f+41,form='unformatted',access='direct',
     &                                      recl=ix*il*(nyr-1)*4)
       
       ! INPUT:
       ! Label data
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)

       ! Initializing
       do i=1,ix
        do j=1,il
         do s=1,4
          dfreq(i,j,s)=0
          stdfreq(i,j,s)=0
          do k=1,nyr
           adfreq(i,j,s,k)=0
           c(s,k)=0
          enddo
         enddo
        enddo
       enddo
       dyear=nyear  ! start year for DJF season
       iyear=nyear
       iday=0
       imon=1
       m=1
       

      do nday=1,maxday
        call datetrack(lmonth,iday,imon,iyear,m,yrtype)
        if ((iyear.eq.nyear).and.(m.le.2)) GOTO 9991
        if ((iyear.eq.nyear+nyr-1).and.(m.gt.2)) GOTO 9991
        if (m.EQ.12) dyear=iyear
        if ((m.EQ.12).OR.(m.EQ.1).OR.(m.EQ.2))  s=1
        if ((m.EQ.3).OR.(m.EQ.4).OR.(m.EQ.5))   s=2
        if ((m.EQ.6).OR.(m.EQ.7).OR.(m.EQ.8))   s=3
        if ((m.EQ.9).OR.(m.EQ.10).OR.(m.EQ.11)) s=4
        !if ((s.eq.1).or.(s.eq.4)) print*,iyear,m,s,dyear

        read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)

        ! finding the number of days per season per year
        if (s.eq.1) then
        c(s,dyear-nyear+1)=c(s,dyear-nyear+1)+1 
        else
        c(s,iyear-nyear+1)=c(s,iyear-nyear+1)+1
        endif
        
        ! blocks occur
        if (sum(label).GT.0) then
        do i=1,ix
        do j=iy,il
           if (s.eq.1) then
             if (label(i,j).GT.0) adfreq(i,j,s,dyear-nyear+1)
     &                          = adfreq(i,j,s,dyear-nyear+1)+1        
           else
             if (label(i,j).GT.0) adfreq(i,j,s,iyear-nyear+1)
     &                          = adfreq(i,j,s,iyear-nyear+1)+1 
           endif
        enddo
        enddo
        endif
9991  continue
      enddo ! nday enddo


       ! Dividing dfreq by # days for dfreq per season per year
       do i=1,ix
        do j=iy,il
         do l=1,nyr-1
           do k=1,4 ! season              
            adfreq(i,j,k,l)=(adfreq(i,j,k,l)/c(k,l))*100
           enddo
           !if((i.eq.60).and.(j.eq.60))
      !&          print*,l,c(1,l),adfreq(i,j,1,l)
         enddo
        enddo
       enddo


       ! Finding the seasonal and annual mean dfreq's
       do i=1,ix
        do j=iy,il
         do k=1,4
          dfreq(i,j,k)=dfreq(i,j,k)+
     &            sum(adfreq(i,j,k,1:nyr-1))/(nyr-1)
         enddo
        enddo 
       enddo


       ! Calculating the std of dfreq fields
       do i=1,ix
        do j=iy,il 
         do k=1,4
          do l=1,nyr-1 
           stdfreq(i,j,k)=stdfreq(i,j,k)+
     &         (adfreq(i,j,k,l)-dfreq(i,j,k))**2
          enddo
         enddo
        enddo
       enddo
 
       do i=1,ix
        do j=iy,il
         do k=1,4
           stdfreq(i,j,k)=stdfreq(i,j,k)/(nyr-1)
           stdfreq(i,j,k)=sqrt(stdfreq(i,j,k))
         enddo
        enddo
       enddo
     
       ! write to f+40
       write(f+40,rec=1) dfreq(:,:,1)  
       write(f+40,rec=2) dfreq(:,:,2)
       write(f+40,rec=3) dfreq(:,:,3)
       write(f+40,rec=4) dfreq(:,:,4)
       write(f+40,rec=5) stdfreq(:,:,1)  
       write(f+40,rec=6) stdfreq(:,:,2)
       write(f+40,rec=7) stdfreq(:,:,3)
       write(f+40,rec=8) stdfreq(:,:,4)

       write(f+41,rec=1) adfreq(:,:,1,1:nyr-1)

       close(f+40)
       close(f+41)
       close(f+60)

       return
       end subroutine smfdist
