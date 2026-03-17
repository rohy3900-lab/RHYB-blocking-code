       subroutine sfdist(lmonth,f,maxday,yrtype,be_jja)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! 1. Record block labels which onset occurs within JJA season
       ! 2. Count  frequency 
       ! ex) block onset : Dec. Jan. Feb.
       ! ex) block end   : Dec. Jan. Feb. Mar.
       
       integer:: i,j,k,f,maxday,nday
       ! duration record
       integer:: be_jja,ibe
       integer,dimension(6,be_jja):: dat
       integer, dimension(be_jja) :: ondat,endat,kdat
       ! all label
       real, dimension(ix,il):: label
       ! winter label
       real, dimension(ix,il):: relabel
       ! Frequency distribution by year
       real, dimension(ix,il,4,nyr)::adfreq
       ! Average frequency distribution
       real, dimension(ix,il,4)::dfreq
       ! STD  of frequency distribtuions
       real, dimension(ix,il,4)::stdfreq
       ! Number of days in season per year
       integer, dimension(4,nyr):: c 
       
       ! Date tracking variables
       integer:: dyear,iyear,imon,iday,m,s,yrtype ! "s" for season
       integer, dimension(12):: lmonth
 
       ! OUTPUT:
       ! dfreq.jja
       open(f+46,form='unformatted',access='direct',recl=ix*il*4)
       ! label.jja 
       open(f+63,form='unformatted',access='direct',recl=ix*il*4)
       
         ! Initializing
         do i=1,ix
          do j=1,il
             label(i,j)=0
           relabel(i,j)=0
          enddo
         enddo
         do nday=1,maxdaylim
          write(f+63,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
         enddo


       ! INPUT:
       ! label 
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! dur.jja
       open(f+106)

       do i=1,3      ; read(f+106,*) ; enddo
       do j=1,be_jja ; read(f+106,*) (dat(i,j),i=1,6) ; enddo
       ondat(:)=dat(2,:)  
       endat(:)=dat(3,:) 
        kdat(:)=dat(5,:)



       ! *******************************************************
       ! Tracking by each blocking event
       do ibe=1,be_jja

         ! Sorting by each blocking label#
         k=kdat(ibe)

         do nday=ondat(ibe),endat(ibe)

           read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
           read(f+63,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)

           if (sum(label).GT.0) then
            do i=1,ix
             do j=iy,il
              if (int(label(i,j)).EQ.k) then
                relabel(i,j)=relabel(i,j)+label(i,j)
              endif
             enddo
            enddo
           endif

           write(f+63,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)

         enddo !nday

       enddo !ibe
       ! *******************************************************
      


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
        !if ((iyear.eq.nyear).and.(m.lt.12)) GOTO 9991
        !if ((iyear.eq.nyear+nyr-1).and.(m.gt.3)) GOTO 9991
        if ((m.EQ.6).OR.(m.EQ.7).OR.(m.EQ.8).OR.(m.EQ.9)) then
           s=1
           c(s,iyear-nyear+1)=c(s,iyear-nyear+1)+1
        else
           s=4
        endif
        !print*,nday,iyear,imon,iday,yrtype

        read(f+63,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)

        ! blocks occur
        if (sum(relabel).GT.0) then
        do i=1,ix
        do j=iy,il
          if (s.eq.1) then
             if (relabel(i,j).GT.0) adfreq(i,j,s,iyear-nyear+1)
     &                          = adfreq(i,j,s,iyear-nyear+1)+1

          endif
        enddo
        enddo
        !print*,nday,iyear,m,s,int(adfreq(50,60,s,iyear-nyear+1))
        endif

9991  continue
      enddo ! nday 



      !------------------------------------------------  


      print*,'summer days =',c(1,1)
      !do k=1,nyr
      ! print*,k,c(1,k),c(2,k),int(adfreq(50,60,1,k)),
      !&                        int(adfreq(50,60,2,k))
      !enddo
      print*,'summer total =',sum(c(1,:)),sum(c(2,:)),
     &    int(sum(adfreq(150,100,1,:))),int(sum(adfreq(50,60,2,:)))



      ! Dividing dfreq by # days for dfreq per season per year
      do i=1,ix
       do j=iy,il
        do l=1,nyr
          adfreq(i,j,1,l)=(adfreq(i,j,1,l)/c(1,l))*100
        enddo
       enddo
      enddo


      ! Finding the seasonal and annual mean dfreq's
      do i=1,ix
       do j=iy,il
         dfreq(i,j,1)=dfreq(i,j,1)+
     &            sum(adfreq(i,j,1,1:nyr))/(nyr)
       enddo
      enddo


       ! write to f+46
       write(f+46,rec=1) dfreq(:,:,1)
       write(f+46,rec=2) dfreq(:,:,2)
       write(f+46,rec=3) dfreq(:,:,3)
       write(f+46,rec=4) dfreq(:,:,4)




       close(f+46)
       close(f+60)
       close(f+63)
       close(f+106)


       return
       end subroutine sfdist
