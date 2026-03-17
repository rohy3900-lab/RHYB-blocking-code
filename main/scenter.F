       subroutine scenter(f,z,be_jja)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! To find blocking center during JJA season  

       ! Z500
       real, dimension(ix,il,maxdaylim):: z
       ! Z500 anomaly 
       real, dimension(ix,il):: anz
       ! label
       real, dimension(ix,il):: label,relabel,relabelWNP
       ! dummy 
       real, dimension(ix,il):: dummy
       ! All center map 
       real, dimension(ix,il):: allpos,allposWNP,pos,posWNP
       ! Z500 anomaly maximum position within relabel
       integer, dimension(2):: maxpos
       ! nlon
       integer, dimension(ix) :: nlon
       ! nlat
       integer, dimension(il) :: nlat
       ! duration record
       integer :: be_jja,ibe
       integer, dimension(6,be_jja) :: dat
       integer, dimension(be_jja) :: ondat,endat,kdat
       ! Periodic boundary function
       integer, external:: pbdry
       ! Zonal and meridional scales
       integer:: minlat,maxlat,midlat
       integer:: minlon,maxlon
       integer:: ipos,jpos
       integer:: i,j,k,li,ri
       integer:: f,nday,wnp_day
 
      

       ! OUTPUT:
       ! relabel
       open(f+981,form='unformatted',access='direct',recl=ix*il*4)
       ! allpos
       open(f+982,form='unformatted',access='direct',recl=ix*il*4)
       ! Center 
       open(f+983,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel WNP
       open(f+984,form='unformatted',access='direct',recl=ix*il*4)
       ! allpos  WNP
       open(f+985,form='unformatted',access='direct',recl=ix*il*4)
       ! Center  WNP
       open(f+986,form='unformatted',access='direct',recl=ix*il*4)


        ! Initializing
         do i=1,ix
          do j=1,il
           relabel(i,j)=0
           relabelWNP(i,j)=0
           allpos(i,j)=0
           allposWNP(i,j)=0
           pos(i,j)=0
           posWNP(i,j)=0
          enddo 
         enddo 
         do nday=1,maxdaylim
          write(f+981,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+983,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+984,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+986,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
         enddo 
      

       ! INPUT:
       ! z500 anomaly
       open(f,form='unformatted',access='direct',recl=ix*il*4)
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

        ! Initializing
        wnp_day=0
        do i=1,ix
         do j=1,il
          anz(i,j)=0
          label(i,j)=0
         enddo
        enddo
        
       ! Tracking by each blocking event
       do ibe=1,be_jja

         ! Sorting by each blocking label#
         k=kdat(ibe)

         do nday=ondat(ibe),endat(ibe)

           read(f,rec=nday) ((anz(i,j),i=1,ix),j=1,il)
           read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
           read(f+981,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           read(f+983,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
           read(f+984,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
           read(f+986,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)


           ! Initializing
           if(sum(label).gt.0)then
             do i=1,ix
             do j=1,il
              dummy(i,j)=0
             enddo
             enddo
           endif


           do i=1,ix
           do j=1,il
             if(label(i,j).eq.k) then
              dummy(i,j)=k
             endif
           enddo
           enddo



           ! --------------------------------------------
           ! Copying label
           ! --------------------------------------------

           if (sum(label,mask=int(dummy).NE.k).GT.0) then
             do i=1,ix
             do j=1,il
              relabel(i,j)=relabel(i,j)+dummy(i,j)
             enddo
             enddo
           else
             do i=1,ix
             do j=1,il
              relabel(i,j)=dummy(i,j)
             enddo
             enddo
           endif




           ! --------------------------------------------
           ! Anomaly maximum position
           ! --------------------------------------------
           
           ! Anomaly maximum postion within relabel area
           maxpos = 0
           maxpos = maxloc(anz,mask=relabel.EQ.k)

           ipos=0
           jpos=0
           ipos=maxpos(1)
           jpos=maxpos(2)
c           print*,nday,k,ipos,jpos
c           if(nday.eq.1492) print*,nday,ibe,k,anz(ipos,jpos),ipos,jpos
c           if(nday.eq.1493) print*,nday,ibe,k,anz(ipos,jpos),ipos,jpos
c           if(nday.eq.1494) print*,nday,ibe,k,anz(ipos,jpos),ipos,jpos

           allpos(ipos,jpos)=allpos(ipos,jpos)+1

           pos(ipos,jpos)=-111

           write(f+981,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           write(f+983,rec=nday) ((pos(i,j),i=1,ix),j=1,il)




           ! --------------------------------------------
           ! Center for a particular region
           ! --------------------------------------------

           ! WNP blocks
           if ((ipos.GE.81).AND.(ipos.LE.161)) then

             wnp_day=wnp_day+1
c             print*,wnp_day,nday,k,ipos,jpos

             do i=1,ix
             do j=1,il
              relabelWNP(i,j)=relabel(i,j)
             enddo
             enddo

             allposWNP(ipos,jpos)=allposWNP(ipos,jpos)+1 

             posWNP(ipos,jpos)=-111

             write(f+984,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
             write(f+986,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)


           endif



         enddo ! nday enddo 

       enddo ! ibe enddo
      

        write(f+982,rec=1) ((allpos(i,j),i=1,ix),j=1,il)
        write(f+985,rec=1) ((allposWNP(i,j),i=1,ix),j=1,il)
        print*,'JJA Total center days = ',int(sum(allpos(:,:)))
        print*,'JJA Total center days WNP blocks = ',
     &                                 int(sum(allposWNP(:,:)))
c        print*,'wnp day =',wnp_day


       close(f)
       close(f+60)
       close(f+981)
       close(f+982)
       close(f+983)
       close(f+984)
       close(f+985)
       close(f+986)
       close(f+106)

       return
       end subroutine scenter

