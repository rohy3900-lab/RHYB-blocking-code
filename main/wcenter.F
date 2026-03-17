       subroutine wcenter(f,z,be_djf)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! To find blocking maximum center within block area  

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
       integer :: be_djf,ibe
       integer, dimension(6,be_djf) :: dat
       integer, dimension(be_djf) :: ondat,endat,kdat
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
       open(f+73,form='unformatted',access='direct',recl=ix*il*4)
       ! allpos
       open(f+74,form='unformatted',access='direct',recl=ix*il*4)
       ! Center 
       open(f+75,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel WNP
       open(f+76,form='unformatted',access='direct',recl=ix*il*4)
       ! allpos  WNP
       open(f+77,form='unformatted',access='direct',recl=ix*il*4)
       ! Center  WNP
       open(f+78,form='unformatted',access='direct',recl=ix*il*4)
       ! Blocking Amplitude Index WNP 
       open(f+79,form='unformatted',access='direct',recl=4)
       ! Blocking Amplitude Index 
       !open(f+979,form='unformatted',access='direct',recl=4)


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
          write(f+73,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+75,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+76,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+78,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+79,rec=nday) relabel(1,1)
          !write(f+979,rec=nday) relabel(1,1)
         enddo 
      

       ! INPUT:
       ! z500 anomaly
       open(f,form='unformatted',access='direct',recl=ix*il*4)
       ! label
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! dur.djf
       open(f+101)

      
       do i=1,3      ; read(f+101,*) ; enddo
       do j=1,be_djf ; read(f+101,*) (dat(i,j),i=1,6) ; enddo
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
       do ibe=1,be_djf

         ! Sorting by each blocking label#
         k=kdat(ibe)

         do nday=ondat(ibe),endat(ibe)

           read(f,rec=nday) ((anz(i,j),i=1,ix),j=1,il)
           read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
           read(f+73,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           read(f+75,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
           read(f+76,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
           read(f+78,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)


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

           write(f+73,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           write(f+75,rec=nday) ((pos(i,j),i=1,ix),j=1,il)

           ! Expanded area of WNP (60E-210W)
           !if ((ipos.GE.24).AND.(ipos.LE.94)) then
           !  write(f+979,rec=nday) anz(ipos,jpos)
           !endif



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

             write(f+76,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
             write(f+78,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
             write(f+79,rec=nday)  anz(ipos,jpos)


           endif



         enddo ! nday enddo 

       enddo ! ibe enddo
      

        write(f+74,rec=1) ((allpos(i,j),i=1,ix),j=1,il)
        write(f+77,rec=1) ((allposWNP(i,j),i=1,ix),j=1,il)
        print*,'DJF Total center days = ',int(sum(allpos(:,:)))
        print*,'DJF Total center days WNP blocks = ',
     &                                 int(sum(allposWNP(:,:)))
c        print*,'wnp day =',wnp_day


       close(f)
       close(f+60)
       close(f+73)
       close(f+74)
       close(f+75)
       close(f+76)
       close(f+77)
       close(f+78)
       close(f+79)
       close(f+101)
       !close(f+979)

       return
       end subroutine wcenter

