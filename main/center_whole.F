       subroutine center_whole(f,bew,bdayw)

#include "qq.h"
       ! Author : Seon-Hwa Kim, PKNU
       ! To find blocking maximum center within block area  

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
       ! daily center amp index
       real, dimension(maxdaylim):: amp, miss
       ! duration record
       integer :: bew,ibe
       integer, dimension(7,bew) :: dat
       integer, dimension(bew) :: ondat,endat,kdat
       real, dimension(:,:),allocatable :: redat
       ! Zonal and meridional scales
       integer:: ipos,jpos
       integer:: f,nday,nrec,bdayw,cen
       integer:: i,j,k,li,ri
      
 
      

       ! OUTPUT:
       ! relabel WNP whole
       open(f+76,form='unformatted',access='direct',recl=ix*il*4)
       ! allpos  WNP whole
       open(f+77,form='unformatted',access='direct',recl=ix*il*4)
       ! center  WNP whole
       open(f+78,form='unformatted',access='direct',recl=ix*il*4)
       ! bdayw   WNP whole
       open(f+103)
       write(f+103,*)'================================================='
       write(f+103,*)'  bew    bdayw     label#    ipos       jpos   
     &   amp         cday  '
       write(f+103,*)'================================================='
       ! Lag BAI WNP whole
       open(f+107,form='unformatted',access='direct',recl=maxdaylim*4)


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
          write(f+76,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+78,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
         enddo 
     



       ! INPUT:
       ! z500 anomaly
       open(f,form='unformatted',access='direct',recl=ix*il*4)
       ! label
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel WNP
       open(f+73,form='unformatted',access='direct',recl=ix*il*4)
       ! center  WNP
       open(f+75,form='unformatted',access='direct',recl=ix*il*4)
       ! dur.WNP.whole (Lagrangian)
       open(f+101)

      
       do i=1,3   ; read(f+101,*) ; enddo
       do j=1,bew ; read(f+101,*) (dat(i,j),i=1,7) ; enddo
       ondat(:)=dat(2,:)
       endat(:)=dat(3,:)
        kdat(:)=dat(5,:)


       ! *******************************************************

        ! Initializing
        nrec=0
        bdayw=0
        do i=1,ix
         do j=1,il
          anz(i,j)=0
          label(i,j)=0
         enddo
        enddo
        
       ! Tracking by each blocking event
       do ibe=1,bew

         ! Sorting by each blocking label#
         k=kdat(ibe)

         do nday=ondat(ibe),endat(ibe)

           nrec = nrec + 1

           read(f,rec=nday) ((anz(i,j),i=1,ix),j=1,il)
           read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
           read(f+73,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
           read(f+75,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
           read(f+76,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           read(f+78,rec=nday) ((pos(i,j),i=1,ix),j=1,il)



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


           allpos(ipos,jpos)=allpos(ipos,jpos)+1

           pos(ipos,jpos)=-111

           ! center exist within regional domain 
           if((relabelWNP(ipos,jpos).EQ.k).and.
     &        (posWNP(ipos,jpos).EQ.-111)) then
            cen=1
           else
            cen=0
           endif

             write(f+76,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
             write(f+78,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
             write(f+103,1111) ibe,nday,k,ipos,jpos,anz(ipos,jpos),cen

1111  format(i6,2i8,2i11,f10.1,i11)

         enddo ! nday enddo 

       enddo ! ibe enddo
      

        write(f+77,rec=1) ((allpos(i,j),i=1,ix),j=1,il)

        bdayw=nrec


      !*****************************************
      ! CHECK : WNP blocking information 
      !*****************************************
      close(f+103)
      rewind(f+103)  ; allocate(redat(7,bdayw))
      do i=1,3 ; read(f+103,*) ; enddo
      do j=1,bdayw
        read(f+103,*) (redat(i,j),i=1,7)
      enddo
      write(f+103,*) 'Total block event NP blocks =  ',bew
      write(f+103,*) 'Total block days  NP blocks =  ',
     &                  count(mask=redat(2,:).NE.0)
      write(f+103,*) 'Mean duration                = ',
     &             real(count(mask=redat(2,:).NE.0)) / real(bew)

      write(f+103,*) ''
      write(f+103,*) 'Total center days NP blocks =  ',sum(redat(7,:))

c      print*,'Total block days  WNP blocks =   ', 
c     &                 count(mask=redat(2,:).NE.0),nrec,bdayw
c      print*,'Total center days WNP blocks =   ',sum(redat(7,:))



      !*****************************************
      ! Lagrangian BAI (daily) 
      !*****************************************

      ! Initializing
      do nday=1,maxdaylim
       amp(nday)=0
       miss(nday)=0
      enddo 
     

      do j=1,bdayw
       nday=nint(redat(2,j))  
       amp(nday)=amp(nday)+redat(6,j)
       miss(nday)=miss(nday)+1
      enddo ! j 

      print*,'bdayw = ',bdayw,count(mask=miss(:).eq.1) +
     & (count(mask=miss(:).eq.2)*2) 
      print*,'center occurrence = 1 at same day',
     & count(mask=miss(:).eq.1), count(mask=miss(:).eq.1) /real(bdayw)
     & *100,'%'
      print*,'center occurrence = 2 at same day',
     & count(mask=miss(:).eq.2), count(mask=miss(:).eq.2) /real(bdayw)
     & *100,'%'
      print*,'center occurrence > 2 at same day',
     & count(mask=miss(:).gt.2)

      do nday=1,maxdaylim
       if(miss(nday).gt.1) then
         amp(nday)=amp(nday)/miss(nday)
         !print*,nday,amp(nday),int(miss(nday))
       endif
      enddo

      write(f+107,rec=1) amp




       close(f)
       close(f+60)
       close(f+73)
       close(f+75)
       close(f+76)
       close(f+77)
       close(f+78)
       close(f+101)
       close(f+103)
       close(f+107)

       return
       end subroutine center_whole



