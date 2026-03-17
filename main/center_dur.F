       subroutine center_dur(f,be,bec,bew,bdayc)

#include "qq.h"
       ! Author : Seon-Hwa Kim (PKNU)
       ! Date   : 2021-06-29 
       ! To recount center duration within regional domain  

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
       integer :: ibe,be,bec,bew,sday,eday
       integer, dimension(6,be) :: dat
       integer, dimension(be) :: ondat,endat,kdat
       integer, dimension(:,:),allocatable :: redat
       real, dimension(:,:),allocatable  :: redat2
       integer, dimension(:),allocatable :: ondat2,endat2,kdat2
       ! duration distribution
       integer, dimension(:,:),allocatable :: ddur
       ! Zonal and meridional scales
       integer:: ipos,jpos
       integer:: f,nday,wnp_day,same_day,ngrid,nmiss
       integer:: minlabel,maxlabel
       integer:: nrec,bdayc
       integer:: i,j,k,li,ri,dr,cen
      

       ! OUTPUT:
       ! dur.WNP.whole (Lagrangian)
       open(f+101)
       write(f+101,*)'================================================='
       write(f+101,*)' year  onsetday endday    duration     label#
     &      center dr  '
       write(f+101,*)'================================================='
       ! dur.WNP (Eulerian)
       open(f+102)
       write(f+102,*)'================================================='
       write(f+102,*)' year    onsetday    endday    center dr   label#
     &        '
       write(f+102,*)'================================================='

       ! allpos WNP (Eulerian)
       open(f+74,form='unformatted',access='direct',recl=ix*il*4)
       ! bdayc  WNP (Eulerian)
       open(f+105)
       write(f+105,*)'================================================='
       write(f+105,*)'  bec    bdayc     label#    ipos       jpos   
     &   amp         cday  '
       write(f+105,*)'================================================='
       ! BAI WNP (Eulerian)
       open(f+108,form='unformatted',access='direct',recl=maxdaylim*4)





       ! INPUT:
       ! z500 anomaly
       open(f,form='unformatted',access='direct',recl=ix*il*4)
       ! label
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel WNP
       open(f+73,form='unformatted',access='direct',recl=ix*il*4)
       ! center  WNP
       open(f+75,form='unformatted',access='direct',recl=ix*il*4)
       ! dur
       open(f+100)

      
       do i=1,3      ; read(f+100,*) ; enddo
       do j=1,be     ; read(f+100,*) (dat(i,j),i=1,6) ; enddo
       ondat(:)=dat(2,:)
       endat(:)=dat(3,:)
        kdat(:)=dat(5,:)


       



c *************************************************************
c
c   Part1 : Lagrangian type of duration
c
c *************************************************************



      ! record center duration within WNP area 
      bew=0
      wnp_day=0
      same_day=0
      do ibe=1,be
       k=kdat(ibe) 
       nday=ondat(ibe)

        do while(nday.lt.endat(ibe))
         read(f+73,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
         read(f+75,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)

         ! center occurrence > 2 at the same day
         if(count(mask=posWNP.EQ.-111).ge.2) then
          same_day=same_day+1
          print*,ibe,nday,count(mask=posWNP.EQ.-111)
         endif

         ! center exists
         if((count(mask=posWNP.EQ.-111).ne.0) .and.
     &    ((minval(relabelWNP,mask=posWNP.EQ.-111).eq.k) .or.
     &     (maxval(relabelWNP,mask=posWNP.EQ.-111).eq.k))) then

            dr=1

            read(f+73,rec=nday+dr) ((relabelWNP(i,j),i=1,ix),j=1,il)
            read(f+75,rec=nday+dr) ((posWNP(i,j),i=1,ix),j=1,il)
            do while((count(mask=posWNP.EQ.-111).ne.0) .and.
     &       ((minval(relabelWNP,mask=posWNP.EQ.-111).eq.k) .or.
     &       (maxval(relabelWNP,mask=posWNP.EQ.-111).eq.k)).and.  
     &       ((nday+dr).le.endat(ibe))  ) 
               dr=dr+1              ! WNP center duration
               wnp_day=wnp_day+1    ! WNP center days
c            print*,'check =',ibe, nday, nday+dr
              if(nday+dr.gt.maxdaylim) exit
              read(f+73,rec=nday+dr) ((relabelWNP(i,j),i=1,ix),j=1,il)
              read(f+75,rec=nday+dr) ((posWNP(i,j),i=1,ix),j=1,il)
            enddo ! while(nday+dr)  

            ! consecutive bdays >= 3 wihin WNP domain
            if(dr.ge.3) then 
             bew=bew+1              ! WNP block event #
             write(f+101,1111) dat(1,ibe),dat(2,ibe),dat(3,ibe),
     &                       dat(4,ibe),dat(5,ibe),dr,bew
             goto 2222 
            endif
            nday=nday+dr
          endif !center exists

         nday=nday+1
        enddo ! while(nday)
2222  continue 
      enddo ! ibe

1111  format(i6,2i8,i11,3i12)
c      print*,'wnp_day = ',wnp_day
c      print*,'**             check             ** '
      
      close(f+101)

      !*****************************************
      ! WNP blocking information 
      !*****************************************
      rewind(f+101)  ; allocate(redat(7,bew))
      do i=1,3 ; read(f+101,*) ; enddo
      do j=1,bew
       read(f+101,*) (redat(i,j),i=1,7)
      enddo
      write(f+101,*) 'Total block event WNP blocks =   ',bew
      write(f+101,*) 'Total block days  WNP blocks =   ',sum(redat(4,:))
      write(f+101,*) 'Mean duration                =   ',
     &                real(sum(redat(4,:))) / real(bew)
      write(f+101,*) ''
      write(f+101,*) 'Total center days WNP blocks =   ',sum(redat(6,:))


      print*,'center occurrence > 2 at the same day = ',same_day
      print*,'Total block event WNP blocks (bew)   =',bew
      print*,'Total block days  WNP blocks (bdayw) =',sum(redat(4,:))
      print*,'Total center days WNP blocks         =',sum(redat(6,:))


      ! DDUR ***********************************************
      ! Initializing
      allocate(ddur(55,bew))
      do j=1,bew
       do l=1,55
        ddur(l,j)=0 
       end do
      end do 

      do j=1,bew
       do l=5,55  ! look for blocks >= D days  
        if (redat(4,j).eq.l) then
         ddur(l,j)=ddur(l,j)+1
        end if
       end do
      end do

      write(f+101,*)'=================================================='
      write(f+101,*)'      duration      event#                        '
      write(f+101,*)'=================================================='
      do i=5,55
       write(f+101,1112) i, sum(ddur(i,:)),
     &             (real(sum(ddur(i,:)))/real(sum(ddur(:,:))))*100
      enddo
1112  format(2i10,f10.2)
       write(f+101,*) 'Total block event =  ',sum(ddur(:,:))
         
      deallocate(ddur)
      deallocate(redat)
      ! DDUR **********************************************



c *************************************************************
c
c   Part2 : Eulerian type of duration
c
c *************************************************************

      ! record center duration within WNP area 
      bec=0
      wnp_day=0
      same_day=0
      sday=0
      eday=0
      do ibe=1,be
       k=kdat(ibe) 
       nday=ondat(ibe)

        do while(nday.lt.endat(ibe))
         read(f+73,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
         read(f+75,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)


         ! center exists
         if((count(mask=posWNP.EQ.-111).ne.0) .and.
     &    ((minval(relabelWNP,mask=posWNP.EQ.-111).eq.k) .or.
     &     (maxval(relabelWNP,mask=posWNP.EQ.-111).eq.k))) then

            dr=1
            read(f+73,rec=nday+dr) ((relabelWNP(i,j),i=1,ix),j=1,il)
            read(f+75,rec=nday+dr) ((posWNP(i,j),i=1,ix),j=1,il)
            do while((count(mask=posWNP.EQ.-111).ne.0) .and.
     &       ((minval(relabelWNP,mask=posWNP.EQ.-111).eq.k) .or.
     &       (maxval(relabelWNP,mask=posWNP.EQ.-111).eq.k)).and.  
     &       ((nday+dr).le.endat(ibe))  ) 
               dr=dr+1              ! WNP center duration
               wnp_day=wnp_day+1    ! WNP center days
              if(nday+dr.gt.maxdaylim) exit
              read(f+73,rec=nday+dr) ((relabelWNP(i,j),i=1,ix),j=1,il)
              read(f+75,rec=nday+dr) ((posWNP(i,j),i=1,ix),j=1,il)
            enddo ! while(nday+dr)  

            ! consecutive bdays >= 3 wihin WNP domain
            if(dr.ge.3) then 
              bec=bec+1              ! WNP block event #
              sday=nday
              eday=nday+dr-1
              write(f+102,1113) dat(1,ibe),nday,eday,
     &                          dr,k,bec
            endif

            nday=nday+dr
          endif !center exists

         nday=nday+1
        enddo ! while(nday)
      enddo ! ibe

1113  format(i6,5i11)

      
      close(f+102)


      !*****************************************
      ! WNP blocking information 
      !*****************************************
      rewind(f+102)  ; allocate(redat(6,bec))
      do i=1,3 ; read(f+102,*) ; enddo
      do j=1,bec
       read(f+102,*) (redat(i,j),i=1,6)
      enddo
      write(f+102,*) 'Total block event WNP blocks =   ',bec
      write(f+102,*) 'Total block days  WNP blocks =   ',sum(redat(4,:))
      write(f+102,*) 'Mean duration                =   ',
     &                real(sum(redat(4,:))) / real(bec)
      write(f+102,*) ''


      print*,'Total block event WNP blocks (bec)  =',bec
      print*,'Total center days WNP blocks (bdayc)=',sum(redat(4,:))


      ! DDUR ***********************************************
      ! Initializing
      allocate(ddur(55,bec))
      do j=1,bec
       do l=1,55
        ddur(l,j)=0 
       end do
      end do 

      do j=1,bec
       do l=5,55  ! look for blocks >= D days  
        if (redat(4,j).eq.l) then
         ddur(l,j)=ddur(l,j)+1
        end if
       end do
      end do

      write(f+102,*)'=================================================='
      write(f+102,*)'      duration      event#                        '
      write(f+102,*)'=================================================='
      do i=5,55
       write(f+102,1112) i, sum(ddur(i,:)),
     &             (real(sum(ddur(i,:)))/real(sum(ddur(:,:))))*100
      enddo
       write(f+102,*) 'Total block event =  ',sum(ddur(:,:))
         
      deallocate(ddur)
      deallocate(redat)
      ! DDUR **********************************************




c *************************************************************
c
c   Part3 : Eulerian type of BAI
c
c *************************************************************

      rewind(f+102)  
      allocate(redat(6,bec)) ; allocate(ondat2(bec)) 
      allocate(endat2(bec))  ; allocate(kdat2(bec))
      do i=1,3 ; read(f+102,*) ; enddo
      do j=1,bec
       read(f+102,*) (redat(i,j),i=1,6)
      enddo
       ondat2(:)=redat(2,:)
       endat2(:)=redat(3,:)
        kdat2(:)=redat(5,:)


       ! Initializing
       nrec=0
       bdayc=0
       do i=1,ix
        do j=1,il
         anz(i,j)=0
         label(i,j)=0
         allposWNP(i,j)=0
        enddo
       enddo


       ! Tracking by each blocking event
       do ibe=1,bec

         ! Sorting by each blocking label#
         k=kdat2(ibe)

         do nday=ondat2(ibe),endat2(ibe)

           nrec = nrec + 1

           read(f,rec=nday) ((anz(i,j),i=1,ix),j=1,il)
           read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
           read(f+73,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
           read(f+75,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)

           ! --------------------------------------------
           ! Anomaly maximum position
           ! --------------------------------------------

           ! Anomaly maximum postion within relabel area
           maxpos = 0
           maxpos = maxloc(anz,mask=label.EQ.k)

           ipos=0
           jpos=0
           ipos=maxpos(1)
           jpos=maxpos(2)

           allposWNP(ipos,jpos)=allposWNP(ipos,jpos)+1


           ! center exist within regional domain 
           if((relabelWNP(ipos,jpos).EQ.k).and.
     &        (posWNP(ipos,jpos).EQ.-111)) then
            cen=1
           else
            cen=0
           endif

             write(f+105,1114) ibe,nday,k,ipos,jpos,anz(ipos,jpos),cen

1114  format(i6,2i8,2i11,f10.1,i11)

         enddo ! nday enddo 

       enddo ! ibe enddo



       bdayc=nrec


       write(f+74,rec=1) ((allposWNP(i,j),i=1,ix),j=1,il)
       ! check allposWNP 
       print*,''
       print*,'Total center days WNP blocks (allposWNP) = ',
     & int(sum(allposWNP(:,:))),maxval(allposWNP),int(minval(allposWNP))


      deallocate(redat)
      deallocate(ondat2)
      deallocate(endat2)
      deallocate(kdat2)


      !*****************************************
      ! CHECK : bdayc 
      !*****************************************
      close(f+105)
      rewind(f+105)  ; allocate(redat2(7,bdayc))
      do i=1,3 ; read(f+105,*) ; enddo
      do j=1,bdayc
        read(f+105,*) (redat2(i,j),i=1,7)
      enddo

      write(f+105,*) 'Total block event NP blocks =  ',bec
      write(f+105,*) 'Total block days  NP blocks =  ',
     &                  count(mask=redat2(2,:).NE.0)
      write(f+105,*) 'Mean duration                = ',
     &             real(count(mask=redat2(2,:).NE.0)) / real(bec)

      write(f+105,*) ''
      write(f+105,*) 'Total center days NP blocks =  ',sum(redat2(7,:))


      !*****************************************
      ! Eulerian BAI (daily) 
      !*****************************************

      ! Initializing
      do nday=1,maxdaylim
       amp(nday)=0
       miss(nday)=0
      enddo

      do j=1,bdayc
       nday=nint(redat2(2,j))
       amp(nday)=amp(nday)+redat2(6,j)
       miss(nday)=miss(nday)+1
      enddo  

      print*,'bdayc = ',bdayc,count(mask=miss(:).eq.1) +
     & (count(mask=miss(:).eq.2)*2)
      print*,'center occurrence = 1 at same day',
     & count(mask=miss(:).eq.1), count(mask=miss(:).eq.1) /real(bdayc)
     & *100,'%'
      print*,'center occurrence = 2 at same day',
     & count(mask=miss(:).eq.2), count(mask=miss(:).eq.2) /real(bdayc)
     & *100,'%'
      print*,'center occurrence > 2 at same day',
     & count(mask=miss(:).gt.2)


      do nday=1,maxdaylim
       if(miss(nday).gt.1) then
         amp(nday)=amp(nday)/miss(nday)
         !print*,nday,amp(nday),int(miss(nday))
       endif
      enddo

      write(f+108,rec=1) amp



c *************************************************************

       close(f)
       close(f+60)
       close(f+73)
       close(f+74)
       close(f+75)
       close(f+100)
       close(f+101)
       close(f+102)
       close(f+105)
       close(f+108)

       return
       end subroutine center_dur

