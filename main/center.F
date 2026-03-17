       subroutine center(f,be,bday)

#include "qq.h"
       ! Author : Seon-Hwa Kim (PKNU) 
       ! Date   : 2022-05-24
       ! To find blocking anomaly maximum center within block area  

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
       integer :: be,ibe
       integer, dimension(6,be) :: dat
       integer, dimension(be) :: ondat,endat,kdat
       real, dimension(:,:),allocatable  :: redat2
       ! Periodic boundary function
       integer, external:: pbdry
       ! Zonal and meridional scales
       integer:: minlat,maxlat,midlat
       integer:: minlon,maxlon
       integer:: ipos,jpos
       integer:: f,nday,nrec,bday,wnp_day
       integer:: i,j,k,li,ri
 
      

       ! OUTPUT:
       ! relabel
       open(f+70,form='unformatted',access='direct',recl=ix*il*4)
       ! all position
       open(f+71,form='unformatted',access='direct',recl=ix*il*4)
       ! Center postion
       open(f+72,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel WNP
       open(f+73,form='unformatted',access='direct',recl=ix*il*4)
       ! Center postion WNP
       open(f+75,form='unformatted',access='direct',recl=ix*il*4)
       ! bday
       open(f+109)
       write(f+109,*)'================================================='
       write(f+109,*)'  bec    bday      label#    ipos       jpos   
     &   amp      '
       write(f+109,*)'================================================='



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
          write(f+70,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+72,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
          write(f+73,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
          write(f+75,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
         enddo 
      

       ! INPUT:
       ! z500 anomaly
       open(f,form='unformatted',access='direct',recl=ix*il*4)
       ! label
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! dur
       open(f+100)

      
       do i=1,3  ; read(f+100,*) ; enddo
       do j=1,be ; read(f+100,*) (dat(i,j),i=1,6) ; enddo
       ondat(:)=dat(2,:)
       endat(:)=dat(3,:)
        kdat(:)=dat(5,:)


       ! *******************************************************

        ! Initializing
        wnp_day=0
        nrec=0
        bday=0
        do i=1,ix
         do j=1,il
          anz(i,j)=0
          label(i,j)=0
         enddo
        enddo
        
       ! Tracking by each blocking event
       do ibe=1,be

         ! Sorting by each blocking label#
         k=kdat(ibe)

         do nday=ondat(ibe),endat(ibe)

           nrec = nrec + 1

           read(f,rec=nday) ((anz(i,j),i=1,ix),j=1,il)
           read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
           read(f+70,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           read(f+72,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
           read(f+73,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
           read(f+75,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)



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
c           if(nday.eq.84) print*,nday,ibe,k

           if ((ipos.eq.0).or.(jpos.eq.0)) then
            print*,'[check when maxlocation is 0]',nday,k,ipos,jpos
           endif


           allpos(ipos,jpos)=allpos(ipos,jpos)+1

           pos(ipos,jpos)=-111

           write(f+70,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
           write(f+72,rec=nday) ((pos(i,j),i=1,ix),j=1,il)
           write(f+109,1114) ibe,nday,k,ipos,jpos,anz(ipos,jpos),nrec

1114  format(i6,2i8,2i11,f10.1,i11)


           ! --------------------------------------------
           ! Center for specific regional blocking
           ! --------------------------------------------
                
           ! URAL blocks ( 30E - 80E )
           if ((ipos.GE.31).AND.(ipos.LE.81)) then

             wnp_day=wnp_day+1
c             print*,wnp_day,nday,k,ipos,jpos

             do i=1,ix
             do j=1,il
               relabelWNP(i,j)=relabel(i,j)
             enddo
             enddo

             allposWNP(ipos,jpos)=allposWNP(ipos,jpos)+1

             posWNP(ipos,jpos)=-111

             write(f+73,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)

             write(f+75,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)


           endif ! regional blocks

         enddo ! nday enddo 

       enddo ! ibe enddo
      
        bday=nrec

        write(f+71,rec=1) ((allpos(i,j),i=1,ix),j=1,il)

        print*,'Total center days =            ',int(sum(allpos(:,:)))
        print*,'bday = ',bday


      !*****************************************
      ! CHECK : bday 
      !*****************************************
      close(f+109)
      rewind(f+109)  ; allocate(redat2(7,bday))
      do i=1,3 ; read(f+109,*) ; enddo
      do j=1,bday
        read(f+109,*) (redat2(i,j),i=1,7)
      enddo

      write(f+109,*) 'Total block events =  ',be
      write(f+109,*) 'Total block days   =  ',
     &                  count(mask=redat2(2,:).NE.0)
      write(f+109,*) 'Mean duration      = ',
     &             real(count(mask=redat2(2,:).NE.0)) / real(be)


c *************************************************************


       close(f)
       close(f+60)
       close(f+70)
       close(f+71)
       close(f+72)
       close(f+73)
       close(f+75)
       close(f+100)
       close(f+109)

       return
       end subroutine center



