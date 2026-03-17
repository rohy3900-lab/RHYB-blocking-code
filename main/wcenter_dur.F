       subroutine wcenter_dur(f,be_djf,bec_djf)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! To find blocking maximum center within block area  

       ! relabel
       real, dimension(ix,il):: relabelWNP
       ! All center map 
       real, dimension(ix,il):: allposWNP,posWNP
       ! duration record
       integer :: be_djf,ibe,bec_djf
       integer, dimension(6,be_djf) :: dat
       integer, dimension(be_djf) :: ondat,endat,kdat
       integer, dimension(:,:),allocatable :: redat
       ! Periodic boundary function
       integer, external:: pbdry
       ! Zonal and meridional scales
       integer:: i,j,k,dr
       integer:: f,nday,wnp_day,ngrid,nmiss
 
      

       ! OUTPUT:
       ! modified Center WNP
       open(f+980,form='unformatted',access='direct',recl=ix*il*4)
       ! dur.djf.WNP
       open(f+105)
       write(f+105,*)'================================================='
       write(f+105,*)' year  onsetday endday    duration     label#
     &      center dr  '
       write(f+105,*)'================================================='


       ! INPUT:
       ! relabel WNP
       open(f+76,form='unformatted',access='direct',recl=ix*il*4)
       ! allpos  WNP
       open(f+77,form='unformatted',access='direct',recl=ix*il*4)
       ! Center  WNP
       open(f+78,form='unformatted',access='direct',recl=ix*il*4)
       ! dur.djf
       open(f+101)

      
       do i=1,3      ; read(f+101,*) ; enddo
       do j=1,be_djf ; read(f+101,*) (dat(i,j),i=1,6) ; enddo
       ondat(:)=dat(2,:)
       endat(:)=dat(3,:)
        kdat(:)=dat(5,:)

 
       ! *******************************************************

       ! check allposWNP 
       read(f+77,rec=1) ((allposWNP(i,j),i=1,ix),j=1,il)

       print*,''
       !print*,'[ TEST start !! ^^ ]'
       print*,'DJF Total center days WNP blocks = ',
     & int(sum(allposWNP(:,:))),maxval(allposWNP),minval(allposWNP)

       
       ! copy pos
       do nday=1,maxdaylim
         read(f+78,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
         write(f+980,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
       enddo

       ! check pos
       wnp_day=0
       do nday=1,maxdaylim
         read(f+980,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
         if(count(mask=posWNP.EQ.-111).ne.0) then
             wnp_day=wnp_day+1
         endif
         if(count(mask=posWNP.EQ.-111).gt.1) then
             print*,'[case of the center occurrence > 1 ]',
     &              wnp_day,nday,count(mask=posWNP.EQ.-111)
         endif
       enddo
       print*,'wnp_day = ',wnp_day



       ! remove a case of center occurrence = 1 
       ngrid=0
       nmiss=0
c       do i=1,ix 
c        do j=iy,il
c         if (allposWNP(i,j).eq.1) then
c            ngrid=ngrid+1

c            do nday=1,maxdaylim
c              read(f+980,rec=nday) ((posWNP(ii,jj),ii=1,ix),jj=1,il)
c              if (posWNP(i,j).eq.-111) then
c                posWNP(i,j)=0
c                nmiss=nmiss+1
c              endif
c              write(f+980,rec=nday) ((posWNP(ii,jj),ii=1,ix),jj=1,il)
c            enddo ! nday

c         endif
c        enddo
c       enddo

       print*,'ngrid = ',ngrid
       print*,'nmiss = ',nmiss

      

       ! check pos
       wnp_day=0
       do nday=1,maxdaylim
         read(f+980,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
         if(count(mask=posWNP.EQ.-111).ne.0) then
             wnp_day=wnp_day+1
             !print*,wnp_day,nday
         endif
       enddo
       print*,''
       print*,'[case of center occurrence >= 2] WNP center days = ',
     &         wnp_day


      ! record duration
      wnp_day=0
      bec_djf=0
      do ibe=1,be_djf
       k=kdat(ibe)
       dr=0

        do nday=ondat(ibe),endat(ibe)
         read(f+76,rec=nday) ((relabelWNP(i,j),i=1,ix),j=1,il)
         read(f+980,rec=nday) ((posWNP(i,j),i=1,ix),j=1,il)
         ! center exists
         if(count(mask=posWNP.EQ.-111).ne.0) then

           ! center corresponds to label#
           if (maxval(relabelWNP,mask=posWNP.EQ.-111).eq.k) then
            wnp_day=wnp_day+1
            dr=dr+1
            !GOTO 7771
           endif

         endif
        enddo ! nday


        if (dr.ne.0) then 
           bec_djf=bec_djf+1
           write(f+105,1111) dat(1,ibe),dat(2,ibe),dat(3,ibe),
     &                       dat(4,ibe),dat(5,ibe),dr,bec_djf
        endif


1111  format(i6,2i8,i11,3i12)
7771  continue  

      enddo ! ibe
      !print*,'wnp_day = ',wnp_day

      
      close(f+105)
      !*****************************************
      ! modified DJF blocking information 
      !*****************************************
      rewind(f+105)  ; allocate(redat(7,bec_djf))
      do i=1,3 ; read(f+105,*) ; enddo
      do j=1,bec_djf
       read(f+105,*) (redat(i,j),i=1,7)
      enddo
      write(f+105,*) 'DJF Total block event =   ',bec_djf
      write(f+105,*) 'DJF Total block days  =   ',sum(redat(4,:))
      write(f+105,*) 'Mean duration         =   ',real(sum(redat(4,:)))
     &                                            / real(bec_djf)
      write(f+105,*) ''
      write(f+105,*) 'DJF Total center days  =   ',sum(redat(6,:))


      print*,'DJF Total block event =   ',bec_djf
      print*,'DJF Total block days  =   ',sum(redat(4,:))


         
       deallocate(redat)

       close(f+76)
       close(f+77)
       close(f+78)
       close(f+101)
       close(f+105)
       close(f+980)



       return
       end subroutine wcenter_dur

