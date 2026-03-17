       subroutine wwavebreak(f,dphi,dlambda,sgrad,be_djf,ber_djf)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! winter Wave Breaking Index (Davini et al. 2012) 
       ! based on only 'onset day'
       ! To detect whether blocking onset is associated with 
       ! Anticyclonic Wave Breaking or Cyclonic Wave Breaking events 

       ! z500
       real, dimension(ix,il,maxdaylim):: z
       ! label
       real, dimension(ix,il):: label,relabel
       ! label by wbi
       real, dimension(ix,il):: clabel,alabel,nlabel
       ! center
       real, dimension(ix,il):: center
       ! duration record
       integer :: be_djf,ibe,ber_djf
       integer, dimension(6,be_djf) :: dat
       integer, dimension(be_djf) :: ondat,kdat
       ! GHGS
       real :: sgrad,GHGS
       ! Dummy variable to handle vector output of maxloc 
       integer, dimension(2):: maxposit,minposit
       ! Position of anomaly maximum
       integer, dimension(lim):: maxIpos,maxJpos
       ! Periodic boundary function
       integer, external:: pbdry
       ! Zonal and meridional scales
       integer:: dphi,dlambda
       ! Date tracking variables
       integer:: yrtype 
       integer, dimension(12):: lmonth

       ! wave breaking index
       real::maxWBI
       integer:: cd,ad,nd
       integer:: f,maxday,nday
       integer:: sj,li,ri
       integer:: i,j,k
 
       
       ! Initializing
       do i=1,ix
        do j=1,il
         clabel(i,j)=0.
         alabel(i,j)=0.
         nlabel(i,j)=0.
         do nday=1,maxdaylim
          z(i,j,nday)=0.
         enddo
        enddo
       enddo


      ! OUTPUT:
      ! label by wbi 
      open(f+83,form='unformatted',access='direct',recl=ix*il*4)
      open(f+84,form='unformatted',access='direct',recl=ix*il*4)
      open(f+85,form='unformatted',access='direct',recl=ix*il*4)
      do nday=1,maxdaylim
        write(f+83,rec=nday) ((clabel(i,j),i=1,ix),j=1,il)
        write(f+84,rec=nday) ((alabel(i,j),i=1,ix),j=1,il)
        write(f+85,rec=nday) ((nlabel(i,j),i=1,ix),j=1,il)
      enddo
      ! dur.djf by wbi
      open(f+102)
      open(f+103)
      open(f+104)
      write(f+102,*)'=================================================='
      write(f+102,*)'     onsetday      label#       ibe 
     &            '
      write(f+102,*)'=================================================='
      write(f+103,*)'=================================================='
      write(f+103,*)'     onsetday      label#       ibe 
     &            '
      write(f+103,*)'=================================================='
      write(f+104,*)'=================================================='
      write(f+104,*)'     onsetday      label#       ibe 
     &            '
      write(f+104,*)'=================================================='




       ! INPUT:
       ! z500
       call readnetcdf(f,maxday,yrtype,lmonth,z)
       ! label
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel
       open(f+73,form='unformatted',access='direct',recl=ix*il*4)
       ! center
       open(f+75,form='unformatted',access='direct',recl=ix*il*4)
       ! dur.djf
       open(f+101)

       do i=1,3      ; read(f+101,*) ; enddo
       do j=1,be_djf ; read(f+101,*) (dat(i,j),i=1,6) ; enddo
       ondat(:)=dat(2,:)
        kdat(:)=dat(5,:)

      ! *******************************************************
    

      cd=0
      ad=0
      nd=0
      do ibe=1,be_djf
        ! Initializing
        if(ibe.eq.1) then
         do i=1,ix
          do j=1,il
           label(i,j)=0.
           relabel(i,j)=0
           center(i,j)=0
           clabel(i,j)=0.
           alabel(i,j)=0.
           nlabel(i,j)=0.
          enddo
         enddo
        else
          if(ondat(ibe).eq.ondat(ibe-1)) then
           do i=1,ix
            do j=1,il
             label(i,j)=0.
             relabel(i,j)=0
             center(i,j)=0
            enddo
           enddo
          else
           do i=1,ix
            do j=1,il
             label(i,j)=0.
             relabel(i,j)=0
             center(i,j)=0
             clabel(i,j)=0.
             alabel(i,j)=0.
             nlabel(i,j)=0.
            enddo
           enddo
          endif
        endif


        nday=ondat(ibe)
        k=kdat(ibe)
        read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
        read(f+73,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
        read(f+75,rec=nday) ((center(i,j),i=1,ix),j=1,il)
        
        ! If blocks occur
        if (sum(label).GT.0) then


          ! Center position 
          if (minval(center,mask=relabel.EQ.k).eq.-111) then
          maxposit = minloc(center,mask=relabel.EQ.k)
          maxIpos(k)=maxposit(1)
          maxJpos(k)=maxposit(2)
          else
           print*,'There is no center',ibe,nday,
     &           minval(center,mask=relabel.EQ.k)
          endif

          
       ! **************************************************
       ! WBI for each blocking labels
       ! **************************************************
        if((maxIpos(k).ne.0).and.(maxJpos(k).ne.0)) then
         ! Reversal
          GHGS=0.
          GHGS=(z(maxIpos(k),maxJpos(k),nday)-
     &     z(maxIpos(k),maxJpos(k)-nint(dphi/latres),nday)) /
     &                             nint(dphi/latres)

          ! maxWBI
          sj=0
          li=0
          ri=0
          sj=maxJpos(k)-nint((dphi*1.0)/latres)
          li=pbdry(maxIpos(k)-nint((dlambda/lonres)/2))
          ri=pbdry(maxIpos(k)+nint((dlambda/lonres)/2))
          maxWBI=0.
          maxWBI=( z(li,sj,nday)-z(ri,sj,nday) ) /
     &            ((nint(dlambda/lonres))*(-1.))

         ! If reversal occur
         if(GHGS.GT.sgrad) then
          !CB
          if(maxWBI.ge.5) then
            cd=cd+1
            do i=1,ix
             do j=iy,il
              if (label(i,j).EQ.k) then
               clabel(i,j)=label(i,j)
              endif
             enddo
            enddo
            write(f+102,*) nday, k, ibe, cd
c            print*,ibe,nday,k,maxIpos(k),maxJpos(k),maxWBI,'CB'
          !AB
          elseif(maxWBI.le.-5) then
            ad=ad+1
            do i=1,ix
             do j=iy,il
              if (label(i,j).EQ.k) then
               alabel(i,j)=label(i,j)
              endif
             enddo
            enddo
            write(f+103,*) nday, k, ibe, ad
c            print*,ibe,nday,k,maxIpos(k),maxJpos(k),maxWBI,'AB'
          !NE
          else
            nd=nd+1
            do i=1,ix
             do j=iy,il
              if (label(i,j).EQ.k) then
                nlabel(i,j)=label(i,j)
              endif
             enddo
            enddo
            write(f+104,*) nday, k, ibe, nd
          endif          
         ! No reversal
         else
c         print*,'no gradient   nday=',nday
         endif


        endif ! maxIpos maxJpos
       ! **************************************************

        endif ! blocks occur

        write(f+83,rec=nday) ((clabel(i,j),i=1,ix),j=1,il)
        write(f+84,rec=nday) ((alabel(i,j),i=1,ix),j=1,il)
        write(f+85,rec=nday) ((nlabel(i,j),i=1,ix),j=1,il)


      enddo ! nday enddo
      
       ber_djf=cd+ad+nd
       print*,''
       print*,''
       print*,'DJF Total block event =   ',cd+ad+nd,'[39 yrs]'
       print*,'CB =',cd,'  AB =',ad,'  NE =',nd 
       close(f+60)
       close(f+73)
       close(f+75)
       close(f+83)
       close(f+84)
       close(f+85)
       close(f+101)
       close(f+102)
       close(f+103)
       close(f+104)

       return
       end subroutine wwavebreak

