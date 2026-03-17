       subroutine wavebreak(f,dphi,dlambda,sgrad,be)

#include "qq.h"
       ! seonhwa12@kopri.re.kr  
       ! Wave Breaking Index (Davini et al. 2012) 
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
       integer :: be,ibe
       integer, dimension(6,be) :: dat
       integer, dimension(be) :: ondat,kdat
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
       open(f+80,form='unformatted',access='direct',recl=ix*il*4)
       open(f+81,form='unformatted',access='direct',recl=ix*il*4)
       open(f+82,form='unformatted',access='direct',recl=ix*il*4)
       do nday=1,maxdaylim
        write(f+80,rec=nday) ((clabel(i,j),i=1,ix),j=1,il)
        write(f+81,rec=nday) ((alabel(i,j),i=1,ix),j=1,il)
        write(f+82,rec=nday) ((nlabel(i,j),i=1,ix),j=1,il)
       enddo

       ! INPUT:
       ! z500
       call readnetcdf(f,maxday,yrtype,lmonth,z)
       ! label
       open(f+60,form='unformatted',access='direct',recl=ix*il*4)
       ! relabel
       open(f+70,form='unformatted',access='direct',recl=ix*il*4)
       ! center
       open(f+72,form='unformatted',access='direct',recl=ix*il*4)
       ! dur
       open(f+100)
      
       do i=1,3  ; read(f+100,*) ; enddo
       do j=1,be ; read(f+100,*) (dat(i,j),i=1,6) ; enddo
       ondat(:)=dat(2,:)
        kdat(:)=dat(5,:)


      ! *******************************************************




      cd=0
      ad=0
      nd=0
      do ibe=1,be
        ! Initializing
        if(ibe.eq.1) then
         do i=1,ix
          do j=1,il
           label(i,j)=0
           relabel(i,j)=0
           center(i,j)=0
           clabel(i,j)=0
           alabel(i,j)=0
           nlabel(i,j)=0
          enddo
         enddo
        else
          if(ondat(ibe).eq.ondat(ibe-1)) then
           do i=1,ix
            do j=1,il
             label(i,j)=0
             relabel(i,j)=0
             center(i,j)=0
            enddo
           enddo
          else
           do i=1,ix
            do j=1,il
             label(i,j)=0
             relabel(i,j)=0
             center(i,j)=0
             clabel(i,j)=0
             alabel(i,j)=0
             nlabel(i,j)=0
            enddo
           enddo
          endif
        endif

        nday=ondat(ibe)
        k=kdat(ibe)
        read(f+60,rec=nday) ((label(i,j),i=1,ix),j=1,il)
        read(f+70,rec=nday) ((relabel(i,j),i=1,ix),j=1,il)
        read(f+72,rec=nday) ((center(i,j),i=1,ix),j=1,il)
        
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
            !if(nday.eq.14580) print*,ibe,nday,k,maxWBI,'NE'
          endif
         ! No reversal
         else
c         print*,'no gradient   nday=',nday
         endif


        endif ! maxIpos maxJpos
       ! **************************************************

        endif ! blocks occur

        write(f+80,rec=nday) ((clabel(i,j),i=1,ix),j=1,il)
        write(f+81,rec=nday) ((alabel(i,j),i=1,ix),j=1,il)
        write(f+82,rec=nday) ((nlabel(i,j),i=1,ix),j=1,il)


      enddo ! nday enddo
       
       print*,''
       print*,''
       print*,'Based on Onset _________________________________'
       print*,'Total block event =      ',cd+ad+nd,' [40 yrs]'
       print*,'CB =',cd,'  AB =',ad,'  NE =',nd
       close(f+60)
       close(f+70)
       close(f+72)
       close(f+80)
       close(f+81)
       close(f+82)
       close(f+100)

       return
       end subroutine wavebreak

