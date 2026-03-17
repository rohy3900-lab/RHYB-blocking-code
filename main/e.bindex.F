        program block_index


! Written by Etienne Dunn-Sigouin 
! July 28th, 2010
! NOTE: critical parameters are found in "qq.h"
! NOTE2: Anomalies and amplitude thresholds are calculated using SKSanomaly.F 

! Rewritten by Seon-Hwa Kim
! May 27th, 2022

! anomaly threshold, sigma modified by Hayeon Noh 
! Sep, 2024

#include "qq.h"
#include "netcdf.inc"

c VARIABLES////////////////////////////////////////////


      ! THRESHOLDS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
      ! Standard deviation level                         
      real:: sigma=2.0
      ! Amplitude array (gpm)
      real, dimension(ix,il,12)::A
      ! Duration (days)
      integer:: D=5
      ! Spatial scale (million Km^2)
      integer:: S=2.5*10**6
      ! Overlap
      real::  O=0.5
      ! Mean zonal and Meridional scales (degrees)
      integer:: dphi=15,dlambda=15
      ! Z500 gradient
      real:: sgrad=0

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Z500-hPa  
      real, dimension(ix,il,maxdaylim):: z
      ! Z500-hPa anomaly 
      dimension:: anz(ix,il)
      ! Area represented by each grid point
      dimension:: pixarea(ix,il)
      ! Min and Max labels for present and past day
      integer, dimension(2,2):: nlabel 
      ! Block label maps for current and past day
      real, dimension(ix,il)::clabel,plabel
      ! Duration of each block by year
      integer, dimension(lim,nyr):: dur 
      ! Duration of each block by month & year
      integer, dimension(12,lim,nyr)::mdur 
      ! Onset date of each block by month & year
      integer, dimension(lim,nyr)::onset 
      ! Filename
      integer:: f,i,j,nday,be,bec,bew,bdayc,bdayw,bday
      integer:: be_djf,ber_djf,bec_djf
      integer:: be_jja,ber_jja,bec_jja
      ! Date tracking variables
      integer:: iyear,imon,iday,m,yrtype,maxday
      integer, dimension(12):: lmonth

      integer:: maxlabel ! tracks the maximum label in ENTIRE (all times) label array


c     START ///////////////////////////////////////////////////////

      ! Calculating area array
      ! (area covered by each grid point)
      call areacalc(pixarea)     

      do f=11,11 ! file number

      ! INITIALIZING ARRAYS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      nlabel(1,1)=1
      nlabel(2,1)=1
      nlabel(1,2)=1
      nlabel(2,2)=1

      maxlabel=1

      do i=1,12
       do j=1,nyr
        do nday=1,lim
          dur(nday,j)=0
          mdur(i,nday,j)=0
          onset(nday,j)=0
        enddo
       enddo
      enddo
      do i=1,ix
       do j=1,iy
        anz(i,j)=0
        clabel(i,j)=0
        plabel(i,j)=0
        do nday=1,maxdaylim
         z(i,j,nday)=0
        enddo
        do m = 1, 12
         A(i,j,m) = 0 
        end do 
       enddo
      enddo
      iyear=nyear
      iday=0
      imon=1
      m=1

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! INPUT
      ! Z500
      call readnetcdf(f,maxday,yrtype,lmonth,z)
      ! Anomaly data
      open(f,form='unformatted',access='direct',recl=ix*il*4)
      ! Thresholds
!      open(f+20)
!      ! Reading thresholds
!      do i=1,12
!       read(f+20,*) A(i)
!       A(i)=sigma*A(i)
!       print*,A(i)
!      enddo
!      close(f+20)
!
      call read_threshold(f, A)    
      A = A * sigma 
      !OUTPUT
      ! Label data
      open(f+60,form='unformatted',access='direct',recl=ix*il*4)



      ! *****************************************************************
      ! BLOCK TRACKING ALGORITHM ****************************************
      ! *****************************************************************
      print*, ''  
      print*, 'File=',f   
      print*, 'Tracking anomalies...'


      do nday=1,maxday

         call datetrack(lmonth,iday,imon,iyear,m,yrtype)
         ! Reading anomalies
         read(f,rec=nday) ((anz(i,j),i=1,ix),j=1,il)
         ! Calculate anomalies >= (A)
         call anomaly(anz,A(:,:,m))
         ! Assign unique labels to contiguous grid points
         call ccl(anz,clabel,nlabel,maxlabel)
         ! Remove anomalies with spatial scale < (S)
         call spatialthreshold(clabel,nlabel,S,pixarea,maxlabel)   
         !print*,nday,maxlabel,nlabel(1,1),nlabel(2,1)
         ! Remove labels who do dont satisfy GHG criteria
         call GHG(nlabel,clabel,z(:,:,nday),anz,sgrad,
     &            dphi,dlambda,maxlabel)
         ! Assign same label to anomalies with >= (O)
         call overlap(nday,O,clabel,plabel,nlabel,maxlabel)
         !print*,nday,maxlabel,nlabel(1,1),nlabel(2,1)
         ! Record duration and onset date
         call stat3(clabel,nlabel,dur,onset,iyear,m,iday,nday)


         ! current max/min labels become past max/min labels
         nlabel(1,2)=nlabel(1,1) 
         nlabel(2,2)=nlabel(2,1)

        ! Tracking the maximum label in the ENTIRE label array
        ! So that the first label on a new day starts at the maximum+1
        ! label. This is so that each different block has a unique label.
        ! By block I mean uniquely labeled anomaly after overlap.F
        if (sum(clabel).GT.0) then
         if (maxval(clabel,mask=clabel.GT.0).GE.maxlabel) then
           maxlabel=maxval(clabel,mask=clabel.GT.0)+1
         endif
        endif 

         do i=1,ix
          do j=1,il
            ! Current label becomes past label
            plabel(i,j)=clabel(i,j) 
          enddo
         enddo

         ! Writing label to file each day
         write(f+60,rec=nday) ((clabel(i,j),i=1,ix),j=1,il)


      enddo

      close(f+60)    ! finished writing lables to file
      close(f)       ! close anomaly file


      ! Remove blocks lasting <= (D) in label data
      call durthresh(dur,onset,nlabel(2,1),D,f,yrtype,lmonth,maxday)


      ! ****************************************************************
      ! END BLOCK TRACKING ALGORITHM ***********************************
      ! ****************************************************************
      print*, 'Label number of anomalies satisfying (A),(S),(O) & GHG ='
     +        ,nlabel(2,1)
      print*, 'Total number of blocks = ',count(mask=dur.GE.D)
      print*, 'Number of D >= 50 days = ',count(mask=dur.GT.50)
      print*, ''
      print*, '  Finding distributions...'

      ! BLOCK CHARACTERISTIC DISTRIBUTIONS ******************************
      ! SORT BY each label *********
      ! Total duration 
      call ddist(dur,onset,nlabel(2,1),f,D,be)
      ! Total date
      call bdate(lmonth,f,maxday,yrtype,be)
      ! Total, WNP center
      call center(f,be,bday)
      ! WNP center duration
      call center_dur(f,be,bec,bew,bdayc)
      ! WNP center date 
      call center_dur_date(lmonth,f,maxday,yrtype,bec,bew)
      ! WNP center whole including outside area of domain
      call center_whole(f,bew,bdayw)
      ! WNP center whole date
      call center_whole_date(lmonth,f,maxday,yrtype,bdayc,bdayw,
     & bday)

      ! SORT BY lat-lon grid ****************
      ! season mean frequency
      call smfdist(lmonth,f,maxday,yrtype)
      ! monthly  frequency 
      call mfdist(lmonth,f,maxday,yrtype)



      print*, 'DONE'
      print*, ''

      enddo ! File number end
 
      end program block_index




      
     
     
      
