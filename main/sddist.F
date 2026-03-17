      subroutine sddist(lmonth,f,maxday,yrtype,D,be,be_jja)

#include "qq.h"
      ! seonhwa12@kopri.re.kr
      ! Record summer information of dur, ddur
      ! Consider block events which onset occurs within JJA seoson 

      integer::f,maxday,yrtype,D,be,be_jja
      integer::i,j,k,l,ibe
      integer::nday,sday,eday,dday
      integer, dimension(6,be) :: dat
      integer, dimension(be) :: ondat,endat,dudat,kdat
      integer, dimension(:,:),allocatable :: redat
      integer, dimension(55) :: ddur
      ! Date tracking variables
      integer:: iyear,imon,iday,m,s,syear
      integer, dimension(12):: lmonth

      ! OUPUT: 
      open(f+106)
      write(f+106,*)'=================================================='
      write(f+106,*)'       year    onsetday      endday     duration 
     &     label#   '
      write(f+106,*)'=================================================='

      ! INPUT:
      open(f+100)
      do i=1,3  ; read(f+100,*) ; enddo
      do j=1,be ; read(f+100,*) (dat(i,j),i=1,6) ; enddo
      ondat=dat(2,:)  ; endat=dat(3,:) 
      dudat=dat(4,:)  ;  kdat=dat(5,:)


      be_jja=0
      sday=0   ; eday=0  ; dday=0 ; k=0 
      do ibe=1,be
        sday=ondat(ibe)
        eday=endat(ibe)
        dday=dudat(ibe)
           k=kdat(ibe)

         ! Initializing
         syear=0
         iyear=nyear
         iday=0
         imon=1
         m=1
         do nday=1,maxday
          call datetrack(lmonth,iday,imon,iyear,m,yrtype)
          if ((iyear.eq.nyear).and.(m.le.2)) GOTO 9991
          !if ((iyear.eq.nyear+nyr-1).and.(m.gt.2)) GOTO 9991
          if ((m.EQ.12).OR.(m.EQ.1).OR.(m.EQ.2))  s=1
          if ((m.EQ.3).OR.(m.EQ.4).OR.(m.EQ.5))   s=2
          if ((m.EQ.6).OR.(m.EQ.7).OR.(m.EQ.8))   s=3
          if ((m.EQ.9).OR.(m.EQ.10).OR.(m.EQ.11)) s=4
          if ((s.eq.3).and.(nday.eq.sday)) then     
            syear=iyear
          endif
9991  continue         
         enddo !nday 

        !------------------------------------------
        ! IMPORTANT POINT
        if (syear.ne.0) then
         be_jja=be_jja+1
         write(f+106,*) syear,sday,eday,dday,k,be_jja
        endif
        !------------------------------------------
      enddo !ibe enddo

      close(f+100)
      close(f+106)
      !*****************************************
      ! DJF blocking information 
      !*****************************************
      rewind(f+106)  ; allocate(redat(6,be_jja))
      do i=1,3 ; read(f+106,*) ; enddo
      do j=1,be_jja
       read(f+106,*) (redat(i,j),i=1,6)
      enddo
      write(f+106,*) 'JJA Total block event =   ',be_jja
      write(f+106,*) 'JJA Total block days  =   ',sum(redat(4,:))
      write(f+106,*) 'Mean duration         =   ',real(sum(redat(4,:)))
     &                                            / real(be_jja)

      print*,'JJA Total block event =   ',be_jja,'[39 yrs]'
      print*,'JJA Total block days  =   ',sum(redat(4,:))


      ! DDUR **********************************************************
      ddur=0
      do j=1,be_jja
      do l=D,55
       if(redat(4,j).eq.l) then
       ddur(l)=ddur(l)+1
       endif
      enddo
      enddo
      write(f+106,*)'=================================================='
      write(f+106,*)'      duration      event#                        '
      write(f+106,*)'=================================================='
      do i=D,55
      write(f+106,*) i,ddur(i)
      enddo
      write(f+106,*) 'Total block event =      ',sum(ddur(:))


      !*****************************************
      ! blocking >= 50 days information
      !*****************************************
      print*, 'Number of D >= 50 days = ',count(mask=redat(4,:).GT.50)
      do j=1,be_jja
      if (redat(4,j).GT.50) print*,
     &  'yr = ',redat(1,j),'   dur = ',redat(4,j),'   k = ',redat(5,j)
      enddo
      print*,''
      ! **************************************************************

      deallocate(redat)
      close(f+106)

      return
      end subroutine


      
      
