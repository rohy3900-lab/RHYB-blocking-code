      subroutine ddist(dur,onset,nlabel,f,D,be)

#include "qq.h"
      ! seonhwa12@kopri.re.kr
      ! Record annual information of dur, ddur
      integer:: iyr,i,j,k,l,f,D
      integer:: nlabel,be
      integer, dimension(lim,nyr):: dur
      integer, dimension(55,nyr):: ddur
      integer, dimension(lim,nyr):: onset
      integer, dimension(:,:),allocatable :: redat

      ! OUPUT: 
      open(f+100)
      write(f+100,*)'=================================================='
      write(f+100,*)'       year    onsetday      endday     duration 
     &     label#   '
      write(f+100,*)'=================================================='



      ! If you do dur=0 at dur<5, result is
      ! block days = sum(dur(:,:)) 
      do iyr=1,nyr
       do k=1,nlabel
        if((dur(k,iyr).LT.D).and.(onset(k,iyr).NE.0)) dur(k,iyr)=0 
       enddo
      enddo


      be=0
      do iyr=1,nyr
       do k=1,nlabel
        if ((dur(k,iyr).GE.D).and.(onset(k,iyr).NE.0)) then
         be=be+1
         write(f+100,*) iyr,onset(k,iyr),onset(k,iyr)+dur(k,iyr)-1,
     &                                           dur(k,iyr),k,be
        endif
       enddo
      enddo

      close(f+100)
      !*****************************************
      ! blocking information
      !*****************************************
      rewind(f+100)  ; allocate(redat(6,be))
      do i=1,3 ; read(f+100,*) ; enddo
      do j=1,be
       read(f+100,*) (redat(i,j),i=1,6)
      enddo 
      write(f+100,*) 'Total block event =  ',be
      write(f+100,*) 'Total block days  =  ',sum(redat(4,:))
      write(f+100,*) 'Mean duration     =  ',real(sum(redat(4,:)))
     &                                            / real(be)
   
      print*,'Total block event =       ',be,
     &                        '[40 yrs] ',count(mask=dur.GE.D)
      print*,'Total block days  =       ',sum(redat(4,:)),
     &                        '         ',sum(dur(:,:))
      !print*,''


      ! DDUR ***********************************************************
      ! Initializing
      do iyr=1,nyr 
       do i=1,55
        ddur(i,iyr)=0
       enddo     
      enddo  

      do iyr=1,nyr    
       do k=1,nlabel
          do l=D,55  ! look for blocks >= D days       
            if (dur(k,iyr).EQ.l) then
              ddur(l,iyr)=ddur(l,iyr)+1
            endif
          enddo
       enddo
      enddo  ! iyr enddo

      write(f+100,*)'=================================================='
      write(f+100,*)'      duration      event#                        '
      write(f+100,*)'=================================================='
      do i=D,55
       write(f+100,1112) i, sum(ddur(i,:)),
     &        (real(sum(ddur(i,:)))/real(sum(ddur(:,:))))*100 
      enddo
1112  format(2i10,f10.2)
      write(f+100,*) 'Total block event =      ',sum(ddur(:,:))


      !*****************************************
      ! blocking >= 50 days information
      !*****************************************
      !print*, 'Number of D >= 50 days = ',count(mask=dur.GT.50)
      !do iyr=1,nyr
      !do k=1,nlabel
      ! if(dur(k,iyr).GT.50) print*,'k =',k,'   yr =',iyr+1949,
      !&                                     '   dur =',dur(k,iyr)
      !enddo
      !enddo
      ! ****************************************************************


      deallocate(redat)
      close(f+100)

      return
      end subroutine


      
      
