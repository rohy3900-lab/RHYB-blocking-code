       subroutine stat3(label,nlabel,dur,onset,iyear,m,iday,nday)

#include "qq.h"

       ! seonhwa12@kopri.re.kr
       ! Records the duration and onset date of each label
     

       integer::i,j,k
       integer::mstar,mdash,iyearstar
       integer::m,iyear,iday,nday
       ! last label assigned
       integer, dimension(2,2):: nlabel 
       real, dimension(ix,il):: label,dummy
       integer, dimension(lim,nyr):: dur
       integer, dimension(lim,nyr):: onset
        

       ! This dummy code will only track the duration of blocks
       ! that exist within a particular geographical area.
       ! i.e this subroutine will eliminate blocks not in 
       ! an area of interest.
       do i=1,ix
        do j=1,il
          dummy(i,j)=label(i,j) ! all blocks
c          if ((i.LT.48).OR.(i.GT.72)) label(i,j)=0 !Kamchatka blocks
c          if ((i.GT.15).AND.(i.LT.118)) label(i,j)=0 ! ATL blocks
c          if ((i.LT.54).OR.(i.GT.80)) label(i,j)=0 ! PAC Blocks
        enddo
       enddo

c       print*,nday,nlabel(1,1),nlabel(2,1),dur(1,1),onset(1,1)



       do k=nlabel(1,1),nlabel(2,1)
         
         ! if a specific label exists that day
         if (count(mask=int(label).EQ.k).GT.0) then
            
            dur(k,iyear-nyear+1)=dur(k,iyear-nyear+1)+1

            !**************************************************
            ! seonhwa12@kopri.re.kr
            if (nday.gt.1) then
            if ((iday.EQ.1).AND.(dur(k,iyear-nyear).NE.0)) then 
            dur(k,iyear-nyear+1)=dur(k,iyear-nyear)+1
            endif
            endif 
c            print*,'k = ',k,dur(1,1),onset(1,1)

            ! if onset date not recorded (ie onset ~= 1) 
            if (sum(onset(k,:)).EQ.0) then 
             onset(k,iyear-nyear+1) = nday 
            endif
            if ((iday.EQ.1).AND.(onset(k,iyear-nyear+1).EQ.0)) then
             onset(k,iyear-nyear+1) = onset(k,iyear-nyear)
            endif
            !**************************************************

         endif ! specific label exist endif   
       enddo ! k enddo

c       print*,nday,nlabel(1,1),nlabel(2,1),dur(1,1),onset(1,1)

       do i=1,ix
        do j=1,il
          label(i,j)=dummy(i,j)
        enddo
       enddo
       
       return
       end subroutine stat3




       




       
