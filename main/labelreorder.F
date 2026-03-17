       subroutine labelreorder(nlabel,label,maxlabel)

#include "qq.h" 

       ! Reorder label values if not separated by 1 (eg 1,2,4,5)

       integer::k,i,j,n
       integer:: minlabel
       integer, dimension(2,2):: nlabel
       real, dimension(ix,il):: label

       integer::maxlabel
             
       if (sum(label).GT.0) then  ! if map not blank
        ! do maxlabel (smallest label that if it exists, should be on map)
        ! to largest label on map      
        do k=maxlabel,nlabel(2,1) 
          ! if the max label is <= k, then we can stop reordering
          if (maxval(label).LE.k) exit 
           minlabel=minval(label,mask=label.GE.k)
           if (minlabel.NE.k) then
             do i=1,ix
              do j=iy,il
               if (label(i,j).EQ.minlabel)  label(i,j)= k   
              enddo
             enddo      
            endif
        enddo
       endif

      return
      end subroutine labelreorder    
