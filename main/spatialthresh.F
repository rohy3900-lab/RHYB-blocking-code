      subroutine spatialthreshold(label,nlabel,S,pixarea,maxlabel)

! This subroutine applies area threshold (S) to 
! array of labels whose anomalies are >= (A)

#include "qq.h" 
      
      integer:: i,j,k,S
      integer, dimension(2,2):: nlabel
      real, dimension(ix,il):: label
      dimension:: pixarea(ix,il)
      ! Area of labels found on nday
      integer, dimension(nlabel(2,1)-nlabel(1,1)+1):: labelarea

      integer::maxlabel
    
      ! If map not empty, do spatial thresholding
      if (sum(label).GT.0) then 
  
       ! Scanning for area covered by unique anomaly 
       call findarea(nlabel,label,labelarea,pixarea)

       ! Applying threshold for anomalies < (S)
       do k=nlabel(1,1),nlabel(2,1)
         if (labelarea(k-nlabel(1,1)+1).LT.S) then
          labelarea(k-nlabel(1,1)+1) = 0    
             do i=1,ix
               do j=iy,il
                 if (label(i,j).EQ.k) then
                     label(i,j)=0
                  endif
               enddo
             enddo
         endif
       enddo
        
      if (sum(label).GT.0) then
       ! Find new min/max labels
       nlabel(1,1) = minval(label,mask=label.GT.0)
       nlabel(2,1) = maxval(label,mask=label.GT.0)
      else
      ! set current min/max labels as those of nday-1
       nlabel(1,1) = nlabel(1,2)
       nlabel(2,1) = nlabel(2,2)
      endif      

      ! Reorder label values if not separated by 1 (eg 1,2,4,5)
      call labelreorder(nlabel,label,maxlabel)

      if (sum(label).GT.0) then
       ! Find new min/max labels
       nlabel(1,1) = minval(label,mask=label.GT.0)
       nlabel(2,1) = maxval(label,mask=label.GT.0)
      else
      ! set current min/max labels as those of nday-1
       nlabel(1,1) = nlabel(1,2)
       nlabel(2,1) = nlabel(2,2)
      endif      
 
      endif

      return 
      end subroutine spatialthreshold


       subroutine findarea(nlabel,label,labelarea,pixarea)

! This subroutine finds the area covered by each contiguous 
! collection of anomalies at the time "nday" 

#include "qq.h" 

        integer::i,j,k
        integer,dimension(2,2)::nlabel
        real,dimension(ix,il)::label,pixarea
        integer,dimension(nlabel(2,1)-nlabel(1,1)+1)::labelarea

        do k=nlabel(1,1),nlabel(2,1)
       
         ! Initializing
         labelarea(k-nlabel(1,1)+1)=0
         
         do i=1,ix
           do j=iy,il
             ! If grid point equals label, add the area covered by 
             ! that gridpoint to labelarea
             if (label(i,j).EQ.k) then
              labelarea(k-nlabel(1,1)+1)=labelarea(k-nlabel(1,1)+1)
     &        +pixarea(i,j)
             endif
           enddo
         enddo
        enddo

       return
       end subroutine findarea
