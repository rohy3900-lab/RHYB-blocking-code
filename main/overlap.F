      subroutine overlap(nday,O,clabel,plabel,nlabel,maxlabel)
#include "qq.h" 

      ! Subroutine to determine temporal coherence.
      ! i.e. if two anomalies overlap by (O)% of points
      ! then the later one gains the label of the previous one

      integer::k,l,i,j,c=0
      integer:: nday
      real:: O
      integer, dimension(2,2):: nlabel
      real, dimension(ix,il):: clabel,plabel
      ! Dummy variable
      integer, dimension(2):: XXX
      ! Overlaping Points
      integer, dimension(ix,il):: opts 
      ! Records the sum of overlaping points shared by two labels
      real:: sumopts(nlabel(2,1)-nlabel(1,1)+1,
     &                      nlabel(2,2)-nlabel(1,2)+1)
      ! Records the total number of points belonging to a label
      ! of the previous and current field
      real::numlabelpast(nlabel(2,2)-nlabel(1,2)+1)
      real::numlabelcur(nlabel(2,1)-nlabel(1,1)+1)

      integer:: maxlabel


      ! Overlaping defined only on nday >= 2
      if (nday.GT.1) then 

c (1) CALCULATING OVERLAPPING POINTS
      do i=1,ix
       do j=iy,il
          opts(i,j)=clabel(i,j)*plabel(i,j)
       enddo
      enddo

      ! if overlaping points exist
      if (count(mask=opts.GT.0).GT.0) then 

      ! Initializing 
      do l=nlabel(1,2),nlabel(2,2) ! previous map labels
       do k=nlabel(1,1),nlabel(2,1) ! current map labels
         sumopts(k-nlabel(1,1)+1,l-nlabel(1,2)+1)=0
         numlabelcur(k-nlabel(1,1)+1)=0
       enddo
         numlabelpast(l-nlabel(1,2)+1)=0       
      enddo

c (2) DETERMINING WHO OVERLAPS WITH WHO AND HOW MUCH
      do i=1,ix
       do j=iy,il       
         ! if Overlaping point exists
         if (opts(i,j).GT.0) then 

           do k=nlabel(1,1),nlabel(2,1) ! current map labels
            do l=nlabel(1,2),nlabel(2,2) ! previous map labels
                   
             if ((clabel(i,j).EQ.k).AND.
     +               (plabel(i,j).EQ.l)) then
                XXX(1)= k-nlabel(1,1)+1 ! made it this way because its too big
                XXX(2)= l-nlabel(1,2)+1
                sumopts(XXX(1),XXX(2))=sumopts(XXX(1),XXX(2))+1 
             endif

            enddo
           enddo 
              
         endif
       enddo
      enddo

c (3) FINDING THE NUMBER OF POINTS EACH PAST AND CURRENT LABEL HAS
      do i=1,ix
       do j=iy,il

        do l=nlabel(1,2),nlabel(2,2) ! previous map labels
         if (plabel(i,j).EQ.l) then
          numlabelpast(l-nlabel(1,2)+1)=numlabelpast(l-nlabel(1,2)+1)+1
         endif
        enddo

        do k=nlabel(1,1),nlabel(2,1) ! current map labels
         if (clabel(i,j).EQ.k) then             
           numlabelcur(k-nlabel(1,1)+1)=numlabelcur(k-nlabel(1,1)+1)+1
         endif
        enddo

       enddo
      enddo

c (4) FINDING THE OVERLAP PERCENTAGE FOR EACH CURRENT LABEL
      do l=nlabel(1,2),nlabel(2,2) ! previous map labels
       do k=nlabel(1,1),nlabel(2,1) ! current map labels

        XXX(1)= k-nlabel(1,1)+1 ! made it this way because its too big
        XXX(2)= l-nlabel(1,2)+1
            
        if (numlabelpast(XXX(2)).GT.0) then
      sumopts(XXX(1),XXX(2))=sumopts(XXX(1),XXX(2))/numlabelpast(XXX(2))
        endif

       enddo
      enddo

c (5) TAKING CARE OF MULTIPLE OVERLAP CASES
      ! if there exists maps with multiple cases of overlap,
      ! choose the largest overlap to connect labels in time,
      ! if multiple overlap with 100% then...
      call multipleoverlap(nlabel,sumopts,O,clabel,numlabelcur)

c (6) SUFFICIENCY OF OVERLAP AND SWITCHING OF LABELS
      ! determining whether the overlap is sufficient
      ! and if so making the label switch       
      do l=nlabel(1,2),nlabel(2,2) ! previous map labels
       do k=nlabel(1,1),nlabel(2,1) ! current map labels

        XXX(1)= k-nlabel(1,1)+1 ! made it this way because its too big
        XXX(2)= l-nlabel(1,2)+1

        ! if overlap criteria is met
        if (sumopts(XXX(1),XXX(2)).GE.O) then
          ! counter to determine whether or not any overlap fulfills the (O)
          ! criteria
          c=c+1 
          ! Perform label switch
          do i=1,ix
           do j=iy,il
             if (clabel(i,j).EQ.k) then
               ! give the current anomaly the label of the past anomaly
               clabel(i,j)= l 
             endif
           enddo
          enddo
           
         endif
       enddo
      enddo

        
      !call nlabelreorder(nlabel,clabel)
      ! Reorder label values if not separated by 1 (eg 1,2,4,5)
      !call labelreorder(nlabel,clabel,maxlabel)
      
      if (sum(clabel).GT.0) then
       ! Find new min/max labels
       nlabel(1,1) = minval(clabel,mask=clabel.GT.0)
       nlabel(2,1) = maxval(clabel,mask=clabel.GT.0)
      else
       ! set current min/max labels as those of nday-1
       nlabel(1,1) = nlabel(1,2)
       nlabel(2,1) = nlabel(2,2)
      endif



    
      endif ! if for whether or not ANY points overlap 
      endif ! nday >= 2 if

      return
      end subroutine overlap




      subroutine multipleoverlap(nlabel,sumopts,O,label,numlabelcur)

#include "qq.h"
 
      ! If there exists maps with multiple cases of overlap,
      ! choose the largest overlap to connect labels in time.

      real:: O, maxoverlap
      integer, dimension(2,2):: nlabel
      integer, dimension(2):: XXX
      real:: sumopts(nlabel(2,1)-nlabel(1,1)+1,
     &                      nlabel(2,2)-nlabel(1,2)+1)
      real, dimension(ix,il):: label
      real:: numlabelcur(nlabel(2,1)-nlabel(1,1)+1)

       ! Removing sumopts that dont satisfy overlap criteria (O)
       do l=nlabel(1,2),nlabel(2,2) ! previous map labels
         do k=nlabel(1,1),nlabel(2,1) ! current map labels
            XXX(1)= k-nlabel(1,1)+1 ! made it this way because its too big
            XXX(2)= l-nlabel(1,2)+1
            if (sumopts(XXX(1),XXX(2)).LT.O) sumopts(XXX(1),XXX(2))=0
         enddo
       enddo

       do l=nlabel(1,2),nlabel(2,2) ! previous map labels       
         XXX(2)= l-nlabel(1,2)+1
       
        ! if there exists multiple current labels overlaping with one 
        ! past label
        if (count(mask=sumopts(:,XXX(2)).GT.0).GT.1) then       
          maxoverlap= maxval(sumopts(:,XXX(2)))
           do k=nlabel(1,1),nlabel(2,1) ! current map labels
             XXX(1)= k-nlabel(1,1)+1
               if (sumopts(XXX(1),XXX(2)).NE.maxoverlap) then
                 sumopts(XXX(1),XXX(2))=0
               endif
           enddo
        endif
       enddo

       do k=nlabel(1,1),nlabel(2,1) ! current map labels      
          XXX(1)= k-nlabel(1,1)+1
           
        ! if there exists multiple past labels overlaping with one 
        ! current label
        if (count(mask=sumopts(XXX(1),:).GT.0).GT.1) then       
          maxoverlap= maxval(sumopts(XXX(1),:))
            do l=nlabel(1,2),nlabel(2,2) ! previous map labels   
               XXX(2)= l-nlabel(1,2)+1
                 if (sumopts(XXX(1),XXX(2)).NE.maxoverlap) then
                      sumopts(XXX(1),XXX(2))=0
                 endif
            enddo
        endif
       enddo

       return
       end subroutine multipleoverlap
