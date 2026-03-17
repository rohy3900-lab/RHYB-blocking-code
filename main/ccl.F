      subroutine ccl(z,label,nlabel,maxlabel)

! Connected component algorithm with recursive longitudinal boundary condition
! used to uniquely identify contigous collections of gridpoints
! Algorithm found at: 
! http://blogs.mathworks.com/steve/2007/05/11/connected-component-labeling-part-5/

#include "qq.h"

      integer:: i,j
      ! Anomalies >= (A)
      real, dimension(ix,il) :: z   
      ! Array of unique labels for each anomaly    
      integer, dimension(ix,il):: scan  
      real, dimension(ix,il):: label
      ! label values of scanned neighbors
      integer, dimension(4):: SN         
      ! Tells us which labels refer to the same object
      integer, dimension(2,2000):: pairs
      ! The nth pairs   
      integer:: npair                   
      ! Current and past map max and min labels
      integer, dimension(2,2):: nlabel   

      integer::maxlabel

c  ALGORITHM START//////////////////////////////////////////////////////////////       

      nlabel(2,1)=maxlabel

      ! Initializing label scan array (binary and label part)
      call initialize(scan,z,label)

      if (sum(z).GT.0) then ! if map not empty do ccl
     

      ! ***********************************************************************************
      ! FIRST PASS (assigning unique labels) **********************************************
      ! ***********************************************************************************

       npair = 1 ! initializing counter to record number of equivalencies
       do i=1,ix
        ! Top to bottom exluding first an last point of collumn
        do j=(il-1),iy+1,-1            
          if (z(i,j).GT.0) then
             call makeSN(i,j,SN,scan,0)
             ! If neighbors have no labels
             if (count(mask=SN.GT.0).EQ.0) then 
                 scan(i,j) = nlabel(2,1)                         
                 nlabel(2,1) = nlabel(2,1) + 1
             ! If one neighbor has a label
             elseif (count(mask=SN .GT. 0).EQ.1) then 
                 scan(i,j) = minval(SN,mask=SN.GT.0)
             ! If at least two neighbors have a label
             elseif (count(mask=SN .GT. 0).GT.1) then
                 scan(i,j) = minval(SN,mask=SN.GT.0 ) ! choose smallest neighbor                
                 call recordpairs(SN,scan,npair,pairs,i,j)
             endif
          endif
        enddo
       enddo
  
       ! ACCOUNTING FOR RECURSIVE LONGITUDINAL BOUNDARY CONDITION

       ! Making one final collumn scan for lon @ 0
       ! since 0 and 359 are neighbors. This will make 
       ! an equivalence between labels on last collumn and labels on first collumn

       ! First collumn
        do i=1,1 
         ! Top to bottom exluding first an last point of collumn    
         do j=(il-1),iy+1,-1 
           ! If label on first collumn not equal to zero    
           if (scan(i,j).NE.0) then                 
             call makeSN(i,j,SN,scan,1)
             
             ! If one neighbor has a label
             if (count(mask=SN .GT. 0).EQ.1) then       
               if (npair.EQ.1) then ! if first pair
                  pairs(1,npair) = scan(i,j)
                  pairs(2,npair) = minval(SN,mask=SN.GT.0) 
                  npair = npair + 1
               ! If >1st pair and not a redundent
               elseif (pairs(2,npair-1).NE.minval(SN,mask=SN.GT.0)) then                            
                  pairs(1,npair) = scan(i,j)
                  pairs(2,npair) = minval(SN,mask=SN.GT.0) 
                  npair = npair + 1
               endif
             ! If at least two neighbors have a label
             elseif (count(mask=SN .GT. 0).GT.1) then 
                 call recordpairs(SN,scan,npair,pairs,i,j)
             endif

           endif
         enddo
        enddo
      ! ******************************************************************************************
      ! ******************************************************************************************
      ! ******************************************************************************************         

 
      ! ***********************************************************************************
      ! SECOND PASS (cycle through and apply equivalency)**********************************
      ! ***********************************************************************************
        ! If at least one pair (recall last pair will be zeros)
        if (npair.GT.1) then
          do k=1,npair-1
            do i=1,ix
              ! Top to bottom exluding first an last point of collumn
              do j=(il-1),iy+1,-1 
                if (scan(i,j).EQ.pairs(2,k)) then
                  ! apply equivalency
                  scan(i,j)=pairs(1,k)
                endif
              enddo
            enddo
          enddo
        endif

        ! Outputting the real values from an integer array
        do i=1,ix
         do j=1,il
             label(i,j)=scan(i,j)
         enddo
        enddo
       ! ************************************************************************************
       ! ************************************************************************************
       ! ************************************************************************************
 
      if (sum(label).GT.0) then
       ! Find new min/max labels
       nlabel(1,1) = minval(label,mask=label.GT.0)
       nlabel(2,1) = maxval(label,mask=label.GT.0)
      else
      ! set current min/max labels as those of nday-1
       nlabel(1,1) = nlabel(1,2)
       nlabel(2,1) = nlabel(2,2)
      endif      

      endif ! blank map if statement
        
      return
      end subroutine ccl    






      subroutine initialize(scan,z,label)

#include "qq.h"

       ! Initializes the scan and label arrays

      integer::i,j
      integer, dimension(ix,il)::scan
      real, dimension(ix,il)::z
      real, dimension(ix,il):: label

      do i=1,ix
        do j=il,iy,-1 ! top to bottom
           ! Need first row to equal zero      
           if (j.EQ.iy.OR.j.EQ.il)  z(i,j) = 0
           scan(i,j) = 0 
           label(i,j) = 0
        enddo
      enddo    

      return
      end subroutine initialize



      subroutine makeSN(i,j,SN,scan,limit)

#include "qq.h"

      ! Creates a scanned neighborhood array

      integer:: i,j,limit
      ! label values of scanned neighbors
      integer, dimension(4):: SN       
      ! label values
      integer, dimension(ix,il)::scan  

      if (limit.EQ.0) then
        ! Defining scanned neighbors
        SN(1) = scan(i,j+1) 
      else
        SN(1) = 0
      endif

      SN(2) = scan(wrap(i-1),j+1)
      SN(3) = scan(wrap(i-1),j)
      SN(4) = scan(wrap(i-1),j-1)
      
      ! Removing values that occur more than once in the scanned neighbors to
      ! avoid a same valued pair equivalence
      !  (e.g  pairs(1,npair)= pairs(2,npair))        
      do k=1,5
        do l=1,5                   
           if (k.NE.l.AND.SN(k).EQ.SN(l)) SN(l)=0
        enddo
      enddo

      return
      end subroutine makeSN


      
      subroutine recordpairs(SN,scan,npair,pairs,i,j)

#include "qq.h"

      ! If at least two neighbors have a label, cycle through all labels and record pairs

      integer:: i,j,k
      integer, dimension(4):: SN   
      integer, dimension(ix,il)::scan
      integer:: npair  
      integer, dimension(2,30):: pairs  

      ! (while loop) if there remains an unrecorded equivalent pair
 10   if (count(mask=SN.GT.0).GT.1) then 

         do k=1,4
           if (SN(k).EQ.minval(SN,mask=SN.GT.0)) then
              SN(k) = 0 
              exit
           endif
         enddo

         ! if first pair   
         if (npair.EQ.1) then                  
            pairs(1,npair) = scan(i,j)
            pairs(2,npair) = minval(SN,mask=SN.GT.0)
            npair = npair + 1
         ! if >1st pair and not a redudent pair
         elseif (pairs(2,npair-1).NE.minval(SN,mask=SN.GT.0)) then                       
            pairs(1,npair) = scan(i,j)
            pairs(2,npair) = minval(SN,mask=SN.GT.0)  
            npair = npair + 1                        
         endif

      goto 10 
      endif

      return
      end subroutine recordpairs     

         

      real function wrap(n)

      ! function defined to wrap longitudinal edges of spatial array

#include "qq.h"

      integer:: n

      if (n .EQ. ix+1) then
         wrap = 1
      elseif (n .EQ. 0) then
          wrap = ix
      else
          wrap = n
      endif   

      return
      end
