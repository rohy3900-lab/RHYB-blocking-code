      subroutine GHG(nlabel,label,z,anz,sgrad,dphi,dlambda,maxlabel)

#include "qq.h"

      ! This subroutine isolates anomalies that satisfy a meridional
      ! gradient criteria

      ! Zonal and meridional scales
      integer:: dphi,dlambda      
      integer, dimension(2,2):: nlabel
      ! Gradient criteria
      real:: sgrad 
      real, dimension(ix,il):: label,z
      ! Position of anomaly maximum
      integer, dimension(lim):: Ipos,Jpos
      ! Gradient
      real:: GHGS
      ! Dummy variable to handle vector output of maxloc 
      integer, dimension(2):: posit  
      ! Anomaly data
      real, dimension(ix,il):: anz ! anomaly data
      ! Dummy Variables
      real,dimension(ix):: XXX 
      real,dimension(il):: YYY 
      ! Periodic boundary function
      integer, external:: pbdry 

      integer:: c

      integer::maxlabel
      

      ! If label exists at this timestep
      if (sum(label).GT.0) then
      
       do k=nlabel(1,1),nlabel(2,1)

        posit = maxloc(anz,mask=label.EQ.k)
        ! maxloc outputs 0 if it cannot find a maximum
 
        ! Position of maximum
        Ipos(k)=posit(1)
        Jpos(k)=posit(2)

        ! Can only define gradient for certain Ipos
        ! because of variation in Jpos for max gradient
        ! (dphi/latres converts degrees to grid points)
         if ((Jpos(k)-nint((dphi+dphi/2)/latres).LT.iy)
     &    .OR.(Jpos(k)+nint((dphi/2)/latres).GT.il)) then

           ! Remove label
           Ipos(k)=0
           Jpos(k)=0
           do i=1,ix
            do j=iy,il
             if (label(i,j).EQ.k) label(i,j)=0
            enddo
           enddo
     
        else

         ! Initialize gradient
         GHGS=0 
         c=0

   
         ! FINDING THE SUM OF MAXIMIZED GRADIENTS FOR SOME LONGITUDES **
        do i=Ipos(k)-nint((dlambda/latres)/2),Ipos(k)+
     &                               nint((dlambda/latres)/2)
          do j=1,il
           YYY(j)=0
          enddo

          do j=Jpos(k)-nint((dphi/latres)/2),Jpos(k)+
     &                                nint((dphi/latres)/2)
             ! Two point gradient
             YYY(j)= z(pbdry(i),j)-z(pbdry(i),j-nint(dphi/latres)) 
          enddo
 
           if (maxval(YYY).GT.sgrad) c=c+1

         enddo 
         ! *************************************************************

         ! If gradient not reversed then remove label 
          if (c.EQ.0) then
           do i=1,ix
            do j=iy,il
             if (label(i,j).EQ.k) label(i,j)=0
            enddo
           enddo
         endif
        
        endif ! Ipos condition endif
    
       enddo ! label enddo



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


      endif  ! no label on field endif

      return 
      end subroutine GHG



      integer function pbdry(i)

#include "qq.h"

      ! maintains periodicity of longitudinal values

      integer:: i
      integer:: dummy

      if (i.GT.ix) then
       dummy = i-ix-1
       pbdry=1+dummy
      elseif (i.LT.1) then
       dummy = i-1+1
       pbdry=ix+dummy
      else
       pbdry=i
      endif
      
      end function pbdry
