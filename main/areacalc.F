       subroutine areacalc(pixarea)

! This subroutine calculates the geographical 
! area covered by a grid point.

! The algorithm is taken from:
! mathforum.org/library/drmath/view/63767.html

#include "qq.h"     

       integer:: i,j,nlabel
       ! grid resolution and radius of earth (km)
       real:: jgrid,igrid,R=6378.1
       real:: pi=3.14159265
       dimension:: label(ix,il)
       dimension:: pixarea(ix,il)
       real, dimension(il):: lat
       real, dimension(ix):: lon

      
       jgrid=latres
       igrid=lonres

       ! Assigning a lat point to each grid point    
       do j=iy,il
         lat(j) = deg2rad((j-1)*jgrid - 90)
       enddo

       jgrid = deg2rad(jgrid/2)

       ! Calculating the area each grid point covers
       do i=1,ix
         do j=iy,il 
        pixarea(i,j)=abs(sin(lat(j)-jgrid)-sin(lat(j)+jgrid))
        pixarea(i,j)=(2*pi)*(R**2)*pixarea(i,j)*(igrid/360)
         enddo
       enddo
    
       return 
       end subroutine areacalc


       real function deg2rad(n)
       ! converts degrees to radians
       real:: pi=3.14
       real:: n
       deg2rad = n*(pi/180)
       return
       end function deg2rad 
         
