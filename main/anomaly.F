       subroutine anomaly(anz,A)

! At every time step, this subroutine finds anomalies 
! that are above a threshold (A)

#include "qq.h"
       ! seonhwa12@kopri.re.kr

       ! rohy3900@gmail.com  

       integer:: i, j
       real, dimension(ix, il) :: A    ! 2-D array A, with thresholds for specific months
       real, dimension(ix, il) :: anz  ! 2-D array anz
       integer :: min_a                ! minimum anomaly 

       min_a = 170 

c      Isolating anomalies
       do i = 1, ix
         do j = 1, il
           if (anz(i,j) .LE. min_a .OR. anz(i,j) .LE. A(i,j)) then
             anz(i,j) = 0 
           end if 
           if (j .LE. llim) anz(i, j) = 0       
         enddo
       enddo

       return
       end subroutine anomaly
