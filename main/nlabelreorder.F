        subroutine nlabelreorder(nlabel,label)

        ! This subroutine finds the min and max label
        ! values on a field.
        ! If none exist (say if they were deleted) then labels 
        ! are retaken from the previous timestep

#include "qq.h" 
         
        integer, dimension(2,2):: nlabel
        real, dimension(ix,il):: label

        if (sum(label).GT.0) then
         ! Find new min/max labels
         nlabel(1,1) = minval(label,mask=label.GT.0)
         nlabel(2,1) = maxval(label,mask=label.GT.0)+1
        else
           ! set current min/max labels as those of nday-1
           nlabel(1,1) = nlabel(1,2)
           nlabel(2,1) = nlabel(2,2)
        endif
              
        return
        end subroutine nlabelreorder
