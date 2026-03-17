       subroutine datetrack(lmonth,iday,iyear,m,yrtype)

       ! tracks the date (year month day) at every nday time in loop
       ! Note: if yrtype = 1 then this algorithm tracks time in a 365 
       ! day calendar WITH lpyrs
       integer:: m,iday,iyear,yrtype
       integer, dimension(12):: lmonth 
       
       iday=iday+1

       if (yrtype.EQ.1)  call leapyear(iyear,lmonth)

       if (iday.GT.sum(lmonth(1:m))) then
        m=m+1
       endif

       if (m.GT.12) then
        iday=1
        m=1
        iyear=iyear+1
        if (yrtype.EQ.1) call leapyear(iyear,lmonth)
       endif

       return
       end subroutine datetrack
     
     
       subroutine leapyear(iyear,lmonth)

       ! finds if the year is a leap year and then 
       ! changes feb amount of days accordingly

       integer:: iyear
       integer, dimension(12):: lmonth

       if (mod(iyear,400).EQ.0) then
          lmonth(2)=29 ! is leap year
       elseif (mod(iyear,100).EQ.0) then
          lmonth(2)=28 ! not leap year
       elseif (mod(iyear,4).EQ.0) then
          lmonth(2)=29 ! is leap year
       else
          lmonth(2)=28 ! not leap year
       endif

       return
       end subroutine
