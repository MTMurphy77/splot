c MM 28/11/00 Subroutine to find the resize parameters from user input
      subroutine resize(pgchar,xin,yin,xmin,xmax,ymin,ymax)
      implicit none
      real xin,yin,xmin,xmax,ymin,ymax,temp,dummy
      character*1 pgchar
      
      if (pgchar.eq.'a') then
         xmin=xin
 1       call PGBAND(6,0,0.0,0.0,xmax,dummy,pgchar)
         if (pgchar.eq.'q') return
         if (pgchar.ne.'a') goto 1
         if (xmax.lt.xmin) then
            temp=xmin
            xmin=xmax
            xmax=temp
         endif
      elseif (pgchar.eq.'A') then
         xmin=xin
         ymin=yin
 2       call PGBAND(2,0,xin,yin,xmax,ymax,pgchar)
         if (pgchar.eq.'q') return
         if (pgchar.ne.'A') goto 2
         if (xmax.lt.xmin) then
            temp=xmin
            xmin=xmax
            xmax=temp
         endif
         if (ymax.lt.ymin) then
            temp=ymin
            ymin=ymax
            ymax=temp
         endif
      elseif (pgchar.eq.'w') then
 3       call PGBAND(5,0,0.0,0.0,dummy,temp,pgchar)
         if (pgchar.eq.'q') return
         if ((pgchar.ne.'b').and.(pgchar.ne.'t')) goto 3
         if (pgchar.eq.'b') then
            ymin=temp
         else
            ymax=temp
         endif
      endif

      return
      end
