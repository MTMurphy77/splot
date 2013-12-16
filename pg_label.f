c MM 3/1/01 Subroutine to place labels on a subpanel.
      subroutine pg_label(panel,nhori,nvert,xmin,xmax,ymin,ymax)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      real xin,yin,xmin,xmax,ymin,ymax
      real anotpos(maxnspec,maxanot,2)
      integer panel,nhori,nvert,i
      character*1 pgchar
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      common /label/label,anot,anotpos

 1    call PGSCI(2)
      call PGBAND(7,0,0.0,0.0,xin,yin,pgchar)
      if (max(nhori,nvert).eq.1) then
         call PGSCH(1.5)
      elseif (max(nhori,nvert).eq.2) then
         call PGSCH(1.7)
      elseif (max(nhori,nvert).eq.3) then
         call PGSCH(1.9)
      elseif (max(nhori,nvert).eq.4) then
         call PGSCH(2.1)
      endif
      if (pgchar.eq.'x') then
         call pg_anot('B',xin,yin,label(panel,1))
      elseif (pgchar.eq.'y') then
         call pg_anot('L',xin,yin,label(panel,2))
      elseif (pgchar.eq.'t') then
         call pg_anot('T',xin,yin,label(panel,3))
      elseif (pgchar.eq.'A') then
         i=0
 2       i=i+1
         if (i.gt.maxanot) then
            WRITE(*,'(2(a))') 'WARNING: Number of anotations ',
     :           'exceeds maximum.'
            WRITE(*,'(2(a))') '         Increase "maxanot" parameter',
     :           'in splot.h.'
         elseif (anot(panel,i).eq.' ') then
            anotpos(panel,i,1)=(xin-xmin)/(xmax-xmin)
            anotpos(panel,i,2)=(yin-ymin)/(ymax-ymin)
            call pg_anot('P',xin,yin,anot(panel,i))
         else
            goto 2
 3       endif
      elseif (pgchar.eq.'q') then
         return
      else
         goto 1
      endif

      return
      end
