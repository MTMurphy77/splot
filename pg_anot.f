c     MM 4/1/01 Subroutine to anotate a plot with input from the user at
c     a defined position on the plot
      subroutine pg_anot(pos,xpos,ypos,label)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      real xpos,ypos,xin,yin,disp
      integer i
      character*1 pos,pgchar
      character*(namelen) label
      SAVE zerochar
      character*(namelen) zerochar
      SAVE first
      logical first
      data first/.true./
      
      if (first) then
         first=.false.
         do i=1,nmcharlength
            zerochar(i:i)=' '
         enddo
      endif

      if (pos.ne.'P') then
         if (pos.eq.'B') then
            disp=xdisp
         elseif (pos.eq.'L') then
            disp=ydisp
         else
            disp=tdisp
         endif
         call PGSCI(0)
         call PGMTXT(pos,disp,0.5,0.5,label)
         label=zerochar
         i=0
 1       i=i+1
         call PGSCI(3)
         if (i.eq.1) then
            call PGMTXT(pos,disp,0.5,0.5,'_')
         else
            call PGMTXT(pos,disp,0.5,0.5,label(1:i-1)//'_')
         endif
         call PGBAND(0,0,0.0,0.0,xin,yin,pgchar)
         if (ichar(pgchar).eq.13) goto 2
         call PGSCI(0)
         if (i.eq.1) then
            call PGMTXT(pos,disp,0.5,0.5,'_')
         else
            call PGMTXT(pos,disp,0.5,0.5,label(1:i-1)//'_')
         endif
         if (ichar(pgchar).eq.8) then
            label(i-1:i-1)=' '
            i=i-2
         else
            label(i:i)=pgchar
         endif
         goto 1
         
 2       call PGSCI(0)
         call PGMTXT(pos,disp,0.5,0.5,label(1:i-1)//'_')
         call PGSCI(3)
         call PGMTXT(pos,disp,0.5,0.5,label(1:i-1))
      else
         call PGSCH(anotsize)
         i=0
 3       i=i+1
         call PGSCI(1)
         if (i.eq.1) then
            call PGTEXT(xpos,ypos,'_')
         else
            call PGTEXT(xpos,ypos,label(1:i-1)//'_')
         endif
         call PGBAND(0,0,0.0,0.0,xin,yin,pgchar)
         if (ichar(pgchar).eq.13) goto 4
         call PGSCI(15)
         if (i.eq.1) then
            call PGTEXT(xpos,ypos,'_')
         else
            call PGTEXT(xpos,ypos,label(1:i-1)//'_')
         endif
         if (ichar(pgchar).eq.8) then
            label(i-1:i-1)=' '
            i=i-2
         else
            label(i:i)=pgchar
         endif
         goto 3
         
 4       call PGSCI(15)
         call PGTEXT(xpos,ypos,label(1:i-1)//'_')
         call PGSCI(1)
         call PGTEXT(xpos,ypos,label(1:i-1))
      endif

      return
      end
