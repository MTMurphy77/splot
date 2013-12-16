c     MM 28/11/00 Subroutine to change the plotting parameters
c     (e.g. what parts of the data are plotted, types of lines to be
c     used for plotting, colours used etc)
      subroutine cpparms(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :     panel,nhori,nvert)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      real xin,yin,xmin,xmax,ymin,ymax,vplimits(maxnspec,4)
      integer nspec,npnts,dtype
      integer plot(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer hist(maxnspec,maxspecdim),model(maxnspec)
      integer oldcol,oldplot,oldhist,panel,nhori,nvert
      character*1 pgchar
      logical changecol
      common /plot/plot
      common /hist/hist
      common /pcol/pcol
      common /model/model

 1    call PGBAND(7,0,0.0,0.0,xin,yin,pgchar)
      
      if (pgchar.eq.'q') then
         return
      elseif (pgchar.eq.'d'.or.pgchar.eq.'c'.or.pgchar.eq.'e'
     :        .or.pgchar.eq.'z') then
         if (pgchar.eq.'d') dtype=2
         if (pgchar.eq.'c') dtype=3
         if (pgchar.eq.'e') dtype=4
         if (pgchar.eq.'z') dtype=8
         changecol=.false.
         oldcol=pcol(nspec,dtype)
         oldplot=plot(nspec,dtype)
         oldhist=hist(nspec,dtype)
         pcol(nspec,dtype)=oldcol+3
         if (pcol(nspec,dtype).gt.14) pcol(nspec,dtype)=
     :        pcol(nspec,dtype)-14
         plot(nspec,dtype)=1
         call plotdat(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :        vplimits,panel,nhori,nvert,.true.)
 2       call PGBAND(7,0,0.0,0.0,xin,yin,pgchar)
         if (pgchar.eq.'q') then
            if (.not.changecol) pcol(nspec,dtype)=oldcol
            call plotdat(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :           vplimits,panel,nhori,nvert,.true.)
            return
         elseif (pgchar.eq.'r') then
            pcol(nspec,dtype)=oldcol
            plot(nspec,dtype)=oldplot
            hist(nspec,dtype)=oldhist
            call plotdat(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :           vplimits,panel,nhori,nvert,.true.)
            goto 2
         elseif (pgchar.eq.'c') then
            if (.not.changecol) changecol=.true.
            pcol(nspec,dtype)=pcol(nspec,dtype)+1
            if (pcol(nspec,dtype).gt.14) pcol(nspec,dtype)=1
            call plotdat(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :     vplimits,panel,nhori,nvert,.true.)
            goto 2
         elseif (pgchar.eq.'t') then
            hist(nspec,dtype)=hist(nspec,dtype)+1
            if (hist(nspec,dtype).gt.6) hist(nspec,dtype)=1
            call plotdat(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :     vplimits,panel,nhori,nvert,.true.)
            goto 2
         elseif (pgchar.eq.'d') then
            if (plot(nspec,dtype).eq.1) then
               plot(nspec,dtype)=0
            else
               plot(nspec,dtype)=1
            endif
            call plotdat(spec,nspec,npnts,xmin,xmax,ymin,ymax,
     :     vplimits,panel,nhori,nvert,.true.)
            goto 2
         endif
      else
         return
      endif
      
      return
      end
