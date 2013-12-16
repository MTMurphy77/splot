c MM Subroutine to plot a single spectrum from a spectrum matrix
      subroutine plotdat(spec,nspec,npnts,xmin,xmax,
     :     ymin,ymax,vplimits,panel,nhori,nvert,letter)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      real x(maxpnts),y(maxpnts),xmin,xmax,ymin,ymax
      real vplimits(maxnspec,4),anotpos(maxnspec,maxanot,2)
      integer nspec,npnts,oldci,oldls,dtype
      integer hist(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer plot(maxnspec,maxspecdim),panel,nhori,nvert
      integer xpan,ypan,i
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      logical letter
      common /hist/hist
      common /pcol/pcol
      common /plot/plot
      common /label/label,anot,anotpos

      call PGQCI(oldci)
      call PGQLS(oldls)
      call pg_panl(panel,nhori,nvert,xpan,ypan)
      call PGPANL(xpan,ypan)
      call PGERAS
      call PGSVP(vplimits(panel,1),vplimits(panel,2),
     :     vplimits(panel,3),vplimits(panel,4))
      call PGSWIN(xmin,xmax,ymin,ymax)
      call PGSCI(15)
      call PGRECT(xmin,xmax,ymin,ymax)
      call PGSCI(5)
      call PGSLW(2)
      if (max(nhori,nvert).eq.1) then
         call PGSCH(1.5)
      elseif (max(nhori,nvert).eq.2) then
         call PGSCH(1.7)
      elseif (max(nhori,nvert).eq.3) then
         call PGSCH(1.9)
      elseif (max(nhori,nvert).eq.4) then
         call PGSCH(2.1)
      endif
      call PGBOX('BCST',0.0,0,'BCST',0.0,0)
      if (letter) then
         call PGSCI(14)
         call PGSCH(10.0)
         call PGSLW(10)
         call PGMTXT('LV',-0.4,0.5,0.5,char(64+panel))
         call PGSLW(2)
      endif
      call PGSCH(anotsize)
      call PGSCI(1)
      do i=1,maxanot
         if (anot(panel,i)(1:1).ne.' ') then
            call PGTEXT(anotpos(panel,i,1)*(xmax-xmin)+xmin,
     :           anotpos(panel,i,2)*(ymax-ymin)+ymin,
     :           anot(panel,i))
         endif
      enddo
      if (max(nhori,nvert).eq.1) then
         call PGSCH(1.5)
      elseif (max(nhori,nvert).eq.2) then
         call PGSCH(1.7)
      elseif (max(nhori,nvert).eq.3) then
         call PGSCH(1.9)
      elseif (max(nhori,nvert).eq.4) then
         call PGSCH(2.1)
      endif
      call PGSCI(3)
      call PGMTXT('B',xdisp,0.5,0.5,label(panel,1))
      call PGMTXT('L',ydisp,0.5,0.5,label(panel,2))
      call PGMTXT('T',tdisp,0.5,0.5,label(panel,3))
      call PGSCI(7)
      call PGBOX('NV',0.0,0,'NV',0.0,0)
      do dtype=2,maxspecdim
         call pg_data(spec,nspec,dtype,npnts,x,y)
         call PGSCI(pcol(nspec,dtype))
         if (plot(nspec,dtype).eq.1) then
            if (hist(nspec,dtype).eq.6) then
               call PGBIN(npnts,x,y,.true.)
            else
               call PGSLS(hist(nspec,dtype))
               call PGLINE(npnts,x,y)
            endif
         endif
      enddo

      call PGSCI(oldci)
      call PGSLS(oldls)

      return
      end
