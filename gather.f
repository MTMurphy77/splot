c     MM 14/2/01 Subroutine to plot a "gathered" plot of a registered
c     set of absorbers
      subroutine gather(spec,npnts,nspec,panel,nhori,nvert,spec_ion,
     :     spec_lev,spec_lam0,xmin,xmax,ymin,ymax,hard)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision spec_lam0(maxnspec)
      real xmin(maxnspec),xmax(maxnspec)
      real ymin(maxnspec),ymax(maxnspec)
      real anotpos(maxnspec,maxanot,2)
      real x(maxpnts),y(maxpnts)
      integer npnts(maxnspec)
      integer hist(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer plot(maxnspec,maxspecdim)
      integer nspec,panel
      integer nvert,nhori
      integer i,j,oldci,oldls,xpan,ypan,dtype
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      character*(ionlen) spec_ion(maxnspec),spec_lev(maxnspec)
      logical hard
      common /label/label,anot,anotpos
      common /hist/hist
      common /pcol/pcol
      common /plot/plot

      call PGSUBP(1,1)
      call PGERAS
      call PGQCI(oldci)
      call PGQLS(oldls)
      do i=1,nspec
         call PGSVP(vpllimit+real((i-1)/nvert)*(vprlimit-vpllimit)/
     :        real(nhori),
     :        vpllimit+real((i-1)/nvert+1)*(vprlimit-vpllimit)/
     :        real(nhori),
     :        vpulimit-real(i-nvert*((i-1)/nvert))*(vpulimit-vpdlimit)
     :        /real(nvert),
     :        vpulimit-real(i-1-nvert*((i-1)/nvert))*(vpulimit-vpdlimit)
     :        /real(nvert))
         call PGSWIN(xmin(i),xmax(i),ymin(i),ymax(i))
         if (hard) then
            call PGSCI(0)
         else
            call PGSCI(15)
         endif
         call PGRECT(xmin(i),xmax(i),ymin(i),ymax(i))
         call PGSCI(5)
         call PGSLW(2)
         if (max(nhori,nvert).eq.1) then
            call PGSCH(0.3)
         elseif (max(nhori,nvert).eq.2) then
            call PGSCH(0.4)
         elseif (max(nhori,nvert).eq.3) then
            call PGSCH(0.5)
         elseif (max(nhori,nvert).eq.4) then
            call PGSCH(0.6)
         endif
         call PGBOX('BCST',0.0,0,'BCST',0.0,0)
         call PGSCH(1.2)
         call PGSCI(1)
         do j=1,maxanot
            if (anot(i,j)(1:1).ne.' ') then
               call PGTEXT(anotpos(i,j,1)*(xmax(i)-xmin(i))+xmin(i),
     :              anotpos(i,j,2)*(ymax(i)-ymin(i))+ymin(i),
     :              anot(i,j))
            endif
         enddo
         if (max(nhori,nvert).eq.1) then
            call PGSCH(1.0)
         elseif (max(nhori,nvert).eq.2) then
            call PGSCH(1.1)
         elseif (max(nhori,nvert).eq.3) then
            call PGSCH(1.2)
         elseif (max(nhori,nvert).eq.4) then
            call PGSCH(1.3)
         endif
         call PGSCI(7)
         call pg_panl(i,nhori,nvert,xpan,ypan)
         if (ypan.eq.nvert.or.i.eq.nspec) call PGBOX('N',0.0,0,'',
     :        0.0,0)
         if (xpan.eq.1) call PGBOX('',0.0,0,'NV',0.0,0)
         do dtype=2,maxspecdim
            call pg_data(spec,i,dtype,npnts(i),x,y)
            call PGSCI(pcol(i,dtype))
            if (plot(i,dtype).eq.1) then
               if (hist(i,dtype).eq.6) then
                  call PGBIN(npnts(i),x,y,.true.)
               else
                  call PGSLS(hist(i,dtype))
                  call PGLINE(npnts(i),x,y)
               endif
            endif
         enddo
         call PGSCI(oldci)
         call PGSLS(oldls)
      enddo
      call PGSVP(vpllimit,vprlimit,vpdlimit,vpulimit)
      call PGSCI(3)
      call PGSCH(1.5)
      call PGMTXT('B',xdisp,0.5,0.5,label(panel,1))
      call PGMTXT('L',ydisp,0.5,0.5,label(panel,2))
      call PGMTXT('T',tdisp,0.5,0.5,label(panel,3))
      call PGSCI(oldci)
      call PGSLS(oldls)

      return
      end
