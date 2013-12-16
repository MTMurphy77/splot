c MM 1/12/00 Subroutine to select which plot to operate on
      subroutine selectplot(spec_ori,npnts_ori,spec,npnts,filenm,
     :     contnm,signm,nspec,xmin,xmax,ymin,ymax,vplimits,panel,
     :     nhori,nvert)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      double precision spec_ori(maxnspec,maxspecdim,maxpnts)
      double precision spec(maxnspec,maxspecdim,maxpnts)
      real vplimits(maxnspec,4),xin,yin
      real xmin(maxnspec),xmax(maxnspec),ymin(maxnspec),ymax(maxnspec)
      integer nspec,npnts(maxnspec),npnts_ori(maxnspec),panel
      integer nhori,nvert,i,J,model(maxnspec),xpan,ypan
      character*1 pgchar,charnspec
      character*(namelen) filenm(maxnspec),contnm(maxnspec)
      character*(namelen) signm(maxnspec)
      logical err
      common /model/model
      
 1    do i=1,nhori
         do j=1,nvert

         enddo
      enddo

      call PGSCI(2)
      call PGBAND(0,0,0.0,0.0,xin,yin,pgchar)
      if (pgchar.eq.'q') then
         call PGCLOS
         stop
      elseif (pgchar.eq.'r') then
         call rspec(filenm(nspec+1),contnm(nspec+1),signm(nspec+1),
     :        nspec+1,spec_ori,npnts_ori(nspec+1),.true.,err)
         if (.not.err) then
            nspec=nspec+1
            call spec_copy(spec_ori,nspec,npnts_ori(nspec),spec,nspec,
     :           npnts(nspec))
            call setvplims(vplimits,nspec,nhori,nvert)
            call PGSUBP(1,1)
            call PGERAS
            call PGSUBP(-nhori,nvert)
            call pg_range(spec,nspec,2,npnts(nspec),
     :           xmin(nspec),xmax(nspec),ymin(nspec),ymax(nspec))
            do i=1,nspec
               call plotdat(spec,i,npnts(i),xmin(i),xmax(i),ymin(i),
     :           ymax(i),vplimits,i,nhori,nvert,.true.)
            enddo
            model(nspec)=0
         endif
         goto 1
      endif
      
      write(charnspec,'(a)') char(64+nspec)
      if (pgchar.lt.'A'.or.pgchar.gt.charnspec) goto 1
      panel=ichar(pgchar)-64
      call pg_panl(panel,nhori,nvert,xpan,ypan)
      call PGPANL(xpan,ypan)
      call PGSWIN(xmin(panel),xmax(panel),ymin(panel),ymax(panel))

      return
      end
