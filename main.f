c MM 1/12/00 Subroutine to present the main options to the user.
      subroutine main(spec,nspec,npnts,filenm,contnm,signm,xmin,
     :     xmax,ymin,ymax,vplimits,panel,nhori,nvert)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      include 'constants_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision spec_ori(maxnspec,maxspecdim,maxpnts)
      double precision spunit(maxnspec,2)
      double precision lambda0(maxnatom),f(maxnatom),gamma(maxnatom)
      double precision atmass(maxnatom),spec_lam0(maxnspec)
      double precision zabs,z
      real xmin(maxnspec),xmax(maxnspec),spzero(maxnspec)
      real ymin(maxnspec),ymax(maxnspec),xin,yin,dummyzero
      real vplimits(maxnspec,4),anotpos(maxnspec,maxanot,2)
      integer npnts(maxnspec),npnts_ori(maxnspec),nspec,pixnum
      integer hist(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer plot(maxnspec,maxspecdim),model(maxnspec)
      integer nvert,nhori,panel
      integer natomdat,i,tindex,spec_tind(maxnspec),xpan,ypan
      character*1 pgchar,spchar(maxnspec,2)
      character*(namelen) filenm(maxnspec),contnm(maxnspec)
      character*(namelen) signm(maxnspec)
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      character*(ionlen) ion(maxnatom),level(maxnatom)
      character*(ionlen) spec_ion(maxnspec),spec_lev(maxnspec)
      character*(namelen) atdatnm
      logical splotrc
      logical hard
      common /orispec/spec_ori,npnts_ori
      common /sptrans/spunit,spchar,spzero
      common /hist/hist
      common /plot/plot
      common /pcol/pcol
      common /label/label,anot,anotpos
      common /model/model
      common /splotrc/splotrc

 1    call PGSCI(2)
      call PGBAND(7,0,0.0,0.0,xin,yin,pgchar)
      
      if (pgchar.eq.'q') then
         return
      elseif (pgchar.eq.' ') then
         call curspos(spec,panel,npnts(panel),dble(xin),dble(yin),
     :        pixnum,.true.)
         goto 1
      elseif (pgchar.eq.'a'.or.pgchar.eq.'A'.or.pgchar.eq.'w') then
         call resize(pgchar,xin,yin,xmin(panel),xmax(panel),
     :        ymin(panel),ymax(panel))
         call plotdat(spec,panel,npnts(panel),xmin(panel),xmax(panel),
     :        ymin(panel),ymax(panel),vplimits,panel,nhori,nvert,
     :        .true.)
         goto 1
      elseif (pgchar.eq.'c') then
         call pg_range(spec,panel,2,npnts(panel),xmin(panel),
     :        xmax(panel),ymin(panel),ymax(panel))
         call plotdat(spec,panel,npnts(panel),xmin(panel),
     :        xmax(panel),ymin(panel),ymax(panel),vplimits,panel,
     :        nhori,nvert,.true.)
         goto 1
      elseif (pgchar.eq.'m') then
         call getstats(spec,panel,npnts(panel),dble(xin),dble(yin))
         goto 1
      elseif (pgchar.eq.'p') then
         call cpparms(spec,panel,npnts(panel),xmin(panel),xmax(panel),
     :        ymin(panel),ymax(panel),panel,nhori,nvert)
         goto 1
      elseif (pgchar.eq.'s') then
         call wsplotrc(filenm,contnm,signm,nspec,xmin,xmax,
     :        ymin,ymax,plot,hist,pcol,model,label,anot,anotpos,
     :        spchar,spunit,spzero)
         goto 1
      elseif (pgchar.eq.'l') then
         call pg_label(panel,nhori,nvert,xmin(panel),xmax(panel),
     :        ymin(panel),ymax(panel))
         goto 1
      elseif (pgchar.eq.'x') then
         call spec_copy(spec_ori,panel,npnts_ori(panel),spec,panel,
     :        npnts(panel))
         call redisp(spec,panel,npnts(panel),xin,xmin(panel),
     :        xmax(panel),spchar(panel,1),spchar(panel,2),
     :        spunit(panel,1),spunit(panel,2),spzero(panel),.true.)
         call plotdat(spec,panel,npnts(panel),xmin(panel),
     :        xmax(panel),ymin(panel),ymax(panel),vplimits,panel,
     :        nhori,nvert,.true.)
         goto 1
      elseif (pgchar.eq.'r') then
         call ratomdat(atdatnm,ion,level,atmass,lambda0,f,gamma,
     :        natomdat)
         call register(ion,level,lambda0,natomdat,filenm,nspec,
     :        spec_ion,spec_lev,spec_lam0,spec_tind,panel,xin,zabs,
     :        xmin,xmax,ymin,ymax)
         do i=1,nspec
            call redisp(spec,i,npnts(i),spzero(i),xmin(i),
     :           xmax(i),spchar(i,1),spchar(i,2),
     :           spunit(i,1),spunit(i,2),dummyzero,.false.)
            call plotdat(spec,i,npnts(i),xmin(i),xmax(i),ymin(i),
     :           ymax(i),vplimits,i,nhori,nvert,.false.)
         enddo
         call pg_panl(panel,nhori,nvert,xpan,ypan)
         call PGPANL(xpan,ypan)
         goto 1
      elseif (pgchar.eq.'z') then
         z=zabs+(1.d0+zabs)*xin*1.d3/c
         WRITE(*,'(f9.7)') z
         goto 1
      elseif (pgchar.eq.'g') then
         call pg_panl(nspec,nhori,nvert,xpan,ypan)
         call PGPANL(xpan,ypan)
         call PGPAGE
         hard=.false.
         call gather(spec,npnts,nspec,panel,nhori,nvert,spec_ion,
     :        spec_lev,spec_lam0,xmin,xmax,ymin,ymax,hard)
         call PGSCI(2)
         call PGBAND(0,0,0.0,0.0,xin,yin,pgchar)
         if (pgchar.eq.'p') then
            hard=.true.
            call PGOPEN('?PGPLOT Device? (<cr>=def, "?" for list):')
            call gather(spec,npnts,nspec,panel,nhori,nvert,spec_ion,
     :           spec_lev,spec_lam0,xmin,xmax,ymin,ymax,hard)
            call PGCLOS
            call PGSLCT(1)
         endif
         call PGPAGE
         call PGERAS
         call PGSUBP(-nhori,nvert)
         do i=1,nspec
            call plotdat(spec,i,npnts(i),xmin(i),xmax(i),ymin(i),
     :           ymax(i),vplimits,i,nhori,nvert,.true.)
         enddo
         call pg_panl(panel,nhori,nvert,xpan,ypan)
         call PGPANL(xpan,ypan)
         goto 1
      else
         goto 1
      endif

      return
      end
