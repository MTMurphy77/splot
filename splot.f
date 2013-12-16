c     MM 27/11/00 SPLOT: Program to display spectra with corresponding
c     sigma arrays and continuua. Many spectra can be plotted together
c     and various operations can be carried out on them.
c     
c     Compile using: make (see Makefile)
c
      program splot
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
c     The spec variable contains all spectral information for each
c     chunk of spectrum. The structure is: chunk number, spectral
c     dimension (1=wavelength;2=flux;3=continuum;4=sigma), pixel number
      double precision spec_ori(maxnspec,maxspecdim,maxpnts)
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision spunit(maxnspec,2)
      double precision lambda0(maxnatom),f(maxnatom),gamma(maxnatom)
      double precision atmass(maxnatom)
      double precision gsmooth_width,spec_dummy(maxpnts)
      real xmin(maxnspec),xmax(maxnspec)
      real ymin(maxnspec),ymax(maxnspec)
      real vplimits(maxnspec,4),anotpos(maxnspec,maxanot,2)
      real spzero(maxnspec)
      integer npnts_ori(maxnspec),npnts(maxnspec),nspec
      integer hist(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer plot(maxnspec,maxspecdim),model(maxnspec)
      integer nvert,nhori,panel,ntrans,length
      integer i,j
      character*1 spchar(maxnspec,2)
      character*(namelen) filenm(maxnspec),contnm(maxnspec)
      character*(namelen) signm(maxnspec)
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      character*(ionlen) ion(maxnatom),level(maxnatom)
      character*(namelen) atdatnm
      logical splotrc,err
      common /orispec/spec_ori,npnts_ori
      common /sptrans/spunit,spchar,spzero
      common /hist/hist
      common /plot/plot
      common /pcol/pcol
      common /model/model
      common /label/label,anot,anotpos
      common /splotrc/splotrc

      WRITE(*,'(a)') 'SPLOT: Plot spectra'
      WRITE(*,'(a)') 'Last modified: 16/2/01 by Michael Murphy'
      WRITE(*,'(a)') ' '

      nspec=0
      call setdefs(plot,hist,pcol,label,anot,anotpos,spchar,
     :     spunit,spzero)
      call rsplotrc(filenm,contnm,signm,nspec,xmin,xmax,
     :     ymin,ymax,plot,hist,pcol,model,label,anot,anotpos,
     :     spchar,spunit,spzero)
      if (splotrc) then
         call rprevspec(filenm,contnm,signm,nspec,spec_ori,npnts_ori)
      else
         nspec=1
         model(nspec)=0
 1       call rspec(filenm(nspec),contnm(nspec),signm(nspec),
     :        nspec,spec_ori,npnts_ori(nspec),.true.,err)
         if (err) then
            err=.false.
            goto 1
         endif
         if (filenm(nspec)(1:1).eq.'*') then
            nspec=0
            call ratomdat(atdatnm,ion,level,atmass,lambda0,f,gamma,
     :           ntrans)
            do i=1,ntrans
               nspec=nspec+1
               model(nspec)=0
               call transname(ion(i),level(i),lambda0(i),0.0,
     :              filenm(nspec),length,0)
               filenm(nspec)=filenm(nspec)(1:length)//'.dat'
               call rspec(filenm(nspec),contnm(nspec),signm(nspec),
     :              nspec,spec_ori,npnts_ori(nspec),.false.,err)
               if (err) nspec=nspec-1
            enddo
         endif
         
      endif
c     TEMPORARY: Smooth the spectra
      gsmooth_width=1.5
      do i=1,nspec
         do j=1,npnts_ori(i)
            spec_dummy(j)=spec_ori(i,2,j)
         enddo
         call gsmooth(spec_dummy,npnts_ori(i),gsmooth_width)
         do j=1,npnts_ori(i)
            spec_ori(i,2,j)=spec_dummy(j)
         enddo
      enddo

      do i=1,nspec
         call spec_copy(spec_ori,i,npnts_ori(i),spec,i,
     :        npnts(i))
      enddo
      call setvplims(vplimits,nspec,nhori,nvert)

c     Transform the x-axes of the spectra and scale them accordingly
      if (splotrc) then
         do i=1,nspec
            if (spchar(i,2).ne.' ')
     :           call redisp(spec,i,npnts(i),spzero(i),xmin(i),
     :           xmax(i),spchar(i,1),spchar(i,2),spunit(i,1),
     :           spunit(i,2),spzero(i),.false.)
         enddo
      endif

c     Begin the PGPLOT device
      call PGOPEN('?PGPLOT Device? (<cr>=def, "?" for list):')
      call PGSUBP(-nhori,nvert)
      call PGPAP(pg_width,pg_aspect)
      call PGSCR(0,0.184,0.31,0.31)
      call PGSCR(15,0.0,0.0,0.0)
      call PGASK(.false.)

      do i=1,nspec
         if (.not.splotrc) call pg_range(spec,i,2,npnts(i),
     :        xmin(i),xmax(i),ymin(i),ymax(i))
         call plotdat(spec,i,npnts(i),xmin(i),xmax(i),ymin(i),ymax(i),
     :        vplimits,i,nhori,nvert,.true.)
      enddo
      
 10   call selectplot(spec_ori,npnts_ori,spec,npnts,filenm,
     :     contnm,signm,nspec,xmin,xmax,ymin,ymax,vplimits,panel,
     :     nhori,nvert)

      call main(spec,nspec,npnts,filenm,contnm,signm,xmin,xmax,ymin,
     :     ymax,vplimits,panel,nhori,nvert)
      goto 10

      stop
      end
