c     MM 13/2/01 Subroutine to register all plots to the same absorption
c     redshift by finding their transition details from their filenames.
      subroutine register(ion,level,lambda0,natomdat,filenm,nspec,
     :     spec_ion,spec_lev,spec_lam0,spec_tind,panel,xin,zabs,
     :     xmin,xmax,ymin,ymax)
      implicit none
      include 'splot_inc.f'
      include 'ratomdat_inc.f'
      include 'charlen_inc.f'
      include 'constants_inc.f'
      double precision lambda0(maxnatom),spec_lam0(maxnspec)
      double precision zabs
      double precision spunit(maxnspec,2)
      real xmin(maxnspec),xmax(maxnspec)
      real ymin(maxnspec),ymax(maxnspec)
      real vplimits(maxnspec,4),spzero(maxnspec)
      real xin,anotpos(maxnspec,maxanot,2)
      integer natomdat,spec_tind(maxnspec),nspec,panel
      integer i,length
      character*1 spchar(maxnspec,2)
      character*(ionlen) ion(maxnatom),level(maxnatom)
      character*(ionlen) spec_ion(maxnspec),spec_lev(maxnspec)
      character*(namelen) filenm(maxnspec)
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      character*(namelen) zerochar,zabs_char
      common /sptrans/spunit,spchar,spzero
      common /label/label,anot,anotpos

      do i=1,namelen
         zerochar(i:i)=' '
      enddo

      do i=1,nspec
         call nametrans(ion,level,lambda0,natomdat,filenm(i),
     :        spec_tind(i))
         spec_ion(i)=ion(spec_tind(i))
         spec_lev(i)=level(spec_tind(i))
         spec_lam0(i)=lambda0(spec_tind(i))
      enddo
      zabs=dble(xin)/spec_lam0(panel)-1.d0

      do i=1,nspec
         if (i.ne.panel) then
            xmin(i)=real(spec_lam0(i)*dble(xmin(panel))/
     :           spec_lam0(panel))
            xmax(i)=real(spec_lam0(i)*dble(xmax(panel))/
     :           spec_lam0(panel))
            spzero(i)=real((1.d0+zabs)*spec_lam0(i))
            xmin(i)=real(c/1.d3)*(xmin(i)-spzero(i))/spzero(i)
            xmax(i)=real(c/1.d3)*(xmax(i)-spzero(i))/spzero(i)
            ymin(i)=ymin(panel)
            ymax(i)=ymax(panel)
         endif
         anotpos(i,5,1)=0.015
         anotpos(i,5,2)=0.200
         anot(i,5)=zerochar
         call transname(spec_ion(i),spec_lev(i),spec_lam0(i),0.0,
     :        anot(i,5),length,0)
         call rounddble(zabs,8,zabs_char,length)
         label(i,1)='Velocity (km/s) [z='//zabs_char(1:length)//']'
         spchar(i,1)='w'
         spchar(i,2)='v'
         spunit(i,1)=1.d-10
         spunit(i,2)=1.d3
      enddo
      spzero(panel)=real((1.d0+zabs)*spec_lam0(panel))
      xmin(panel)=real(c/1.d3)*(xmin(panel)-spzero(panel))/
     :     spzero(panel)
      xmax(panel)=real(c/1.d3)*(xmax(panel)-spzero(panel))/
     :     spzero(panel)

      return
      end
