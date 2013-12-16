c MM 30/11/00 Subroutine to set the view port limits for each spectrum
      subroutine setvplims(vplimits,nspec,nx,ny)
      implicit none
      include 'splot_inc.f'
      real vplimits(maxnspec,4),vpwidth,vpheight
      integer nspec,i,nx,ny,xpan,ypan

      nx=1
      if (nspec.gt.3) nx=2
      ny=nspec/nx+mod(nspec,nx)
      if (nspec.gt.8) then
         nx=3
         ny=3
         if (nspec.gt.9) ny=4
         if (nspec.gt.12) nx=4
      endif
      do i=1,nspec
         vplimits(i,1)=vpllimit
         vplimits(i,2)=vprlimit
         vplimits(i,3)=vpdlimit
         vplimits(i,4)=vpulimit
      enddo
         
      return
      end
