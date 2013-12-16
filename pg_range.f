c     MM 27/11/00 Subroutine to find the plotting range for the first
c     and one extra channel from a spectrum matrix
      subroutine pg_range(spec,nspec,nchannel,npnts,xmin,xmax,
     :     ymin,ymax)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision spec_min,spec_max
      integer npnts
      real xmin,xmax,ymin,ymax,range
      integer nspec,nchannel
      integer plot(maxnspec,maxspecdim)
      common /plot/plot
      
      xmin=real(spec_min(spec,nspec,1,npnts))
      xmax=real(spec_max(spec,nspec,1,npnts))
      range=xmax-xmin
      xmin=xmin-llimit*range
      xmax=xmax+rlimit*range
      if (plot(nspec,4).eq.1) then
         ymin=0.0
      else
         ymin=real(spec_min(spec,nspec,nchannel,npnts))
      endif
      ymax=real(spec_max(spec,nspec,nchannel,npnts))
      range=ymax-ymin
      ymin=ymin-dlimit*range
      ymax=ymax+ulimit*range

      return
      end
