c     MM 27/11/00 Function to find the maximum value in a spectrum
c     matrix
      double precision function spec_max(spec,nspec,nchannel,npnts)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      integer npnts
      integer i,nspec,nchannel

      spec_max=spec(nspec,nchannel,1)
      do i=2,npnts
         if (spec(nspec,nchannel,i).gt.spec_max) spec_max=
     :        spec(nspec,nchannel,i)
      enddo

      return
      end
