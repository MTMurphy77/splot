      subroutine spec_stats(spec,nspec,spix,epix,mx,wmx,ewmx,rms,
     :     mex,rmsex,chisqc,chisqwm,chisqm)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision mx,mex,wmx,ewmx,rms,rmsex,sumxonex,sum1onex
      double precision chisqc,chisqwm,chisqm
      integer model(maxnspec)
      integer i,nspec,spix,epix
      common /model/model

      mx=0.d0
      mex=0.d0
      ewmx=0.d0
      rms=0.d0
      rmsex=0.d0
      sumxonex=0.d0
      sum1onex=0.d0
      chisqc=0.d0
      chisqwm=0.d0
      chisqm=0.d0
      
      do i=spix,epix
         sumxonex=sumxonex+spec(nspec,2,i)/spec(nspec,4,i)**2
         sum1onex=sum1onex+1.d0/spec(nspec,4,i)**2
         mx=mx+spec(nspec,2,i)/dble(epix-spix+1)
         mex=mex+spec(nspec,4,i)/dble(epix-spix+1)
      enddo

      wmx=sumxonex/sum1onex
      ewmx=1.d0/dsqrt(sum1onex)
      do i=spix,epix
         rms=rms+(spec(nspec,2,i)-mx)**2/dble(epix-spix+1)
         rmsex=rmsex+(spec(nspec,4,i)-mex)**2/dble(epix-spix+1)
         chisqc=chisqc+(spec(nspec,2,i)-spec(nspec,3,i))**2/
     :        spec(nspec,4,i)**2
         chisqwm=chisqwm+(spec(nspec,2,i)-wmx)**2/
     :        spec(nspec,4,i)**2
         if (model(nspec).eq.1) chisqm=chisqm+(spec(nspec,2,i)-
     :        spec(nspec,6,i))**2/spec(nspec,4,i)**2
      enddo
      rms=dsqrt(rms)
      rmsex=dsqrt(rmsex)

      return
      end
