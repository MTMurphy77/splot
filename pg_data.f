c     MM 27/11/00 Subroutine to make real*4 arrays for PGPLOT from
c     double precision arrays.
      subroutine pg_data(spec,nspec,nchannel,npnts,x,y)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      integer npnts
      real x(npnts),y(npnts)
      integer i,nspec,nchannel
      
      if (nchannel.eq.8) then
         do i=1,npnts
            x(i)=real(spec(nspec,1,i))
            y(i)=0.0
         enddo
      else
         do i=1,npnts
            x(i)=real(spec(nspec,1,i))
            y(i)=real(spec(nspec,nchannel,i))
         enddo
      endif

      return
      end
