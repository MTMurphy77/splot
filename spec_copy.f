c MM 27/11/00 Subroutine to copy a spectrum to another spectrum matrix.
      subroutine spec_copy(spec_ori,nspec_ori,npnts_ori,
     :     spec_new,nspec_new,npnts_new)
      implicit none
      include 'splot_inc.f'
      double precision spec_ori(maxnspec,maxspecdim,maxpnts)
      double precision spec_new(maxnspec,maxspecdim,maxpnts)
      integer nspec_ori,nspec_new,npnts_ori,npnts_new
      integer i,j

      npnts_new=npnts_ori
      do i=1,npnts_new
         do j=1,maxspecdim
            spec_new(nspec_new,j,i)=spec_ori(nspec_ori,j,i)
         enddo
      enddo

      return
      end
