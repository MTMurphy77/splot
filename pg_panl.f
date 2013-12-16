c     MM 5/12/00 Subroutine to find the x and y indices corresponding to
c     a specific panel
      subroutine pg_panl(panel,nhori,nvert,xpan,ypan)
      implicit none
      integer panel,nhori,nvert,xpan,ypan

      if (nhori.eq.1.and.nvert.eq.1) then
         xpan=1
         ypan=1
      else
         xpan=(panel-1)/nvert+1
         ypan=panel-(xpan-1)*nvert
      endif

      return
      end
