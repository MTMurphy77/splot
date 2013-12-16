c     MM 27/11/00 Subroutine to find the spectral parameter and flux,
c     contin and error nearest the cursor position
      subroutine curspos(spec,nspec,npnts,x,y,pixnum,output)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision x,y
      integer model(maxnspec)
      integer npnts,nspec,pixnum
      logical output
      common /model/model

      do pixnum=1,npnts
         if (spec(nspec,1,pixnum).gt.x) then
            goto 1
         endif
      enddo
      pixnum=pixnum-1
      
 1    if ((spec(nspec,1,pixnum)-x.gt.x-spec(nspec,1,pixnum-1))
     :     .and.(pixnum.gt.1)) pixnum=pixnum-1

      if (output) then
         WRITE(*,*) 'x=',real(x),' y=',real(y),' p=',pixnum
         WRITE(*,*) 's=',real(spec(nspec,1,pixnum)),' f=',
     :        real(spec(nspec,2,pixnum)),' c=',
     :        real(spec(nspec,3,pixnum)),' e=',
     :        real(spec(nspec,4,pixnum))
         if (model(nspec).eq.1) WRITE(*,*) 'm=',
     :        real(spec(nspec,6,pixnum)),' res=',
     :        real(spec(nspec,2,pixnum)-spec(nspec,6,pixnum))
         WRITE(*,'(a)') ' '
      endif

      return
      end
