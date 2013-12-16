c MM 28/11/00 Subroutine to get statistics of selected spectral chunk
      subroutine getstats(spec,nspec,npnts,xin,yin)
      implicit none
      include 'splot_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision xin,yin,mx,wmx,ewmx,rms,mex,rmsex
      double precision chisqc,chisqwm,chisqm
      real rxin,ryin
      integer model(maxnspec)
      integer nspec,npnts,spix,epix
      character*1 pgchar
      common /model/model

      call curspos(spec,nspec,npnts,xin,yin,spix,.false.)
 1    call PGBAND(6,0,0.0,0.0,rxin,ryin,pgchar)
      if (pgchar.ne.'m') goto 1
      call curspos(spec,nspec,npnts,dble(rxin),dble(ryin),
     :     epix,.false.)

      call spec_stats(spec,nspec,spix,epix,mx,wmx,ewmx,rms,
     :     mex,rmsex,chisqc,chisqwm,chisqm)

      WRITE(*,*) 'm=',mx,' em=',rms/dsqrt(dble(epix-spix+1)),
     :     ' rms=',rms,' n=',epix-spix+1
      WRITE(*,*) 'wm=',wmx,' ewm=',ewmx,' me=',mex,' rmse=',rmsex
      if (model(nspec).eq.1) then
         WRITE(*,*) 'chisq_c=',chisqc,' chisq_wm=',chisqwm,
     :        ' chisqm=',chisqm
      else
         WRITE(*,*) 'chisq_c=',chisqc,' chisq_wm=',chisqwm
      endif
      WRITE(*,'(a)') ' '

      return
      end
