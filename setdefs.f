c     MM 28/11/00 Subroutine to set the hist matrix from user defaults.
c     This holds integers which specify the way in which the data,
c     continuum, error and normalised error (when a fit model is
c     present) is plotted.  1=solid line, 2=dashed, 3=dot-dash,
c     4=dotted, 5=dash-dot-dot-dot, 6=histogram
      subroutine setdefs(plot,hist,pcol,label,anot,anotpos,spchar,
     :     spunit,spzero)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      double precision spunit(maxnspec,2)
      real anotpos(maxnspec,maxanot,2),spzero(maxnspec)
      integer hist(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer plot(maxnspec,maxspecdim)
      integer i,j
      character*1 spchar(maxnspec,2)
      character*(nmcharlength) label(maxnspec,3)
      character*(nmcharlength) anot(maxnspec,maxanot)
      SAVE zerochar
      character*(nmcharlength) zerochar
      SAVE first
      logical first
      data first/.true./
      
      if (first) then
         first=.false.
         do i=1,nmcharlength
            zerochar(i:i)=' '
         enddo
      endif
      
      do i=1,maxnspec
         hist(i,2)=hist_flux
         hist(i,3)=hist_cont
         hist(i,4)=hist_sig
         hist(i,5)=hist_nsig
         hist(i,6)=hist_fit
         hist(i,7)=hist_ofit
         hist(i,8)=hist_zero
         plot(i,2)=plot_flux
         plot(i,3)=plot_cont
         plot(i,4)=plot_sig
         plot(i,5)=plot_nsig
         plot(i,6)=plot_fit
         plot(i,7)=plot_ofit
         plot(i,8)=plot_zero
         pcol(i,2)=pcol_flux
         pcol(i,3)=pcol_cont
         pcol(i,4)=pcol_sig
         pcol(i,5)=pcol_nsig
         pcol(i,6)=pcol_fit
         pcol(i,7)=pcol_ofit
         pcol(i,8)=pcol_zero
         label(i,1)=zerochar
         label(i,1)=xlabel
         label(i,2)=zerochar
         label(i,2)=ylabel
         label(i,3)=zerochar
         label(i,3)=tlabel
         spchar(i,1)=' '
         spchar(i,2)=' '
         spunit(i,1)=1.d0
         spunit(i,1)=1.d0
         spzero(i)=0.d0
         do j=1,maxanot
            anot(i,j)=zerochar
            anotpos(i,j,1)=0.0
            anotpos(i,j,2)=0.0
         enddo
      enddo

      return
      end
