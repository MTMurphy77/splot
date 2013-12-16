c File defining the array sizes for splot.f
      integer maxnspec
c The maximum number of chunks of spectrum
      parameter (maxnspec=16)

      integer maxpnts
c The maximum number of points in a single chunk of spectrum
      parameter (maxpnts=100000)

      integer maxspecdim
c The maximum number of columns of information associated with a spectrum
      parameter (maxspecdim=8)

      integer nmcharlength
c The character length of the filenames
      parameter (nmcharlength=64)

      integer maxnchunk
c The maximum number of chunks of data to be fitted. This should be
c larger than maxnspec but not too large.
      parameter (maxnchunk=20)

      integer maxlinpchunk
c The maximum number of lines to be fitted per chunk of data fitted
      parameter (maxlinpchunk=100)

      integer maxlinespec
c The maximum number of items which completely parametrize/specify
c a line in the Voigt profile generation routine voigtp
      parameter (maxlinespec=5)

      double precision tol_spsim
c The relative tolerance for the similarity between spectral parameters
c read from .dat, .cont.dat and .sig.dat. This should typically
c characterise the magnitude of the round off error inherient in the
c program which wrote the data files.
      parameter (tol_spsim=1.d-12)

      integer plot_flux,plot_cont,plot_sig,plot_nsig,plot_fit,
     :     plot_ofit,plot_zero
c Defaults for the what parts of the spectra are to be plotted
      parameter (plot_flux=1,plot_cont=1,plot_sig=1,
     :     plot_nsig=0,plot_fit=0,plot_ofit=0,plot_zero=1)

      integer pcol_flux,pcol_cont,pcol_sig,pcol_nsig,pcol_fit,
     :     pcol_ofit,pcol_zero
c Defaults for the way colours in which the data are plotted:
c 1=white,2=red,3=green,4=blue,5=cyan,
c 6=violet,7=yellow,8=orange,9=yellowgreen,10=cyangreen,
c 11=lightblue,12=purple,13=pink,14=grey,15=black
      parameter (pcol_flux=1,pcol_cont=5,pcol_sig=2,pcol_nsig=1,
     :     pcol_fit=3,pcol_ofit=4,pcol_zero=5)

      integer hist_flux,hist_cont,hist_sig,hist_nsig,hist_fit,
     :     hist_ofit,hist_zero
c Defaults for the way in which the data are plotted:
c 1=solid line,2=dashed,3=dash-dot,4=dotted,5=dash-dot-dot-dot,
c 6=histogram
      parameter (hist_flux=6,hist_cont=4,hist_sig=1,
     :     hist_nsig=1,hist_fit=1,hist_ofit=2,hist_zero=4)

c
c PGPLOT parameters
c
c View surface size
      real pg_width,pg_aspect
c Width and aspect ratio of PGPLOT window on screen in inches
      parameter (pg_width=13.0,pg_aspect=0.625)

c Viewport size control

      real vpdlimit
c Bottom of view port set
      parameter (vpdlimit=0.12)

      real vpulimit
c Top of view port set
      parameter (vpulimit=0.93)

      real vpllimit
c Left edge of view port set
      parameter (vpllimit=0.10)

      real vprlimit
c Right edge of view port set
      parameter (vprlimit=0.98)

c Intra-viewport control

      real dlimit
c Fraction of vertical range to be left free at bottom
      parameter (dlimit=0.08)

      real ulimit
c Fraction of vertical range to be left free at top
      parameter (ulimit=0.08)

      real llimit
c Fraction of vertical range to be left free to the left
      parameter (llimit=0.03)

      real rlimit
c Fraction of vertical range to be left free to the right
      parameter (rlimit=0.03)


c Label control

      character*(64) xlabel
c Default label for x-axis
      parameter (xlabel='Wavelength (Å)')

      character*(64) ylabel
c Default label for y-axis
      parameter (ylabel='Normalized Flux')

      character*(64) tlabel
c Default label for title
      parameter (tlabel='')

      real xdisp
c Displacement of the x-axis label from the x-axis in
c units of character height
      parameter (xdisp=2.2)

      real ydisp
c Displacement of the y-axis label from the y-axis in
c units of character height
      parameter (ydisp=2.5)

      real tdisp
c Displacement of the title from the top of the view port in
c units of character height
      parameter (tdisp=0.5)

      integer maxanot
c Maximum number of anotations allowed per plot/viewport
      parameter (maxanot=5)

      real anotsize
c Character height of anotations
      parameter (anotsize=2.0)
