      subroutine voigtp(wlen,flux,npnts,lambda0,b,N,f,gamma,vsig,h,
     :     linear,convopt)
      implicit none
      include 'constants_inc'
c     h is the fine binning scale.
      integer h
      integer npnts,h
      double precision wlen(npnts),flux(npnts)
      double precision wlen_h(npnts*h),flux_h(npnts*h)
      double precision lambda0,b,N,f,gamma,vsig
      double precision dlam,h_dble,dlam_h
      integer i,j
c     linear is true if the wavelength scale in wlen is linear. If not,
c     then more detailed calculation is done. This takes more
c     computational time.
      logical linear
c     convopt is an option for the convolution. If convopt is true then
c     convolution will occur before the spectrum is rebinned to the
c     original course scale. This is preferred but requires more memory
c     space and more computational time.
      logical convopt
      parameter (convopt=.true.)
      
      h_dble=dble(h)
      
c     First slice the spectrum up to the fine scale defined by h
      if (linear) then
         dlam=wlen(2)-wlen(1)
         dlam_h=dlam/h_dble
         do i=1,npnts
            do j=1,h
               if (j.eq.1) then
                  wlen_h(i*h)=wlen(i)-0.5d0*(dlam-dlam_h)
                  flux_h(i*h)=flux(i)
               else
                  wlen_h(i*h+j-1)=wlen_h(i*h+j-2)+dlam_h
                  flux_h(i*h+j-1)=flux(i)
               endif
            enddo
         enddo
      endif

c     Now rebin the fine sprectrum back up to the source slicing.
      if (linear) then
         do i=1,npnts
            flux(i)=0.d0
            do j=1,h
               flux(i)=flux(i)+flux_h(i*h+j-1)
            enddo
            flux(i)=flux(i)/h_dble
         enddo
      endif

      return
      end
