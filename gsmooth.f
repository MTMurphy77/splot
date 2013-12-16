c     MM 19/06/12 [Quick and dirty] subroutine to Gaussian smooth an
c     array. The Gaussian kernel (sigma) width is specified in
c     pixels. No integration of the Gaussian across pixels is done; only
c     the value of the Gaussian kernal at the middle of the pixel is
c     used. Smoothing is done by brute force calculation, not Fourier
c     methods, out to at-least N times the Gaussian kernal (sigma) width
c     on either side.
      subroutine gsmooth(dat,ndat,gwidth)
      implicit none
      integer ndat
      double precision dat(ndat),tempdat(ndat),gwidth
      double precision gwidth2,g,sum,gsum,N
      parameter (N=5.0)
      integer i,j,js,je

c     Save square of Gaussian width once for speed
      gwidth2=gwidth*gwidth

c     Loop over pixels to do the smoothing
      do i=1,ndat
         sum=0.0
         gsum=0.0
         js=max(1,i-nint(N*gwidth/2.0)-1)
         je=min(ndat,i+nint(N*gwidth/2.0)+1)
         do j=js,je
            g=dexp(-0.5*(dble(i-j))**2/gwidth2)
            gsum=gsum+g
            sum=sum+dat(j)*g
         enddo
         tempdat(i)=sum/gsum
      enddo

c     Loop over pixels to replace the input array
      do i=1,ndat
         dat(i)=tempdat(i)
      enddo

      return
      end
