c     MM 29/8/00 Subroutine to do an n-point polynomial interpolation on
c     an array of length n. If the calling program enters arrays longer
c     than n then offsets can be used. Taken from Numerical Recipes and
c     made into double precision.
      subroutine polint(xa,ya,n,x,y,dy)
      implicit none
      integer n,nmax
      parameter (nmax=10)
      double precision dy,x,y,xa(n),ya(n)
      double precision den,dif,dift,ho,hp,w,c(nmax),d(nmax)
      integer i,m,ns

      ns=1
      dif=dabs(x-xa(1))
      do i=1,n
         dift=dabs(x-xa(i))
         if (dift.lt.dif) then
            ns=i
            dif=dift
         endif
         c(i)=ya(i)
         d(i)=ya(i)
      enddo
      y=ya(ns)
      ns=ns-1
      do m=1,n-1
         do i=1,n-m
            ho=xa(i)-x
            hp=xa(i+m)-x
            w=c(i+1)-d(i)
            den=ho-hp
            if (den.eq.0) then
               WRITE(*,'(a)') 'STOPPING: XAs are identical in polint!'
               stop
            endif
            den=w/den
            d(i)=hp*den
            c(i)=ho*den
         enddo
         if (2*ns.lt.n-m) then
            dy=c(ns+1)
         else
            dy=d(ns)
            ns=ns-1
         endif
         y=y+dy
      enddo

      return
      end
