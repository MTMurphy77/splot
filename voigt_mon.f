c     MM 29/8/00 Function to calculate the Voigt function given the
c     damping parameter a and the distance-from-center parameter
c     u. Monaghan's (1971, MNRAS, 152, 509) algorithm is used and we
c     emply the same notation. The function W need only be calculated
c     once and is saved for future calls to this function. Given an
c     arbitrary value of u, W(u) is then found by interpolating the
c     already-calculated table of W. With the step size in a, da, being
c     0.001, and the number of interpolating points being 3, the tables
c     of Hummer (1965, MNRAS, 70, 1) are reproduced to 1 place in the
c     8th significant figure.
c
c     Requires: polint.f constants.h
c
c     USER MUST: call this routine once to initialise the W function. NO
c     SENSIBLE OUTPUT IS GIVEN (i.e. voigt_monaghan is set to -1).
c
      double precision function voigt_monaghan(a,u)
      implicit none
c     tolerance for a below which a is assumed to be 0.0, step spacing
c     for calculation of a
      double precision atol,nomda
      parameter (atol=1.d-9,nomda=1.d-3)
c     maxnumber of steps in a, maximum number of steps in u, number of
c     points to used for interpolating W for given value of u.
      integer nastep,nustep,nfit
      parameter (nastep=200000,nustep=3700000,nfit=3)
      double precision a,u,usq
      double precision wu,dwu,v(0:nastep-1),vd(5),an(0:nastep-1),fac
      SAVE du,un,w
      double precision du,da,un(0:nustep),w(0:nustep)
      SAVE nf
      integer i,nf,na
c to test whether we need to calculate W or not
      SAVE first
      logical first
      data first/.true./
c need this include file for pi and sqrtpi
      include 'constants_inc.f'

c Initialise W for this and future calls
      if (first) then
         first=.false.
         nf=nfit/2
c step size for u
         du=37.d0/dble(nustep)
c find W in range u=0.0 to 37.d0
         un(0)=0.d0
         un(1)=du
c find W(0) and W(du) from 5th order polynomial
         w(0)=2.d0
         w(1)=w(0)-3.d0*du*du+22.d0*du**4/24.d0
c find all W(u)
         fac=du**2/12.d0
         do i=2,nustep
            un(i)=dble(i)*du
            w(i)=(w(i-1)*(fac*10.d0*(un(i-1)**2-3.d0)+2.d0)+
     :           w(i-2)*(fac*(un(i-2)**2-3.d0)-1.d0))/
     :           (1.d0-fac*(un(i)**2-3.d0))
         enddo
         voigt_monaghan=-1.d0
         return
      endif

c speed saving definitions
      usq=u*u

c immediately recognise and return for a.eq.0.0
      if (a.lt.atol) then
         voigt_monaghan=dexp(-usq)
         return
      endif

c Voigt function symmetric
      if (u.lt.0.d0) u=-u

c overflow stuff      
      if (u.gt.37.d0) then
         if (a.gt.3.d-1) then
            WRITE(*,'(a)')'STOPPING: a and u are both large in voigt.f!'
            stop
         endif
c approx for large u but small a
         voigt_monaghan=a/sqrtpi/usq*(1.d0+1.5d0/usq+3.75d0/usq/usq-a*a/
     :        usq*(1.d0+5.d0/usq+26.25d0/usq/usq))
         return
      endif
      
c Find correct W(u) by interpolation
      i=dnint(u/du)
      if (i-nf.lt.0) then
         call polint(un(0),w(0),nfit,u,wu,dwu)
      elseif (i+nf.gt.nustep) then
         call polint(un(nustep-nfit+1),w(nustep-nfit+1),nfit,u,wu,dwu)
      else
         call polint(un(i-nf),w(i-nf),nfit,u,wu,dwu)
      endif

c     step size for a
      na=dint(a/nomda)
      if (na+2.gt.nastep) then
         WRITE(*,'(a,a)') 'STOPPING: Too many steps in a required ',
     :        'in voigt.f'
         stop
      endif
      na=na+1
      da=a/dble(na)
c     Calculate V(a)
      an(0)=0.d0
      an(1)=da

c     V(0)
      v(0)=dexp(-usq)/sqrtpi

c     V'(0) to V'''''(0)
      vd(1)=-wu/dexp(usq/2.d0)/pi
      vd(2)=-2.d0*usq*v(0)
      vd(3)=1.d0/pi-usq*vd(1)
      vd(4)=usq**2*v(0)
      vd(5)=-6.d0/pi-usq*vd(3)

c     V(da) from 5th order Taylor expansion
      v(1)=v(0)+vd(1)*da+vd(2)*da**2+vd(3)*da**3/1.5d0+
     :     vd(4)*da**4/1.5d0+vd(5)*da**5/30.d0

c     V(a)=V(na*da)
      fac=da**2/3.d0
      do i=2,na
         an(i)=dble(i)*da
         an(i)=an(i)*dexp(-an(i)**2)
         v(i)=(v(i-1)*(2.d0-fac*10.d0*usq)+fac/pi*(an(i)+
     :        10.d0*an(i-1)+an(i-2)))/(1.d0+fac*usq)-v(i-2)
      enddo

      voigt_monaghan=sqrtpi*v(na)/dexp(-a**2)

      return
      end

      subroutine voigt_monaghan_init


      return
      end

