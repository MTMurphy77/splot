c     MM 29/8/00 This is the Voigt function as given by Harris
c     (D. L. Harris III, 1948, ApJ, 108, 112). VPFIT and SPABS use this
c     routine but without the h3 and h4 co-efficients in the power
c     series expansion in terms of a. They also use tabulated values of
c     h0 and h2 even though they are analytic functions - the effect on
c     speed is minimal since the exponential function need only be
c     called once. The co-efficients have been calculated for h1 and h3
c     by integrating Harris' F and can be included as an include file in
c     future.
c
c     USER MUST: Call this function once to initialise the h_1 and h_3
c     arrays. NO SENSIBLE OUTPUT IS GIVEN on this call
c     (i.e. voigt_harris1=-1.d0)
c
      double precision function voigt_harris3(a,u)
      implicit none
      double precision interr,vmax,atol
      parameter (interr=1.d-10,vmax=20.d0,atol=1.d-10)
      integer nustep
      parameter (nustep=20000)
      double precision a,asq,u,u0,u1,u2,usq,v,vsq
      double precision h0,F
      double precision expxsq
      SAVE h_0,h_1,h_2,h_3,h_4
      double precision h_0(nustep+1),h_2(nustep+1),h_4(nustep+1)
      double precision h_1(nustep+1),h_3(nustep+1)
      SAVE dv
      double precision dv
      integer i,n,n1
      SAVE first
      logical first
      data first/.true./
      external expxsq
      include 'constants_inc.f'

c     Initialization: The first time the routine is called, calculate
c     the F array and find from it the values of the first 5 h
c     co-efficients
      if (first) then
         first=.false.
         dv=vmax/dble(nustep)
         do i=1,nustep+1
            v=dble(i-1)*dv
            vsq=v*v
            call integrate(expxsq,0.d0,v,interr,F)
            h_0(i)=dexp(-vsq)
            F=F*h_0(i)
            h_1(i)=-2.d0/sqrtpi*(1.d0-2.d0*v*F)
            h_2(i)=(1.d0-2.d0*vsq)*h_0(i)
            h_3(i)=-4.d0/sqrtpi*((1.d0-vsq)/3.d0-
     :           v*(1.d0-2.d0/3.d0*vsq)*F)
            h_4(i)=2.d0*h0*(0.25d0-vsq+vsq*vsq/3.d0)
         enddo
         voigt_harris3=-1.d0
      endif

c     Now use a linear interpolation between values of the h
c     coefficients to obtain the Voigt function.
      usq=u*u
      asq=a*a
      if (u.lt.0.d0) u=-u
      
c     overflow stuff      
      if (u.gt.vmax) then
         if (a.gt.3.d-1) then
            WRITE(*,'(a)')'STOPPING: a and u are both large in voigt.f!'
            stop
         endif
c     approx for large u but small a
         voigt_harris3=a/sqrtpi/usq*(1.d0+1.5d0/usq+3.75d0/usq/usq-asq/
     :        usq*(1.d0+5.d0/usq+26.25d0/usq/usq))
         return
      endif

      u0=u/dv
      n=dint(u0)
      u1=dble(n)
      n=n+1
      u2=u0-u1
      n1=n+1
      voigt_harris3=h_0(n)+u2*(h_0(n1)-h_0(n))+a*(h_1(n)+
     :     u2*(h_1(n1)-h_1(n)))+asq*(h_2(n)+u2*(h_2(n1)-h_2(n)))+
     :     a*asq*(h_3(n)+u2*(h_3(n1)-h_3(n)))+asq*asq*(h_4(n)+
     :     u2*(h_4(n)-h_4(n1)))

      return
      end
