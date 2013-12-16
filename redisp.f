c     MM 5/1/01 Subroutine to redisperse a spectrum to a different scale
c     (e.g. from observed wavelength the frequency etc.)
      subroutine redisp(spec,panel,npnts,xin,xmin,xmax,spchar_o,
     :     spchar_n,spunit_o,spunit_n,spzero,outflag)
      implicit none
      include 'splot_inc.f'
      include 'constants_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts)
      double precision spunit_o,spunit_n,spunit_c,dumspec(maxpnts)
      real xin,xmin,xmax,temp,spzero
      integer npnts,panel,i,j
      character*1 spchar_o,spchar_n,spchar_c,spchar
      logical outflag

      if (outflag) then
         if (spchar_o.eq.' ') then
 1          call rchar(spchar,
     :            'Current spectral parameter? (w,f,v,q)','w')
            if (spchar.ne.'w'.and.spchar.ne.'f'.and.
     :           spchar.ne.'v'.and.spchar.ne.'q') goto 1
            if (spchar.eq.'q') return
            spchar_o=spchar
            if (spchar_o.eq.'w') then
               call rdble(spunit_o,
     :              'Wavelength unit in units of m?',
     :              '1.d-10')
            elseif (spchar_o.eq.'f') then
               call rdble(spunit_o,
     :              'Frequency unit in units of Hz?','1.d9')
            elseif (spchar_o.eq.'v') then
               call rdble(spunit_o,
     :              'Velocity unit in units of m/s?','1.d3')
            endif
         else
            if (spchar_o.eq.'w'.and.spchar_n.eq.'f') then
               xin=real(c/dble(xin)/spunit_o/spunit_n)
               temp=xmin
               xmin=real(c/dble(xmax)/spunit_o/spunit_n)
               xmax=real(c/dble(temp)/spunit_o/spunit_n)
            elseif (spchar_o.eq.'f'.and.spchar_n.eq.'w') then
               xin=real(c/dble(xin)/spunit_o/spunit_n)
               temp=xmin
               xmin=real(c/dble(xmax)/spunit_o/spunit_n)
               xmax=real(c/dble(temp)/spunit_o/spunit_n)
            elseif (spchar_o.eq.'w'.and.spchar_n.eq.'v') then
               xin=spzero*xin*spunit_n/real(c)+spzero
               xmin=spzero*xmin*spunit_n/real(c)+spzero
               xmax=spzero*xmax*spunit_n/real(c)+spzero
            elseif (spchar_o.eq.'f'.and.spchar_n.eq.'v') then
               xin=spzero*xin*spunit_n/real(c)+spzero
               temp=xmin
               xmin=spzero*xmax*spunit_n/real(c)+spzero
               xmax=spzero*xmin*spunit_n/real(c)+spzero
            endif
            spzero=0.0
         endif

 2       call rchar(spchar_n,
     :        'New spectral parameter? (w,f,v)','v')
         if (spchar_n.ne.'w'.and.spchar_n.ne.'f'.and.
     :        spchar_n.ne.'v') goto 2
         if (spchar_n.eq.'w') then
            call rdble(spunit_n,
     :           'Wavelength unit in units of m?',
     :           '1.d-10')
         elseif (spchar_n.eq.'f') then
            call rdble(spunit_n,
     :           'Frequency unit in units of Hz?','1.d9')
         elseif (spchar_n.eq.'v') then
            call rdble(spunit_n,
     :           'Velocity unit in units of m/s?','1.d3')
         endif
      endif

      if (spchar_o.eq.'w'.and.spchar_n.eq.'f') then
         do i=1,npnts
            dumspec(i)=spec(panel,1,i)
         enddo
         do i=1,npnts
            spec(panel,1,i)=c/dumspec(npnts-i+1)/spunit_o/spunit_n
         enddo
         if (outflag) then
            temp=xmax
            xmax=real(c/dble(xmin)/spunit_o/spunit_n)
            xmin=real(c/dble(temp)/spunit_o/spunit_n)
         endif
         do j=2,maxspecdim
            do i=1,npnts
               dumspec(i)=spec(panel,j,i)
            enddo
            do i=1,npnts
               spec(panel,j,i)=dumspec(npnts-i+1)
            enddo
         enddo
      elseif (spchar_o.eq.'f'.and.spchar_n.eq.'w') then
         do i=1,npnts
            dumspec(i)=spec(panel,1,i)
         enddo
         do i=1,npnts
            spec(panel,1,i)=c/dumspec(npnts-i+1)/spunit_o/spunit_n
         enddo
         if (outflag) then
            temp=xmax
            xmax=real(c/dble(xmin)/spunit_o/spunit_n)
            xmin=real(c/dble(temp)/spunit_o/spunit_n)
         endif
         do j=2,maxspecdim
            do i=1,npnts
               dumspec(i)=spec(panel,j,i)
            enddo
            do i=1,npnts
               spec(panel,j,i)=dumspec(npnts-i+1)
            enddo
         enddo
      elseif (spchar_o.eq.'w'.and.spchar_n.eq.'v') then
         spzero=xin
         do i=1,npnts
            spec(panel,1,i)=c*(spec(panel,1,i)-dble(xin))/
     :           spec(panel,1,i)/spunit_n
         enddo
         if (outflag) then
            xmin=real(c)*(xmin-xin)/xin/spunit_n
            xmax=real(c)*(xmax-xin)/xin/spunit_n
         endif
      elseif (spchar_o.eq.'f'.and.spchar_n.eq.'v') then
         spzero=xin
         do i=1,npnts
            dumspec(i)=spec(panel,1,i)
         enddo
         do i=1,npnts
            spec(panel,1,i)=c*(dumspec(npnts-i+1)-dble(xin))/
     :           dumspec(npnts-i+1)/spunit_n
         enddo
         if (outflag) then
            temp=xmax
            xmax=real(c)*(xmin-xin)/xin/spunit_n
            xmin=real(c)*(temp-xin)/xin/spunit_n
         endif
         do j=2,maxspecdim
            do i=1,npnts
               dumspec(i)=spec(panel,j,i)
            enddo
            do i=1,npnts
               spec(panel,j,i)=dumspec(npnts-i+1)
            enddo
         enddo
      elseif (spchar_o.eq.spchar_n) then
         do i=1,npnts
            spec(panel,1,i)=spunit_o*spec(panel,1,i)/spunit_n
         enddo
         xmin=real(spunit_o*dble(xmin)/spunit_n)
         xmax=real(spunit_o*dble(xmax)/spunit_n)
      endif

      return
      end
