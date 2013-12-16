c     MM 12/2/01 Subroutine to read in an atomic data file in the new
c     format (i.e. including the atomic mass).
      subroutine ratomdat(atdatnm,ion,level,atmass,lambda0,f,gamma,
     :     ntrans)
      implicit none
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      double precision lambda0(maxnatom),f(maxnatom),gamma(maxnatom)
      double precision atmass(maxnatom)
      integer ntrans
      integer i,sindex,eindex
      character*(ionlen) ion(maxnatom),level(maxnatom)
      character*(namelen) atdatnm
      character*(namelen) atomdr,zerochar
      character*(longchar) charline,sepline(maxatomwords)

      do i=1,namelen
         zerochar(i:i)=' '
      enddo
      
      atomdr=zerochar
      call getenv('SPLOT_ATOM_DIR',atomdr)
      sindex=0
      if (atomdr(1:1).ne.' ') then
         call getlen(atomdr,sindex,eindex)
         OPEN(unit=1,file=atomdr(sindex:eindex)//'/atom.dat',
     :        status='old')
         atdatnm=atomdr(sindex:eindex)//'/atom.dat'
         eindex=eindex-sindex+9
         sindex=1
         if (sindex.eq.0) then
 1          call rchar(atdatnm,'Atomic data file?','atom.dat')
            call getlen(atdatnm,sindex,eindex)
            OPEN(unit=1,file=atdatnm(sindex:eindex),status='old',err=1)
         endif
      else
 99      call rchar(atdatnm,'Atomic data file?','atom.dat')
         call getlen(atdatnm,sindex,eindex)
         OPEN(unit=1,file=atdatnm(sindex:eindex),status='old',err=99)
      endif

      i=0
 2    i=i+1
      READ(1,'(a)',err=3,end=4) charline
      call sepchar(charline,sepline)
      ion(i)=zerochar(1:ionlen)
      read(sepline(1),'(a)') ion(i)
      level(i)=zerochar(1:ionlen)
      read(sepline(2),'(a)') level(i)
      read(sepline(3),*) atmass(i)
      read(sepline(4),*) lambda0(i)
      read(sepline(5),*) f(i)
      read(sepline(6),*) gamma(i)
      goto 2

 3    close(1)
      WRITE(*,'(a,i3,a,a)') 'STOPPING: Error reading line ',i,
     :     ' of ',atdatnm(sindex:eindex)
      stop

 4    close(1)
      ntrans=i-1

      return
      end
