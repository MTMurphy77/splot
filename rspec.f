c     MM 27/11/00 Subroutine to read in a spectrum in different data
c     formats. The supported formats are currently: (1) A single data
c     file containing (spectral parameter
c     (wavelength/frequency/velocity), flux, continuum, 1-sigma error or
c     (2) .dat file containing spectral parameter and flux, .cont file
c     containing spectral parameter and continuum, .sig file containing
c     spectral parameter and 1-sigma error.
      subroutine rspec(filename,contname,signame,nspec,spec,
     :     npnts,outflag,err)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts),dummy
      integer i,npnts,ncols,sindex,eindex,sindex2,eindex2,nspec
      character*(namelen) filename,contname,signame,zeroname
      character*120 charline(maxpnts),charline0
      logical err,outflag

c     A character string used to 'zero' other character strings
      do i=1,120
         charline0(i:i)=' '
      enddo
c     A character string used to 'zero' the filename strings
      do i=1,nmcharlength
         zeroname(i:i)=' '
      enddo

c     Get filename and find the name length
      if (outflag) call rchar(filename,'Data file name?',
     :     'HI1215.dat')
      call getlen(filename,sindex,eindex)
      err=.false.
      if (filename(1:1).eq.'*') return
      
      OPEN(unit=1,file=filename,status='old',err=1)
      goto 2

 1    if (outflag) WRITE(*,'(a)') 'WARNING: File does not exist!'
      err=.true.
      return

 2    i=0
 3    i=i+1
      charline(i)=charline0
      READ(1,'(a)',err=4,end=5) charline(i)
      goto 3
      
 4    if (outflag) WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :     filename(sindex:eindex),'. Character string error.'
      err=.true.
      return

 5    npnts=i-1
      close(1)

c     Find the number of columns in the file
      call getncols(charline,120,npnts,ncols,err)
      if (err) then
         if (outflag) WRITE(*,'(3(a))') 
     :        'WARNING: Error reading file ',filename(sindex:eindex),
     :        '. Column separation error'
         return
      endif
      if ((ncols.ne.4).and.(ncols.ne.2)) then
         if (outflag) WRITE(*,'(a,a,a,i2)') 
     :        'WARNING: Error reading file ',
     :        filename(sindex:eindex),
     :        '. Invalid number of colmuns = ',ncols
         err=.true.
         return
      endif

c     Read the data in if all information is in the one file
      if (ncols.eq.4) then
         do i=1,npnts
            read(charline(i),*,err=6) spec(nspec,1,i),spec(nspec,2,i),
     :           spec(nspec,3,i),spec(nspec,4,i)
         enddo
         goto 7
 6       if (outflag) WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :        filename(sindex:eindex),
     :        '. Double precision read error'
         err=.true.
         return
         
 7       contname=zeroname
         contname=filename(sindex:eindex)
         signame=zeroname
         signame=filename(sindex:eindex)

      else
c     Information is in several files so read in the spectral parameter
c     and flux first
         do i=1,npnts
            read(charline(i),*,err=8) spec(nspec,1,i),spec(nspec,2,i)
         enddo

         goto 9
 8       if (outflag) WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :        filename(sindex:eindex),
     :        '. Double precision read error'
         err=.true.
         return

 9       if (filename(eindex-3:eindex).eq.'.dat') then
c     Find the continuum file and read it in
            contname=filename(sindex:eindex-4)//'.cont.dat'
            OPEN(unit=1,file=contname(sindex:eindex+5),
     :           status='old',err=10)
            goto 11
            
 10         if (outflag) WRITE(*,'(2(a))') 
     :           'WARNING: No continuum file. ',
     :           'Setting continuum = 1 everywhere.'
            do i=1,npnts
               spec(nspec,3,i)=1.d0
            enddo
            contname=zeroname
            contname='unity'
            goto 24

 11         i=0
 12         i=i+1
            READ(1,'(a)',err=13,end=14) charline(i)
            goto 12
            
 13         if (outflag) WRITE(*,'(3(a))') 
     :           'WARNING: Error reading file ',
     :           contname(sindex:eindex+5),'. Character string error.'
            err=.true.
            return
            
 14         close(1)
            if (npnts.ne.i-1) then
               if (outflag) WRITE(*,'(4(a))') 
     :              'WARNING: Error reading file ',
     :              contname(sindex:eindex+5),
     :              '. Number of points not consistent with ',
     :              filename(sindex:eindex)
               err=.true.
               return
            endif

c     Find the number of columns and make sure it is 2
            call getncols(charline,120,npnts,ncols,err)
            if (err) then
               if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              contname(sindex:eindex+5),
     :              '. Column separation error.'
               err=.true.
               return
            endif
            if (ncols.ne.2) then
               if (outflag) WRITE(*,'(a,a,a,i2)') 
     :              'WARNING: Error reading file ',
     :              contname(sindex:eindex+5),
     :              '. Invalid number of colmuns = ',ncols
               err=.true.
               return
            else
               do i=1,npnts
                  read(charline(i),*,err=15) dummy,spec(nspec,3,i)
                  if (dabs((dummy-spec(nspec,1,i))/dummy).gt.
     :                 tol_spsim) then
                     if (outflag) WRITE(*,'(a,a,a,a,a,a,i5)')
     :                    'WARNING: Error reading file',
     :                    contname(sindex:eindex+5),
     :                    '. Spectral parameter inconsistent ',
     :                    'with ',filename(sindex:eindex),' on line',i
                     err=.true.
                     return
                  endif
               enddo
               goto 16
 15            if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              contname(sindex:eindex+5),
     :              '. Double precision read error'
               err=.true.
               return

 16            continue
            endif
         else
c     Data file not ending in .dat so ask for the continuum name
            call rchar(contname,'Continuum file name?',
     :           filename(sindex:eindex)//'cont')
            OPEN(unit=1,file=contname,status='old',err=17)
            call getlen(contname,sindex2,eindex2)
            goto 18
            
 17         if (outflag) WRITE(*,'(a)') 'WARNING: File does not exist!'
            err=.true.
            return
            
 18         i=0
 19         i=i+1
            charline(i)=charline0
            READ(1,'(a)',err=20,end=21) charline(i)
            goto 19
            
 20         if (outflag) WRITE(*,'(3(a))') 
     :           'WARNING: Error reading file ',
     :           contname(sindex2:eindex2),'. Character string error.'
            err=.true.
            return
            
 21         close(1)
            if (npnts.ne.i-1) then
               if (outflag) WRITE(*,'(5(a))') 
     :              'WARNING: Error reading file. ',
     :              'Number of points in ',contname(sindex2:eindex2),
     :              ' not consistent with ',filename(sindex:eindex)
               err=.true.
               return
            endif

            call getncols(charline,120,npnts,ncols,err)
            if (err) then
               if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              contname(sindex2:eindex2),
     :              '. Column separation error.'
               err=.true.
               return
            endif
            if (ncols.ne.2) then
               if (outflag) WRITE(*,'(a,a,a,i2)') 
     :              'WARNING: Error reading file ',
     :              contname(sindex2:eindex2),
     :              '. Invalid number of colmuns = ',ncols
               err=.true.
               return
            else
               do i=1,npnts
                  read(charline(i),*,err=22) dummy,spec(nspec,3,i)
                  if (dabs((dummy-spec(nspec,1,i))/dummy).gt.
     :                 tol_spsim) then
                     if (outflag) WRITE(*,'(a,a,a,a,a,a,i5)')
     :                    'WARNING: Error reading file',
     :                    contname(sindex2:eindex2),
     :                    '. Spectral parameter inconsistent ',
     :                    'with ',filename(sindex:eindex),
     :                    ' on line',i
                     err=.true.
                     return
                  endif
               enddo
               goto 23
 22            if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              contname(sindex2:eindex2),
     :              '. Double precision read error'
               err=.true.
               return
               
 23            continue
            endif
         endif

c     Now read in the error array. First find it.
 24      if (filename(eindex-3:eindex).eq.'.dat') then
            signame=filename(sindex:eindex-4)//'.sig.dat'
            OPEN(unit=1,file=signame,status='old',err=25)
            goto 26
            
 25         if (outflag) WRITE(*,'(2(a))') 'WARNING: No sigma file. ',
     :           'Setting sigma = 1 everywhere.'
            do i=1,npnts
               spec(nspec,4,i)=1.d0
            enddo
            contname=zeroname
            contname='unity'
            goto 38
            
 26         i=0
 27         i=i+1
            charline(i)=charline0
            READ(1,'(a)',err=28,end=29) charline(i)
            goto 27
            
 28         if (outflag) WRITE(*,'(3(a))') 
     :           'WARNING: Error reading file ',
     :           signame(sindex:eindex+4),
     :           '. Character string error.'
            err=.true.
            return
            
 29         close(1)
            if (npnts.ne.i-1) then
               if (outflag) WRITE(*,'(4(a))') 
     :              'WARNING: Error reading file ',
     :              signame(sindex:eindex+4),
     :              '. Number of points not consistent with ',
     :              filename(sindex:eindex)
               err=.true.
               return
            endif

            call getncols(charline,120,npnts,ncols,err)
            if (err) then
               if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              signame(sindex:eindex+4),
     :              '. Column separation error.'
               err=.true.
               return
            endif
            if (ncols.ne.2) then
               if (outflag) WRITE(*,'(a,a,a,i2)') 
     :              'WARNING: Error reading file ',
     :              signame(sindex:eindex+4),
     :              '. Invalid number of colmuns = ',ncols
               err=.true.
               return
            else
               do i=1,npnts
                  read(charline(i),*,err=30) dummy,spec(nspec,4,i)
                  if (dabs((dummy-spec(nspec,1,i))/dummy).gt.
     :                 tol_spsim) then
                     if (outflag) WRITE(*,'(a,a,a,a,a,a,i5)')
     :                    'WARNING: Error reading file ',
     :                    signame(sindex:eindex+4),
     :                    '. Spectral parameter inconsistent ',
     :                    'with ',filename(sindex:eindex),' on line ',
     :                    i
                     err=.true.
                     return
                  endif
               enddo
               goto 31
 30            if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              signame(sindex:eindex+4),
     :              '. Double precision read error'
               err=.true.
               return
               
 31            continue
            endif
         else
c     Data file not ending in .dat so ask for error file directly
            call rchar(signame,'Sigma file name?',filename//'sig')
            OPEN(unit=1,file=signame,status='old',err=32)
            call getlen(signame,sindex2,eindex2)
            goto 33
            
 32         if (outflag) WRITE(*,'(a)') 'WARNING: File does not exist!'
            err=.true.
            return
            
 33         i=0
 34         i=i+1
            charline(i)=charline0
            READ(1,'(a)',err=35,end=36) charline(i)
            goto 34
            
 35         if (outflag) WRITE(*,'(3(a))') 
     :           'WARNING: Error reading file ',
     :           signame(sindex2:eindex2),
     :           '. Character string error.'
            err=.true.
            return
            
 36         close(1)
            if (npnts.ne.i-1) then
               if (outflag) WRITE(*,'(5(a))') 
     :              'WARNING: Error reading file. ',
     :              'Number of points in ',signame(sindex2:eindex2),
     :              ' not consistent with ',filename(sindex:eindex)
               err=.true.
               return
            endif

            call getncols(charline,120,npnts,ncols,err)
            if (err) then
               if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              signame(sindex2:eindex2),
     :              '. Column separation error.'
               err=.true.
               return
            endif
            if (ncols.ne.2) then
               if (outflag) WRITE(*,'(a,a,a,i2)') 
     :              'WARNING: Error reading file ',
     :              signame(sindex2:eindex2),
     :              '. Invalid number of colmuns = ',ncols
               err=.true.
               return
            else
               do i=1,npnts
                  read(charline(i),*,err=37) dummy,spec(nspec,4,i)
                  if (dabs((dummy-spec(nspec,1,i))/dummy).gt.
     :                 tol_spsim) then
                     if (outflag) WRITE(*,'(a,a,a,a,a,a,i5)')
     :                    'WARNING: Error reading file',
     :                    signame(sindex2:eindex2),
     :                    '. Spectral parameter inconsistent ',
     :                    'with ',filename(sindex:eindex),' on line',i
                     err=.true.
                     return
                  endif
               enddo
               goto 38
 37            if (outflag) WRITE(*,'(3(a))') 
     :              'WARNING: Error reading file ',
     :              signame(sindex2:eindex2),
     :              '. Double precision read error'
               err=.true.
               return               
            endif
c 38         continue
 38      endif
      endif

      return
      end
