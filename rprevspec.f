c     MM 29/11/00 Subroutine to read in data that the splotrc file says
c     was previously loaded.
      subroutine rprevspec(filename,contname,signame,nspec,spec,npnts)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      double precision spec(maxnspec,maxspecdim,maxpnts),dummy
      integer nspec,npnts(maxnspec),i,j
      integer ncols,sindex,eindex,sindex2,eindex2
      character*(namelen) filename(maxnspec),contname(maxnspec)
      character*(namelen) signame(maxnspec),zeroname
      character*120 charline(maxpnts),charline0
      logical err

c     A character string used to 'zero' other character strings
      do i=1,120
         charline0(i:i)=' '
      enddo
c     A character string used to 'zero' the filename strings
      do i=1,nmcharlength
         zeroname(i:i)=' '
      enddo

      do i=1,nspec
         call getlen(filename(i),sindex,eindex)
         OPEN(unit=1,file=filename(i),status='old',err=1)
         goto 2
         
 1       WRITE(*,'(3(a))') 'STOPPING: ',filename(i)(sindex:eindex),
     :        'does not exist!'
         stop
         
 2       j=0
 3       j=j+1
         charline(j)=charline0
         READ(1,'(a)',err=4,end=5) charline(j)
         goto 3
         
 4       WRITE(*,'(3(a))') 'STOPPING: Error reading file ',
     :        filename(i)(sindex:eindex),'. Character string error.'
         stop
         
 5       npnts(i)=j-1
         close(1)
      
c     Find the number of columns in the file
         call getncols(charline,120,npnts(i),ncols,err)
         if (err) then
            WRITE(*,'(3(a))') 'STOPPING: Error reading file ',
     :           filename(i)(sindex:eindex),
     :           '. Column separation error.'
            stop
         endif
         if ((ncols.ne.4).and.(ncols.ne.2)) then
            WRITE(*,'(a,a,a,i2)') 'STOPPING: Error reading file ',
     :           filename(i)(sindex:eindex),
     :           '. Invalid number of colmuns = ',ncols
            stop
         endif
         
c     Read the data in if all information is in the one file
         if (ncols.eq.4) then
            do j=1,npnts(i)
               read(charline(j),*,err=6) spec(i,1,j),
     :              spec(i,2,j),spec(i,3,j),spec(i,4,j)
            enddo
            goto 7
 6          WRITE(*,'(3(a))') 'STOPPING: Error reading file ',
     :           filename(i)(sindex:eindex),
     :           '. Double precision read error'
            stop

 7          contname(i)=zeroname
            contname(i)=filename(i)(sindex:eindex)
            signame(i)=zeroname
            signame(i)=filename(i)(sindex:eindex)
         else
c     Information is in several files so read in the spectral parameter
c     and flux first
            do j=1,npnts(i)
               read(charline(j),*,err=8) spec(i,1,j),spec(i,2,j)
            enddo
            goto 9

 8          WRITE(*,'(3(a))') 'STOPPING: Error reading file ',
     :           filename(i)(sindex:eindex),
     :           '. Double precision read error'
            stop

c     Now read in the continuum
 9          call getlen(contname(i),sindex2,eindex2)
            if (contname(i)(sindex2:eindex2).eq.'unity') then
               do j=1,npnts(i)
                  spec(i,3,j)=1.d0
               enddo
            else
               OPEN(unit=1,file=contname(i),status='old',err=10)
               goto 11
               
 10            WRITE(*,'(3(a))') 'STOPPING: ',
     :              contname(i)(sindex2:eindex2),'does not exist!'
               stop
               
 11            j=0
 12            j=j+1
               charline(j)=charline0
               READ(1,'(a)',err=13,end=14) charline(j)
               goto 12
               
 13            WRITE(*,'(3(a))') 'STOPPING: Error reading file ',
     :              contname(i)(sindex2:eindex2),
     :              '. Character string error.'
               stop
               
 14            if (npnts(i).ne.j-1) then
                  WRITE(*,'(4(a))') 'STOPPING: Error reading file ',
     :                 contname(i)(sindex2:eindex2),
     :                 '. Number of points not consistent with ',
     :                 filename(i)(sindex:eindex)
                  stop
               endif
               close(1)
            
               call getncols(charline,120,npnts(i),ncols,err)
               if (err) then
                  WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :                 contname(i)(sindex2:eindex2),
     :                 '. Column separation error.'
                  return
               endif
               if (ncols.ne.2) then
                  WRITE(*,'(a,a,a,i2)') 'WARNING: Error reading file ',
     :                 contname(i)(sindex2:eindex2),
     :                 '. Invalid number of colmuns = ',ncols
                  return
               else
                  do j=1,npnts(i)
                     read(charline(j),*,err=15) dummy,spec(i,3,j)
                     if (dabs((dummy-spec(i,1,j))/dummy).gt.
     :                    tol_spsim) then
                        WRITE(*,'(a,a,a,a,a,a,i5)')
     :                       'WARNING: Error reading file ',
     :                       contname(i)(sindex2:eindex2),
     :                       '. Spectral parameter inconsistent ',
     :                       'with ',filename(i)(sindex:eindex),
     :                       ' on line',j
                        return
                     endif
                  enddo
                  goto 16
 15               WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :                 contname(i)(sindex2:eindex2),
     :                 '. Double precision read error'
                  stop

 16               continue
               endif
            endif

c     Now read in the sigma array
            call getlen(signame(i),sindex2,eindex2)
            if (signame(i)(sindex2:eindex2).eq.'unity') then
               do j=1,npnts(i)
                  spec(i,4,j)=1.d0
               enddo
            else
               OPEN(unit=1,file=signame(i),status='old',err=17)
               goto 18
               
 17            WRITE(*,'(3(a))') 'STOPPING: ',
     :              signame(i)(sindex2:eindex2),'does not exist!'
               stop
               
 18            j=0
 19            j=j+1
               charline(j)=charline0
               READ(1,'(a)',err=20,end=21) charline(j)
               goto 19
               
 20            WRITE(*,'(3(a))') 'STOPPING: Error reading file ',
     :              signame(i)(sindex2:eindex2),
     :              '. Character string error.'
               stop
               
 21            if (npnts(i).ne.j-1) then
                  WRITE(*,'(4(a))') 'STOPPING: Error reading file ',
     :                 signame(i)(sindex2:eindex2),
     :                 '. Number of points not consistent with ',
     :                 filename(i)(sindex:eindex)
                  stop
               endif
               close(1)
               
               call getncols(charline,120,npnts(i),ncols,err)
               if (err) then
                  WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :                 signame(i)(sindex2:eindex2),
     :                 '. Column separation error.'
                  return
               endif
               if (ncols.ne.2) then
                  WRITE(*,'(a,a,a,i2)') 'WARNING: Error reading file ',
     :                 signame(i)(sindex2:eindex2),
     :                 '. Invalid number of colmuns = ',ncols
                  return
               else
                  do j=1,npnts(i)
                     read(charline(j),*,err=22) dummy,spec(i,4,j)
                     if (dabs((dummy-spec(i,1,j))/dummy).gt.
     :                    tol_spsim) then
                        WRITE(*,'(a,a,a,a,a,a,i5)')
     :                       'WARNING: Error reading file ',
     :                       signame(i)(sindex2:eindex2),
     :                       '. Spectral parameter inconsistent ',
     :                       'with ',filename(i)(sindex:eindex),
     :                       ' on line',j
                        return
                     endif
                  enddo
                  goto 23
 22               WRITE(*,'(3(a))') 'WARNING: Error reading file ',
     :                 signame(i)(sindex2:eindex2),
     :                 '. Double precision read error'
                  stop
                  
 23               continue
               endif
            endif
         endif
      enddo
      
      return
      end
