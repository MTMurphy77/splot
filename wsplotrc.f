c MM 29/11/00 Subroutine to write out the current plot configuration
      subroutine wsplotrc(filenm,contnm,signm,nspec,xmin,xmax,
     :     ymin,ymax,plot,hist,pcol,model,label,anot,anotpos,
     :     spchar,spunit,spzero)
      implicit none
      include 'splot_inc.f'
      include 'charlen_inc.f'
      double precision spunit(maxnspec,2)
      real xmin(maxnspec),xmax(maxnspec),ymin(maxnspec),ymax(maxnspec)
      real anotpos(maxnspec,maxanot,2),spzero(maxnspec)
      integer hist(maxnspec,maxspecdim),pcol(maxnspec,maxspecdim)
      integer plot(maxnspec,maxspecdim),model(maxnspec)
      integer sindex,eindex,i,j,nspec
      character*1 spchar(maxnspec,2)
      character*(namelen) filenm(maxnspec),contnm(maxnspec)
      character*(namelen) signm(maxnspec)
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
     
      OPEN(unit=1,file='splotrc',status='unknown')

      do i=1,nspec
         call getlen(filenm(i),sindex,eindex)
         WRITE(1,'(2(a))') 'file=',filenm(i)(sindex:eindex)
         call getlen(contnm(i),sindex,eindex)
         WRITE(1,'(2(a))') 'cont=',contnm(i)(sindex:eindex)
         call getlen(signm(i),sindex,eindex)
         WRITE(1,'(2(a))') 'sig=',signm(i)(sindex:eindex)
         WRITE(1,'(a)') 'Spectral parameter transformation:'
         do j=1,2
            if (spchar(i,j).eq.' ') spchar(i,j)='n'
         enddo
         WRITE(1,*) spchar(i,1),
     :        spunit(i,1),spchar(i,2),spunit(i,2),
     :        spzero(i)
         do j=1,2
            if (spchar(i,j).eq.'n') spchar(i,j)=' '
         enddo
         WRITE(1,'(a)') 'plot range:'
         WRITE(1,*) xmin(i),xmax(i),ymin(i),ymax(i)
         WRITE(1,'(a)') ''
      enddo
      WRITE(1,'(2(a))') 'plot matrix: Flux, Cont., Error,',
     :     ' Norm. error, Model, Old model, Zero'
      WRITE(1,'(a)') 'f  c  e  n  m om  z'
      do i=1,nspec
         WRITE(1,'(7(i1,2x))') plot(i,2),plot(i,3),
     :        plot(i,4),plot(i,5),plot(i,6),plot(i,7),plot(i,8)
      enddo
      WRITE(1,'(2(a))') 'Data type matrix: 1=solid, 2=dashed, ',
     :     '3=dash-dot, '
      WRITE(1,'(2(a))') '4=dotted, 5=dash-dot-dot-dot, 6=histogram'
      WRITE(1,'(a)') 'f  c  e  n  m om  z'
      do i=1,nspec
         WRITE(1,'(7(i1,2x))') hist(i,2),hist(i,3),
     :        hist(i,4),hist(i,5),hist(i,6),hist(i,7),hist(i,8)
      enddo
      WRITE(1,'(2(a))') 'Data colour matrix: 1=white,2=red,',
     :     '3=green,4=blue,5=cyan,'
      WRITE(1,'(2(a))') '6=violet,7=yellow,8=orange,',
     :     '9=yellowgreen,10=cyangreen,'
      WRITE(1,'(2(a))') '11=lightblue,12=purple,13=pink,14=grey,',
     :     '15=black'
      WRITE(1,'(a)') ' f  c  e  n  m om  z'
      do i=1,nspec
         WRITE(1,'(7(i2,1x))') pcol(i,2),pcol(i,3),
     :        pcol(i,4),pcol(i,5),pcol(i,6),pcol(i,7),pcol(i,8)
      enddo
      WRITE(1,'(a)') 'Model existence array: 1=model exists'
      do i=1,nspec
         WRITE(1,'(i1)') model(i)
      enddo
      WRITE(1,'(a)') 'Label arrays for each spectrum: spec. num.,'
      WRITE(1,'(a)') 'x-axis, y-axis, title labels'
      do i=1,nspec
         WRITE(1,'(i2)') i
         do j=1,3
            WRITE(1,'(a)') label(i,j)
         enddo
      enddo
      WRITE(1,'(2(a))') 'Anotations per spectrum: x-pos, ',
     :     'y-pos, anotation'
      do i=1,nspec
         WRITE(1,'(i2)') i
         do j=1,maxanot
            WRITE(1,'(2(f8.6,1x),a64)') anotpos(i,j,1),
     :           anotpos(i,j,2),anot(i,j)
         enddo
      enddo

      close(1)

      return
      end
