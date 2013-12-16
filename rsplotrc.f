c     MM 29/11/00 Subroutine to read in the splotrc file which contains
c     the plot configuration
      subroutine rsplotrc(filenm,contnm,signm,nspec,xmin,xmax,
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
      integer i,j,nspec
      character*1 spchar(maxnspec,2)
      character*4 dummy4
      character*5 dummy5
      character*(namelen) filenm(maxnspec),contnm(maxnspec)
      character*(namelen) signm(maxnspec)
      character*(namelen) label(maxnspec,3)
      character*(namelen) anot(maxnspec,maxanot)
      character*80 dummy
      logical splotrc
      common /splotrc/splotrc

      OPEN(unit=1,file='splotrc',status='old',err=1)
      goto 2

 1    splotrc=.false.
      return

 2    splotrc=.true.
      i=0
 3    i=i+1
      READ(1,'(a)') dummy
      if (dummy(1:5).eq.'file=') then
         read(dummy,'(a5,a)') dummy5,filenm(i)
      else
         goto 4
      endif
      READ(1,'(a5,a)') dummy5,contnm(i)
      READ(1,'(a4,a)') dummy4,signm(i)
      READ(1,'(a)') dummy
      READ(1,*) spchar(i,1),spunit(i,1),spchar(i,2),spunit(i,2),
     :     spzero(i)
      do j=1,2
         if (spchar(i,j).eq.'n') spchar(i,j)=' '
      enddo
      READ(1,'(a)') dummy
      READ(1,*) xmin(i),xmax(i),ymin(i),ymax(i)
      READ(1,'(a)') dummy
      goto 3

 4    nspec=i-1
      READ(1,'(a)') dummy
      do i=1,nspec
         READ(1,'(7(i1,2x))') plot(i,2),plot(i,3),
     :        plot(i,4),plot(i,5),plot(i,6),plot(i,7),plot(i,8)
      enddo
      READ(1,'(a)') dummy
      READ(1,'(a)') dummy
      READ(1,'(a)') dummy
      do i=1,nspec
      READ(1,'(7(i1,2x))') hist(i,2),hist(i,3),
     :        hist(i,4),hist(i,5),hist(i,6),hist(i,7),hist(i,8)
      enddo
      READ(1,'(a)') dummy
      READ(1,'(a)') dummy
      READ(1,'(a)') dummy
      READ(1,'(a)') dummy
      do i=1,nspec
      READ(1,'(7(i2,1x))') pcol(i,2),pcol(i,3),
     :        pcol(i,4),pcol(i,5),pcol(i,6),pcol(i,7),pcol(i,8)
      enddo
      READ(1,'(a)') dummy
      do i=1,nspec
         READ(1,'(i1)') model(i)
      enddo
      READ(1,'(a)') dummy
      READ(1,'(a)') dummy
      do i=1,nspec
         READ(1,'(a)') dummy
         do j=1,3
            READ(1,'(a)') label(i,j)
         enddo
      enddo
      READ(1,'(a)') dummy
      do i=1,nspec
         READ(1,'(a)') dummy
         do j=1,maxanot
            READ(1,'(2(f8.6,1x),a64)') anotpos(i,j,1),
     :           anotpos(i,j,2),anot(i,j)
         enddo
      enddo

      close(1)
      
      return
      end
