c     MM 27/11/00 Subroutine to find the number of columns contained in
c     an array of character strings. The first array element is checked
c     and then the rest of the elements are checked to make sure they
c     contain the same number of columns
      subroutine getncols(charline,nchar,nlines,ncols,err)
      implicit none
      integer j,i,nchar,nlines,ncols,ncols_new
      character*(*) charline(nlines)
      logical err

      err=.false.
      ncols_new=0

      do j=1,nlines
         if ((j.eq.2).or.(j.eq.1)) ncols=ncols_new
         if (ncols.ne.ncols_new) then
            err=.true.
            return
         endif
c     Find the first non-space
         i=0
 2       i=i+1
         if (charline(j)(i:i).eq.' ') goto 2
c     Scan whole line to see how many columns of data there are
         ncols_new=0
 3       i=i+1
         if (i.gt.nchar) goto 4
         if (charline(j)(i:i).ne.' ') then
            goto 3
         elseif ((charline(j)(i:i).eq.' ').and.
     :           (charline(j)(i-1:i-1).ne.' ')) then
            ncols_new=ncols_new+1
            goto 3
         else
            goto 3
         endif
 4       if (charline(j)(nchar:nchar).ne.' ') ncols_new=ncols_new+1
      enddo

      return
      end

