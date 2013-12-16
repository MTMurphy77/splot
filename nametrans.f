c     MM 12/2/01 Subroutine to take atomic data and names of transitions
c     and output the line identification, resindex. If resindex=natom+1
c     then the transition name did not match any of the known transition
c     parameters.
      subroutine nametrans(ion,level,lambda0,natom,tname,resindex)
      implicit none
      include 'ratomdat_inc.f'
      include 'charlen_inc.f'
      double precision lambda0(maxnatom)
      integer natom,resindex
      integer i,templen
      character*(ionlen) ion(maxnatom),level(maxnatom)
      character*(namelen) tname
      character*(namelen) tempnm
      
      do i=1,natom
         call transname(ion(i),level(i),lambda0(i),0.0,tempnm,templen,0)
         if (index(tname,tempnm(1:templen)).eq.1) then
            resindex=i
            return
         endif
      enddo
      resindex=i+1

      return
      end
