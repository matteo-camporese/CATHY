C
C**************************  DURLO  ***********************************
C
C     3D  advective flux for the FINITE VOLUME scheme
C***********************************************************************
C
      subroutine durlo(ntetra,nface,volur,plist,vn,
     1                precl,precr,time,flux)

      implicit none

      INCLUDE 'CATHY.H'
      integer ntetra,nface
      integer itetra, iface, itetra1, itetra2

      integer plist(2,*)
      real*8 time
      real*8 volur(*),c,a
      real*8 vn(*),precl(*),precr(*)
      real*8 flux(ntemax),cup
      real*8 uno,zero,qn, qn1, qn2
      parameter (uno=1.0d0,zero=0.0d0)

      
      
      call init0r(ntemax,flux)
c
c  
c
       do iface=1,nface
          c=c+precl(iface)
       end do
       do iface=1,nface
          c=c+precr(iface)
       end do
      do iface = 1,nface
         itetra1 = plist(1,iface)
         itetra2 = plist(2,iface)
          qn=vn(iface)
c     solve the Riemann problem
c
         if (qn.gt.0.0) then
            cup = precl(iface)
         else
            cup = precr(iface)
         end if
         flux(itetra1)=flux(itetra1)+qn*cup
         if(itetra2.ne.0) flux(itetra2)=flux(itetra2)-qn*cup
      end do
      do itetra = 1,ntetra
         flux(itetra)= -dabs(volur(itetra))*flux(itetra)
      end do


      return
      end

