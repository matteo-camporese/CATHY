C*****************************  ZTZVEC  ********************************
C
C    pvec := Z^T Z vec :       scr=Z vec; pvec = Z^T scr
C
C    la matrice Z^T (triangolare alta) e' memorizzata nel solito 
C    formato (CSR)
C    si usa il prodotto per matrici non simmetriche poiche' Z^T 
C    e' triangolare alta
C
C***********************************************************************
      subroutine ztzvec(nequ,nterm,ia,ja,ztmat,vec,scr,pvec)
c
      implicit none
      integer nequ,nterm
      integer ja(nterm),ia(nequ+1)
      integer k,m,mm,i
      real*8  ztmat(nterm),vec(nequ),pvec(nequ)
      real*8  scr(nequ)
c
c   scr=Z x equivale al prodotto della matrice trasposta per un vettore
c
      do k=1,nequ
         scr(k)=0.0d0
         pvec(k)=0.0d0
      end do
      do k=1,nequ
         m=ia(k)
         mm=ia(k+1)-1
         do i=m,mm
            scr(ja(i))=scr(ja(i))+ztmat(i)*vec(k)
         end do
      end do
c
c  y = Z^T scr equivale al prodotto della matrice per un vettore
c
      do k=1,nequ
         m=ia(k)
         mm=ia(k+1)-1
         do i=m,mm
            pvec(k)=pvec(k)+ztmat(i)*scr(ja(i))
         end do
      end do
c
      return
      end
