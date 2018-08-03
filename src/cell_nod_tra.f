C
C**************************  CELL_NOD_TRA ******************************
C
C  transfer cell-values to nodal values taking into account
C  possible coarsening of the triangulation (dostep>1)
C
C***********************************************************************
C
      subroutine cell_nod_tra(ncell,nnod,cell,conccel,concnod,
     1                    delta_x,delta_y,arenod,pondcel,pondnod)
C
      implicit none
      integer ncell,nnod
      integer k,ii,inod,i
      integer cell(5,*)
      real*8  concnod(*),conccel(*)
      real*8  mass_node(nnod)
      real*8  delta_x,delta_y,arenod(*)
      real*8  pondcel(*),pondnod(*)
C
      call init0r(nnod,mass_node)
      call init0r(nnod,concnod)
      DO K=1,ncell
         DO II=1,4
            INOD=CELL(II,K)
            mass_node(INOD)= mass_node(INOD) + conccel(k) *
     1              delta_x*delta_y*pondcel(k)/4
         END DO
      END DO
      DO i=1,NNOD
        if (pondnod(i).NE.0.0d0) then
         concnod(i) = mass_node(i)/ (arenod(i)*pondnod(i))
        end if
      END DO
C
      RETURN
      END
