C
C**************************  CELL_NOD **********************************
C
C  transfer cell-values to nodal values taking into account
C  possible coarsening of the triangulation (dostep>1)
C
C***********************************************************************
C
      subroutine cell_nod(ncell,nrow,ncol,nnod,ntri,dostep,
     1                    cell,tp2d,triang,indcel,dem_map,
     2                    pondcell,cellcoarse,pondnod,
     3                    delta_x,delta_y,arenod)

      implicit none
      include 'CATHY.H'
      
      integer ncell,nrow,ncol,nnod,ntri,dostep
      integer icell,i,j,k,l,ii,inod,nonnull,m,n
      integer tp2d(nnod),triang(4,ntri),cell(5,*)
      integer indcel(rowmax,ncol)
      real*8  pondnod(nnod),cellcoarse(maxcel),pondcell(maxcel)
      real*8  dem_map(rowmax,ncol),volume_node(nnod)
      real*8  delta_x,delta_y,arenod(*)
c      do i=1,ncell
c         cellcoarse(i) = 0.0d0
c      end do
c 
c dalle celle piccole alle grandi
c
c Laura : Idem than in nod_cell -> I cut the second part of the subroutine 
c because it doesn't conserve the total mass/volume. Carefull if dostep different than 1 !!

c      icell = 0
c      do i=1,nrow,dostep
c         do j=1,ncol,dostep
cc       write(6,*) 'i,j=',i,j,'dem=',dem_map(i,j),'indcel=',indcel(i,j)
c          nonnull=0
c          do m=1,dostep
c           do n=1,dostep
c            if(dem_map(i+m-1,j+n-1).ne.0 .and. i+dostep-1.le.nrow
c     &                      .and. j+dostep-1.le.ncol) then
c               nonnull=nonnull+1
c            end if
c           end do
c          end do
ccccd            write(6,*) 'i,j=',i,j,'nonnull=',nonnull
c            if(nonnull.gt.0) then  
c               icell = icell +1
c               do k=1,dostep
c                  do l=1,dostep
c                    if(dem_map(i+k-1,j+l-1) .ne. 0 .and.
c     1                 indcel(i+k-1,j+l-1).ne.0)  then
c                     cellcoarse(icell) = cellcoarse(icell) + 
c     1                                   pondcell(indcel(i+k-1,j+l-1))
c
c                    end if
c                  end do 
c               end do
c               cellcoarse(icell) = cellcoarse(icell)/nonnull
cccd            write(6,*) 'icell=',icell,'cellcoarse=',cellcoarse(icell)
c            end if
c         end do
c      end do
ccc      write(6,*) 'icell=',icell
c
c dalle celle grandi ai nodi
c
    
      call init0r(nnod,volume_node)
      call init0r(nnod,pondnod)
      DO K=1,ncell
         DO II=1,4
            INOD=CELL(II,K)
            volume_node(INOD)= volume_node(INOD) + pondcell(k) *
     1              delta_x*delta_y/4
         END DO
      END DO
      DO i=1,NNOD
         pondnod(i) = volume_node(i)/arenod(i)
      END DO
C
      RETURN
      END
