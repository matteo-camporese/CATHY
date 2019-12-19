C
C***************************** NOD_CELL ********************************
C
C   transfer nodal values to dem cell values taking into account
C   coarsening of triangulation (dostep>1)
C
C***********************************************************************
C
      subroutine nod_cell(ncell,nrow,ncol,dostep,ncell_coarse,
     1                   nnod,cell,dem_map,indcelwl,cellcoarse,nodvalue,
     2                   cellvalue,arenod,delta_x,delta_y,pondnod)

      implicit none
      include 'CATHY.H'

      integer nrow,ncol
      integer icell,k,l,ncell,nnod,i,j,icell_coarse,m,n
      integer dostep,ncell_coarse,nonnull
      integer cell(5,maxcel),indcelwl(rowmax,colmax),cont(nodmax)
      real*8 delta_x,delta_y,pondnod(*)
      real*8 cellvalue(*),nodvalue(*),arenod(*)
      real*8 cellcoarse(*)
      REAL*8 dem_map(rowmax,*) 
       
C
c
        DO i=1,nnod
        cont(i)=0
        end do
        DO I=1,NCELL
            DO k=1,4
                CONT(cell(K,I))=CONT(cell(K,I))+1
            END DO
        END DO
c
c dai nodi alle celle grandi
c
      call init0r(ncell,cellcoarse)
      do icell_coarse=1,ncell_coarse
         do k=1,4
            cellcoarse(icell_coarse)=cellcoarse(icell_coarse)+
     1       nodvalue(cell(k,icell_coarse))/ cont(cell(k,icell_coarse))
         end do
      end do
c
c Laura : I cut the second part of the subroutine because it doesn't 
c conserve the total fluxes. Carefull if dostep different than 1 !!
c
      do i= 1,ncell
      cellvalue(i)=cellcoarse(i)
      end do
c
c
c dalle celle grandi alle piccole
c      
c      icell_coarse = 0
c      do i=1,nrow,dostep
c         do j=1,ncol,dostep
c          nonnull=0
c          do m=1,dostep
c           do n=1,dostep 
c            if(dem_map(i+m-1,j+n-1).ne.0 .and. i+dostep-1.le.nrow
c     &                         .and. j+dostep-1.le.ncol) then
c               nonnull=nonnull+1
c            end if
c           end do
c          end do
ccccd          write(99,*) 'i=',i,'j=',j,'nonnull=',nonnull
c            if (nonnull.gt.0) then   
c               icell_coarse = icell_coarse + 1
c               do k=1,dostep
c                 do l=1,dostep 
c                  if (dem_map(i+k-1,j+l-1).ne.0) then
c                   icell = indcelwl(i+k-1,j+l-1)
ccccccd                   write(99,*) 'cella non nulla=',icell
c                   cellvalue(icell) = cellcoarse(icell_coarse)*
c     1                                delta_x*delta_y*dostep*dostep/
c     2                                nonnull
cccccc                   write(99,*)'i=',icell,'cellvalue=',cellvalue(icell)
c                  end if
c                 end do
c               end do
c            end if
c         end do
c      end do
      return

      end
