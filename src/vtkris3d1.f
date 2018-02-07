         subroutine vtkris3d1(ntetra, nnode,iunit,tetra,nstr,ntri,
     1                       mean,stdev,x,y,z,ks,time)
         implicit none
         include 'CATHY.H'
         integer nnode, ntetra, iunit,  i, j, numb,nstr
         integer tetra(5,*), field,vtkf,ntri
         real*8 ks(*)
         real*8 mean(maxstr,*),stdev(maxstr,*)
         real*8 x(*),y(*),z(*)
         real*8 time
         character*15 nome

         field=1

         write(nome,'(a1,i3,a4)') 'a',iunit, '.vtk'
         open(iunit, file=nome)
        
         write(iunit,78) 
 78      FORMAT('# vtk DataFile Version 2.0',
     1     /,'3D Unstructured Grid of Linear Triangles', 
     2     /,'ASCII')

         write(iunit,77) field, field, field, time
 77      FORMAT('DATASET UNSTRUCTURED_GRID',
     1     /, 'FIELD FieldData',1x, i2,
     2     /, 'TIME', 1x, i1,1x,i1, 1x, 'double',
     3     /, 1f18.5)

        write(iunit,79) nnode
 79     FORMAT('POINTS',1x, i8, ' float')

         do i=1,nnode
             write(iunit,100) x(i), y(i), z(i)
         end do
 100     format(4x, 3(1pe16.8))

         numb=ntetra*5
         write(iunit, 80) ntetra,numb
 80      format('CELLS',1x,i8,1x,i8)
         j=4
         do i=1,ntetra
           write(iunit,101) j, tetra(1,i)-1, tetra(2,i)-1, 
     1                    tetra(3,i)-1, tetra(4,i)-1
         end do
 101     format(i1, 3x,i8,3x,i8,3x,i8,3x,i8)

         write(iunit,81)  ntetra
 81      format('CELL_TYPES',i8)
         do i=1,ntetra
          write(iunit,*) 10
         end do

        write(iunit,82)  ntetra
 82     format('CELL_DATA', 1x,i8, 
     1    /, 'SCALARS mean float',
     2    /, 'LOOKUP_TABLE default' )
        do i =1,nstr
           do j=(i-1)*ntri*3+1,i*ntri*3
              ks(j)=mean(i,tetra(5,j))
           end do
        end do

        do i=1,ntetra
           write(iunit,*) ks(i)
        end do

        write(iunit,83)
 83     format('SCALARS stdev float',
     2    /,   'LOOKUP_TABLE default' )
        do i =1,nstr
           do j=(i-1)*ntri*3+1,i*ntri*3
              ks(j)=stdev(i,tetra(5,j))
           end do
        end do

        do i=1,ntetra
           write(iunit,*) ks(i)
        end do

         close(iunit)
         return
         end
         
