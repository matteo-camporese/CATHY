         subroutine vtkris2d(ntria, nnode,iunit,triang,x,z,mean,stdev,
     1                       time)
         implicit none
         include 'CATHY.H'
         integer nnode, ntria, iunit,  i, j, numb,field
         integer triang(4,*)
         real*8 x(*), z(*)
         real*8 mean(maxstr,*),stdev(maxstr,*),time
         character*15 nome


         write(nome,'(a1,i4,a4)') 'a',iunit, '.vtk'
         open(iunit, file=nome)
        
         write(iunit,78) 
 78      FORMAT('# vtk DataFile Version 2.0',
     1     /,'2D Unstructured Grid of Linear Triangles', 
     2     /,'ASCII')

         field=1
        write(iunit,77) field, field, field, time
 77      FORMAT('DATASET UNSTRUCTURED_GRID',
     1     /, 'FIELD FieldData',1x, i2,
     2     /, 'TIME', 1x, i1,1x,i1, 1x, 'double',
     3     /, 1f18.5)

         write(iunit,79) nnode
 79     FORMAT('POINTS',1x, i8, ' float')
         do i=1,nnode
             write(iunit,100) x(i), z(i), 0.d0 
         end do
 100     format(4x, 3(1pe16.7))

         numb=ntria*4
         write(iunit, 80) ntria,numb
 80      format('CELLS',1x,i8,1x,i8)
         j=3
         do i=1,ntria
           write(iunit,101) j, triang(1,i)-1, triang(2,i)-1, 
     1                    triang(3,i)-1
         end do
 101     format(i1, 3x,i8,3x,i8,3x,i8,3x,i8)

         write(iunit,81)  ntria
 81      format('CELL_TYPES',i8)
         do i=1,ntria
          write(iunit,*) 5
         end do

         write(iunit,82)  ntria
c 82      format('CELL_DATA', 1x,i8, 
 82      format('CELL_DATA', 1x,i8, 
     1    /, 'SCALARS mean float',
     2    /, 'LOOKUP_TABLE default' )
c         do i=1,ntria
         do i=1,ntria
          write(iunit,*) mean(1,triang(4,i))
         end do

         write(iunit,83)  
 83      format('SCALARS stdev float',
     2    /, 'LOOKUP_TABLE default' )
c         do i=1,ntria
         do i=1,ntria
          write(iunit,*) stdev(1,triang(4,i))
         end do
         close(iunit)
         return

         end
         
