!-----------------------------------------------------------------------------
module commfeat

use ShapeFile

implicit none

integer (kind=4) :: cnt_s_tot

type Points
   real (kind=8) :: x    !X coordinate
   real (kind=8) :: y    !Y coordinate
end type

type feat_stream
   real (kind=4) :: length    !length
   integer (kind=4) :: n_elem    !numero elementi
   integer (kind=4) :: hso   !Stralher stream order
   real (kind=4) :: area    !up slope area
   type (Point),dimension(:),pointer::nodes
end type

type table
   integer (kind=4) :: order       !strahler order
   integer (kind=4) :: nos         !number of streams
   real (kind=4) :: bif_ratio      !bifurcation ratio
   real (kind=4) :: av_bif_ratio   !average bifurcation ratio
   real (kind=4) :: av_area        !average area 
   real (kind=4) :: av_length      !average length   
end type

type(feat_stream),dimension(:),allocatable:: stream
type(table),dimension(:),allocatable:: tab_sa !table strahler analysis

end module commfeat

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!
! HA (Horton Analysis)
!
! This program perform a Horton analysis of the drainage net based on
! features computed by RN/DSF
!
!-----------------------------------------------------------------------------

program ha

use commfeat
use mpar
use mbbio
use ShapeFile
use DBF 

implicit none

integer (kind=4) :: i,j,n_elem_tmp,i_basin_chiusura,jr,max_so,cnt
real (kind=4) :: av_area,av_length,av_bif_ratio,max_len,resto
type(PolyLine),dimension(:),allocatable:: poly_tmp
type(DBFfile)::db_main

call rparfile('hap.in')

call load_dtm("basin_b","basin_i")

open(16,file='qoi',access='direct',status='unknown',form='unformatted',recl=4)

open(24,file='tab_ha.dat',status='unknown')

read(16,rec=N_celle+1) i_basin_chiusura

!max_len=dtm_upl(i_basin_chiusura)
!resto=mod(max_len,delta_x)

allocate(stream(N*M))

do i=1,N*M
   stream(i)%n_elem=0
end do

! identify the outlet cell (the lowest catchment cell)

cnt_s_tot=0

jr=mod(i_basin_chiusura,M)
if (jr.ne.0) then
   j=jr
   i=(i_basin_chiusura-j)/M+1
else
   j=M
   i=i_basin_chiusura/M
end if

call rcs(0,0,i,j)

write(6,*) 'array allocation'

! array allocation

do i=1,cnt_s_tot
   allocate(stream(i)%nodes(stream(i)%n_elem)) 
end do

cnt_s_tot=0

jr=mod(i_basin_chiusura,M)
if (jr.ne.0) then
   j=jr
   i=(i_basin_chiusura-j)/M+1
else
   j=M
   i=i_basin_chiusura/M
end if

call rcs(0,0,i,j)

write(6,*) 'writing file horton_net.shp'

call IniziaScrittura('horton_net',3) ! 3 Polyline

db_main%NomeFile='horton_net.dbf'
db_main%uni=40
db_main%nFields=4
db_main%scrittura=.TRUE.
allocate(db_main%F(db_main%nFields))

db_main%F(1)%FieldName='length'
db_main%F(1)%FieldType='F'
db_main%F(1)%FieldLength=11
db_main%F(1)%decimal=6

db_main%F(2)%FieldName='n_elem'
db_main%F(2)%FieldType='N'
db_main%F(2)%FieldLength=8
db_main%F(2)%decimal=0

db_main%F(3)%FieldName='hso'
db_main%F(3)%FieldType='N'
db_main%F(3)%FieldLength=8
db_main%F(3)%decimal=0


db_main%F(4)%FieldName='A_outflow'
db_main%F(4)%FieldType='F'
db_main%F(4)%FieldLength=12
db_main%F(4)%decimal=6

call CreaDB(db_main%NomeFile, db_main%nFields, db_main%F%FieldName, &
& db_main%F%FieldType, db_main%F%FieldLength, db_main%F%decimal, db_main%uni)

call InitDbf(db_main%NomeFile, db_main, db_main%uni, db_main%scrittura)

do i=1,cnt_s_tot
   n_elem_tmp=stream(i)%n_elem
   allocate(poly_tmp(1))
   allocate(poly_tmp(1)%Points(n_elem_tmp))
   poly_tmp(1)%Points(1:n_elem_tmp)=stream(i)%nodes(1:n_elem_tmp)
   call ScriviPolyline(poly_tmp(1))
   call Add_Record(db_main)
   call Set_Field_real(db_main, db_main%F(1)%FieldName, stream(i)%length)
   call Set_Field_integer(db_main, db_main%F(2)%FieldName, stream(i)%n_elem)
   call Set_Field_integer(db_main, db_main%F(3)%FieldName, stream(i)%hso)
   call Set_Field_real(db_main, db_main%F(4)%FieldName, stream(i)%area)
   call Scrivi_Record(db_main)
   deallocate(poly_tmp)
end do

call ChiudiScrittura()
call CloseDbf(db_main) 

max_so=maxval(stream(:)%hso)

allocate(tab_sa(max_so))

write(24,'(a1,12x,a6,23x,a7,4x,a12,8x,a7)') '#','Number', &
'Average','Average Area','Average'
write(24,'(a1,16x,a2,4x,a11,4x,a11,7x,a9,9x,a6)')'#','of', &
'Bifurcation','Bifurcation','of Single','Length'
write(24,'(a7,5x,a7,10x,a5,10x,a5,5x,a11,12x,a3)')'# Order', &
'Streams','Ratio','Ratio','Basin (m^2)','(m)'
write(24,'(a1)') '#'
do i=1,max_so
   cnt=0
   av_area=0.0E0
   av_length=0.0E0
   do j=1,cnt_s_tot
      if (stream(j)%hso == i) then
         cnt=cnt+1
         av_area=av_area+stream(j)%area
         av_length=av_length+stream(j)%length
      end if
   end do
   tab_sa(i)%order=i
   tab_sa(i)%nos=cnt
   tab_sa(i)%av_area=av_area/cnt
   tab_sa(i)%av_length=av_length/cnt
   if (i > 1) then
      tab_sa(i)%bif_ratio=real(tab_sa(i-1)%nos)/real(tab_sa(i)%nos)
   else
      tab_sa(i)%bif_ratio=0.0E0
   end if   
end do

do i=2,max_so-1
   av_bif_ratio=0.0
   cnt=0
   do j=i,max_so
      cnt=cnt+1
      av_bif_ratio=av_bif_ratio+tab_sa(j)%bif_ratio
   end do
   tab_sa(i)%av_bif_ratio=av_bif_ratio/cnt
end do

tab_sa(1)%av_bif_ratio=0.0E0
tab_sa(max_so)%av_bif_ratio=0.0E0

do i=1,max_so
   write(24,'(i7,2x,i10,3x,f12.4,3x,f12.4,2x,f14.2,3x,f12.2)') & 
   tab_sa(i)%order,tab_sa(i)%nos,tab_sa(i)%bif_ratio,tab_sa(i)%av_bif_ratio, &
   tab_sa(i)%av_area,tab_sa(i)%av_length
end do

close (16)

end program ha

!-----------------------------------------------------------------------------

recursive subroutine rcs(i_cv,j_cv,i_init,j_init)

use commfeat
use mpar
use mbbio

implicit none

!private
integer(kind=4) :: i,j,ii,jj,i_basin,ii_basin,i_cm,j_cm
integer(kind=4) :: cm,cnt_n,cnt_s
integer(kind=4), intent(in) :: i_cv,j_cv,i_init,j_init
integer(kind=4) :: p_outflow_1_ciijj,p_outflow_2_ciijj,p_inflow

real(kind=8) :: len_stream

i=i_init
j=j_init
i_basin=(i-1)*M+j

cnt_s_tot=cnt_s_tot+1
cnt_s=cnt_s_tot
cnt_n=0

if (i_cv /= 0 .and. j_cv /= 0) then
   cnt_n=cnt_n+1
   if (stream(cnt_s)%n_elem > 0) &
   stream(cnt_s)%nodes(cnt_n)%x=xllcorner+delta_x/2+(i_cv-1)*delta_x
   if (stream(cnt_s)%n_elem > 0) &
   stream(cnt_s)%nodes(cnt_n)%y=yllcorner+delta_y/2+(j_cv-1)*delta_y
end if

cnt_n=cnt_n+1
if (stream(cnt_s)%n_elem > 0) &
stream(cnt_s)%nodes(cnt_n)%x=xllcorner+delta_x/2+(i-1)*delta_x
if (stream(cnt_s)%n_elem > 0) &
stream(cnt_s)%nodes(cnt_n)%y=yllcorner+delta_y/2+(j-1)*delta_y

if (i_cv /= 0 .and. j_cv /= 0) then
   if (i_cv == i .or. j_cv == j) then
      len_stream=delta_x  !length current stream
   else
      len_stream=delta_x*dsqrt(2.0D0)
   end if
end if

cnt_n=cnt_n+1
cm=1
do while (cm.eq.1)
   cm=0
   do ii=i-1,i+1
      do jj=j-1,j+1
      if (ii.eq.0.or.ii.eq.N+1) cycle
         if (jj.eq.0.or.jj.eq.M+1) cycle
         ii_basin=(ii-1)*M+jj
         if (dtm_index_pr(ii_basin) == 0) cycle
         p_outflow_1_ciijj=dtm_p_outflow_1(ii_basin)*dtm_w_1(ii_basin)
         p_outflow_2_ciijj=dtm_p_outflow_2(ii_basin)*dtm_w_2(ii_basin)
         p_inflow=3*(ii-i)+(jj-j)+5
         if (p_inflow+p_outflow_1_ciijj.eq.10) then
            if (dtm_hso(ii_basin).ne.dtm_hso(i_basin)) then
               call rcs(i,j,ii,jj)
            else
               if (stream(cnt_s)%n_elem > 0) &
               stream(cnt_s)%nodes(cnt_n)%x=xllcorner+delta_x/2+(ii-1)*delta_x
               if (stream(cnt_s)%n_elem > 0) &
               stream(cnt_s)%nodes(cnt_n)%y=yllcorner+delta_y/2+(jj-1)*delta_y
               len_stream=len_stream+delta_x
               cnt_n=cnt_n+1 
               cm=1
               i_cm=ii
               j_cm=jj
             end if
         end if 
         if (p_inflow+p_outflow_2_ciijj.eq.10) then
            if (dtm_hso(ii_basin).ne.dtm_hso(i_basin)) then
               call rcs(i,j,ii,jj)
            else
               if (stream(cnt_s)%n_elem > 0) &
               stream(cnt_s)%nodes(cnt_n)%x=xllcorner+delta_x/2+(ii-1)*delta_x
               if (stream(cnt_s)%n_elem > 0) &
               stream(cnt_s)%nodes(cnt_n)%y=yllcorner+delta_y/2+(jj-1)*delta_y
               len_stream=len_stream+delta_x*dsqrt(2.0D0)
               cnt_n=cnt_n+1 
               cm=1
               i_cm=ii
               j_cm=jj
            end if
         end if 
      end do
   end do
   i=i_cm
   j=j_cm  
end do !while

cnt_n=cnt_n-1
stream(cnt_s)%length=len_stream
stream(cnt_s)%n_elem=cnt_n
stream(cnt_s)%hso=dtm_hso(i_basin)
stream(cnt_s)%area=dtm_A_inflow(i_basin)+delta_x*delta_y

end subroutine rcs
