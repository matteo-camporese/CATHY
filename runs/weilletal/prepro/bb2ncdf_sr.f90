!-----------------------------------------------------------------------------
!
! BB2NCDF
!
! The program BB2NCDF reads the parameters contained
! in the binary files basin_b/basin_i and writes many DTM files in a netCDF
! output file.
!
!-----------------------------------------------------------------------------
subroutine bb2ncdf_sr

use netcdf
use mpar
use mbbio

implicit none

integer(kind=ISP) i,j,i_basin

real(kind=RSP) :: nodata

real(kind=REP),allocatable :: topo(:,:)
real(kind=REP),allocatable :: ainf(:,:)
real(kind=REP),allocatable :: ls1(:,:)
real(kind=REP),allocatable :: ls2(:,:)
real(kind=REP),allocatable :: ls(:,:)
real(kind=REP),allocatable :: w1(:,:)
real(kind=REP),allocatable :: w2(:,:)

real(kind=REP),allocatable :: hcid(:,:)

integer(kind=ISP) ,dimension(2) :: dimids
integer(kind=ISP) ,dimension(4) :: var_id
integer(kind=ISP) :: file_id, status


allocate(topo(N,M))
allocate(ainf(N,M))
allocate(ls1(N,M))
allocate(ls2(N,M))
allocate(ls(N,M))
allocate(w1(N,M))
allocate(w2(N,M))
allocate(hcid(N,M))

call rparfile('hap.in')
call load_dtm("basin_b","basin_i")

do i=1,N
   do j=1,M
      i_basin=(i-1)*M+j
      if (dtm_index_pr(i_basin) == 0)  then
         topo(i,j) = nodata
         ainf(i,j) = nodata
         ls1(i,j)  = nodata
         ls2(i,j)  = nodata
         ls(i,j)   = nodata
         w1(i,j)   = nodata
         w2(i,j)   = nodata
         hcid(i,j) = 1e+36
      else
         topo(i,j) = dtm_quota(i_basin)
         ainf(i,j) = dtm_A_inflow(i_basin)
         hcid(i,j) = dtm_hcID(i_basin)
         ls1(i,j)  = dtm_local_slope_1(i_basin)
         ls2(i,j)  = dtm_local_slope_2(i_basin) 
         w1(i,j)   = dtm_w_1(i_basin)
         w2(i,j)   = dtm_w_2(i_basin)
         ls(i,j)   = w1(i,j) * ls1(i,j) + w2(i,j) * ls2(i,j)
      end if
   end do
end do

!    
status = nf90_create("CATHY_prepro.nc", NF90_CLOBBER, file_id)
status = nf90_def_dim(file_id, "LON", N, dimids(1))
status = nf90_def_dim(file_id, "LAT", M, dimids(2))
status = nf90_def_var(file_id, "DEM", NF90_DOUBLE, dimids, var_id(1))
status = nf90_def_var(file_id, "AINF", NF90_DOUBLE, dimids, var_id(2))
status = nf90_def_var(file_id, "LS", NF90_DOUBLE, dimids, var_id(3))
status = nf90_def_var(file_id, "HCID", NF90_DOUBLE, dimids, var_id(4))
status = nf90_enddef(file_id)
status = nf90_put_var(file_id, var_id(1), topo)
status = nf90_put_var(file_id, var_id(2), ainf)
status = nf90_put_var(file_id, var_id(3), ls)
status = nf90_put_var(file_id, var_id(4), hcid)
status = nf90_close(file_id)


call close_dtm()

end subroutine bb2ncdf_sr
