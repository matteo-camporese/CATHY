!-----------------------------------------------------------------------------
!
! CCA
!
! This program (Contour Curvature Analysis) evaluates the curvature of each 
! cell; and  assign the dmID. 
!
!-----------------------------------------------------------------------------

subroutine cca()

use mpar
use mbbio

implicit none

integer(kind=RSP) i,j,ii,jj,i_basin,ii_basin,dmID
real(kind=REP) quota_cij,zx,zy,zxx,zyy,zxy,p,q,Kc
real(kind=RSP) Kp,Ktot
real(kind=REP) quota_ciijj,dx2

real(kind=REP),allocatable :: mesh(:,:)

! read <parfile>

call rparfile('hap.in')

allocate(mesh(3,3))

write(6,*) 'contour curvature threshold value = ',CC_threshold
! write(6,*) 'delta_x = ', delta_x

dx2=delta_x*delta_x

do j=M,1,-1
   do i=1,N
      dmID=2 ! default drainage direction method (D8-LTD)
      i_basin=(i-1)*M+j
      if (dtm_index_pr(i_basin).eq.0) cycle
      quota_cij=dtm_quota(i_basin)
      do ii=i-1,i+1
         do jj=j-1,j+1
            if (ii.eq.0.or.ii.eq.N+1) go to 100
            if (jj.eq.0.or.jj.eq.M+1) go to 100
            ii_basin=(ii-1)*M+jj
            if (dtm_index_pr(ii_basin).eq.0) go to 100
            quota_ciijj = dtm_quota(ii_basin)
            if (quota_ciijj.eq.0.0_REP) quota_ciijj=-9999.0_REP
            mesh(ii-i+2,jj-j+2)=quota_ciijj			      
         end do
      end do
      zx=(mesh(2,3)-mesh(2,1))/(2*delta_x)
      zy=(mesh(1,2)-mesh(3,2))/(2*delta_x)
      zxx=(mesh(2,3)-2*mesh(2,2)+mesh(2,1))/dx2
      zyy=(mesh(1,2)-2*mesh(2,2)+mesh(3,2))/dx2
      zxy=(-mesh(1,1)+mesh(1,3)+mesh(3,1)-mesh(3,3))/(4*dx2)
      if (abs(zx).gt.epsilon(zx).or.abs(zy).gt.epsilon(zy)) then
         p=zx*zx+zy*zy
         q=p+1.0
      else
         p=1.0E-09
         q=p+1.0E+00
      end if
      Ktot=zxx*zxx+2.0*zxy*zxy+zyy*zyy
      Kc=(zxx*zy*zy-2.0*zxy*zx*zy+zyy*zx*zx)/p**1.5
      if (abs(Kc).lt.epsilon(Kc)) Kc=0.0D+00
      Kp=(zxx*zx*zx+2.0*zxy*zx*zy+zyy*zy*zy)/(p*q**1.5)
      if (Kc.lt.CC_threshold) then ! diverging flow -> D_inf
          dmID=1
      else ! converging flow -> D8_LTD
          dmID=2
      end if
 100  continue
      call set_dmID(i_basin,dmID)
     ! call set_DN(i_basin,Kp)
	  call set_DN(i_basin,Ktot)
   end do
end do

return
end subroutine cca
