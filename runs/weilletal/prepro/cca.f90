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
real(kind=REP) quota_cij,zx,zy,zxx,zyy,zxy,p,q
real(kind=REP) zx_old,zy_old,zxx_old,zyy_old,zxy_old
real(kind=RSP) Kc,Kp,Ktot
real(kind=REP) quota_ciijj,twodx,dxtwo,fourdxtwo

real(kind=REP),allocatable :: mesh(:,:)

! read <parfile>

call rparfile('hap.in')

allocate(mesh(3,3))

write(6,*) 'contour curvature threshold value = ',CC_threshold
! write(6,*) 'delta_x = ', delta_x

twodx=2*delta_x
dxtwo=delta_x*delta_x
fourdxtwo=4*dxtwo

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
      zx_old=(mesh(2,3)-mesh(2,1))/(2*delta_x)
      zy_old=(mesh(1,2)-mesh(3,2))/(2*delta_x)
      zxx_old=(mesh(2,3)-2*mesh(2,2)+mesh(2,1))/dxtwo
      zyy_old=(mesh(1,2)-2*mesh(2,2)+mesh(3,2))/dxtwo
      zxy_old=(-mesh(1,1)+mesh(1,3)+mesh(3,1)-mesh(3,3))/(4*dxtwo)
      !Terrain Analysis definition
      zx=(mesh(3,2)-mesh(1,2))/twodx
      zy=(mesh(2,3)-mesh(2,1))/twodx
      zxx=(mesh(3,2)-2*mesh(2,2)+mesh(1,2))/dxtwo
      zyy=(mesh(2,3)-2*mesh(2,2)+mesh(2,1))/dxtwo
      zxy=(-mesh(1,3)+mesh(3,3)+mesh(1,1)-mesh(3,1))/fourdxtwo

      if (abs(zx-(-zy_old)).ge.1.0D-12) stop 'abs(zx-(-zy_old)).ge.1.0D-12'
      if (abs(zy-zx_old).ge.1.0D-12) stop 'abs(zy-zx_old).ge.1.0D-12'
      if (abs(zxx-zyy_old).ge.1.0D-12) stop 'abs(zxx-zyy_old).ge.1.0D-12'
      if (abs(zyy-zxx_old).ge.1.0D-12) stop 'abs(zyy-zxx_old).ge.1.0D-12'
      if (abs(zxy-(-zxy_old)).ge.1.0D-12) stop 'abs(zxy-(-zxy_old)).ge.1.0D-12'

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
      call set_DN(i_basin,Kc)
!ms
!ms      write(18,*)i_basin,Kc
!ms
   end do
end do

return
end subroutine cca
