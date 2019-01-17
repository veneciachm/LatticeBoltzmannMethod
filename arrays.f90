module arrays

	implicit none

  	real(kind=8), allocatable, dimension (:,:,:) :: x,y,z
  	real(kind=8), allocatable, dimension (:) :: w,cx,cy,cz
 	real(kind=8), allocatable, dimension (:,:,:) :: rho,vx,vy,vz, vx_old, vy_old, vz_old
 	real(kind=8), allocatable, dimension (:,:,:) :: Force_x, Force_y, Force_z
  	real(kind=8), allocatable, dimension (:,:,:,:) :: f,feq
  	real(kind=8), allocatable, dimension (:,:,:,:) :: Forcing
  	
  	real(kind=8), allocatable, dimension (:,:,:) :: pressure
  	
	real(kind=8), allocatable, dimension (:,:,:) :: Momentum  	
	  
end module arrays
  
