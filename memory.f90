subroutine memory( )

  	use arrays
  	use global_numbers

  	implicit none

  	allocate(x(1:Nx,1:Ny,1:Nz),y(1:Nx,1:Ny,1:Nz), z(1:Nx,1:Ny,1:Nz))
  	allocate(rho(1:Nx,1:Ny,1:Nz),vx(1:Nx,1:Ny,1:Nz),vy(1:Nx,1:Ny,1:Nz), vz(1:Nx,1:Ny,1:Nz))
	allocate (vx_old(1:Nx,1:Ny,1:Nz), vy_old(1:Nx,1:Ny,1:Nz), vz_old(1:Nx,1:Ny,1:Nz))
	allocate (force_x(1:Nx,1:Ny,1:Nz), force_y(1:Nx,1:Ny,1:Nz), force_z(1:Nx,1:Ny,1:Nz))
	allocate (pressure(1:Nx,1:Ny,1:Nz))

	! Distribution function and eq. distribution function
  	allocate(f(0:18,1:Nx,1:Ny,1:Nz),feq(0:18,1:Nx,1:Ny,1:Nz))
  	allocate(Forcing(0:18,1:Nx,1:Ny,1:Nz))

	! Flux cell velocities and weights
  	allocate(w(0:18),cx(0:18),cy(0:18), cz(0:18))

	allocate(Momentum(1:Nx,1:Ny,1:Nz))

  end subroutine memory

