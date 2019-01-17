subroutine LB_Units

	use arrays
 	use global_numbers
	
  	implicit none
  	
  	integer i, j, h
  	
  	!------- Physical Units ------
  	!dx = (xmax - xmin)/dble(Nx-1)
    	!dy = (ymax - ymin)/dble(Ny-1)
    	!dz = (zmax - zmin)/dble(Nz-1)
    
	!--------Lattice Units--------
    	dx=one
    	dy=one
    	dz=one
	print*, '-------------------------------------------------------------------------'
	print*, '------------------------ SPACE IN LATTICE UNITS -------------------------'
	print*, '-TO CHANGE THIS GO TO LB_Units.f90 AND USE dx = (xmax - xmin)/dble(Nx-1)-'

    
    	do h=1,Nz
    		do j=1,Ny
    			do i=1, Nx
           			x(i,j,h) = dble(i-1)*dx + xmin
           			y(i,j,h) = dble(j-1)*dy + ymin
           			z(i,j,h) = dble(h-1)*dz + zmin
           		end do
        	end do
    	end do
	!nu_dim is an input by te user
	!dt = third*(tau-half)* (dx**2)/nu_dim	
	dt=one
	print*, '---------------------------------------------------------------------------------'
	print*, '------------------------------TIME  IN LATTICE UNITS-----------------------------'
	print*, '-TO CHANGE THIS GO TO LB_Units.f90 AND USE dt = third*(tau-half)* (dx**2)/nu_dim-'


	Nt = t_final/dt 
  	
  	!Cu= dx/dt
  	Cu=one 
  	print*, '---------Results in LATTICE UNITS, go to LB_Units and use Cu= dx/dt -------------'

  	every_0D = int(every_0Dt/dt)	!This three lines, so it calls----------------
    	every_1D = int(every_1Dt/dt) 	!the Save-subroutines, at the-----------------
    	every_2D = int(every_2Dt/dt)	!same time t, no matters the resolution.------
end subroutine LB_Units
