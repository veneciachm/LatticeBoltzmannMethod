subroutine YoungLaplaceTest ()

	use arrays
  	use global_numbers

  	implicit none
  	integer i,j,h
  	integer y2, z2
  	real (kind = 8) rho_g_aver, rho_l_aver, pressure_g_aver, pressure_l_aver, rho_average, pressure_average
  	real (kind = 8) delta_pressure, gamma, R
  	real (kind = 8) drho, deltaX 
  	real (kind = 8) rho_ratio
  	real (kind = 8) i64
  	
  	
  	y2 = Ny/2
  	z2 = Nz/2
  	rho_g_aver = zero 
  	rho_l_aver = zero
  	pressure_g_aver = zero
  	pressure_l_aver = zero
  	i64 = one / 64.0d0
  	
  	!Average gas pressure in a square box with 4 lattice nodes
    	do h = 1, 4
    		do j = 1, 4
    			do i=1, 4
        			rho_g_aver = rho_g_aver + rho (i,j,h)
        			pressure_g_aver = pressure_g_aver + pressure (i,j,h)
        		end do
        	end do
    	end do
    
    	!Average liquid pressure in a square box with 4 lattice nodes
    	do h=Nz/2-2, Nz/2+1
    		do j = Ny/2-2, Ny/2+1
    			do i = Nx/2-2, Nx/2+1
        			rho_l_aver = rho_l_aver + rho (i,j,h)
        			pressure_l_aver = pressure_l_aver + pressure (i,j,h)
        		end do
        	end do
    	end do
    
    	rho_g_aver = rho_g_aver * i64
  	rho_l_aver = rho_l_aver * i64
  	pressure_g_aver = pressure_g_aver * i64
  	pressure_l_aver = pressure_l_aver * i64
            
	delta_pressure = pressure_l_aver - pressure_g_aver
	rho_average = half*(rho_l_aver - rho_g_aver)
	rho_ratio = rho_l_aver / rho_g_aver
  	
  	do i = 1, Nx/2
  		if (rho(i,y2,z2) .gt. rho_average ) then 
  			drho = rho (i, y2, z2) - rho (i-1, y2, z2) 
  			DeltaX = (rho_average - rho (i-1, y2, z2))/drho
  			R = (half*Nx-i) + (one - DeltaX)
  			exit
  		end if
  	end do
  	
  	gamma = delta_pressure * R * half
  	
  	if (l-1 .eq. zero ) then
  		call saveYLtest(rho_g_aver, rho_l_aver, rho_ratio,delta_pressure, R, gamma, 0 )
  	else if (mod(l,every_1D).eq.0) then
  		call saveYLtest(rho_g_aver, rho_l_aver, rho_ratio,delta_pressure, R, gamma, 1 )
    	end if
  	
end subroutine  	
  	
  	
