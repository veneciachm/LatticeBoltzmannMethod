subroutine hydro_variables

	use arrays
  	use global_numbers

  	implicit none

  	real(kind=8) qn_x, qn_y, qn_z
  	integer i,j,h,k
  	real (kind = 8) iSix
	real(kind=8), dimension(1:Nx,1:Ny,1:Nz) :: irho
  	


	vx_old=vx
	vy_old=vy
	vz_old=vz

	iSix = 1.0d0/6.0d0
	
	steady_x = 0.0d0
	steady_y = 0.0d0
	steady_z = 0.0d0

	qn_x=0.0d0
	qn_y=0.0d0
	qn_z=0.0d0

	! ----------------------------------------------------------------
	! --------------------------   RHO   -----------------------------
	! ----------------------------------------------------------------
  	do h=1,Nz
    		do j=1,Ny
    			do i=1, Nx
        			rho(i,j,h) = f(0,i,j,h) + f(1,i,j,h) + f(2,i,j,h) + f(3,i,j,h) + f(4,i,j,h) + &
        				f(5,i,j,h) + f(6,i,j,h) + f(7,i,j,h) + f(8,i,j,h) + f(9,i,j,h) + f(10,i,j,h)  + & 
        				f(11,i,j,h) + f(12,i,j,h) + f(13,i,j,h) + f(14,i,j,h) + f(15,i,j,h) + f(16,i,j,h)  + &
        				f(17,i,j,h) + f(18,i,j,h) 
      				irho(i,j,h) = one / rho(i,j,h)
      			end do
    		end do
  	end do
  	! ----------------------------------------------------------------
	! ----------------------------------------------------------------

	! ----------------------------------------------------------------
	! ------------------------- Pressure   ---------------------------
	! ----------------------------------------------------------------
  	do h=1,Nz
    		do j=1,Ny
    			do i=1, Nx
				pressure (i,j,h) = third * rho (i,j,h) + (G * (rho_0*(one-exp(-(rho(i,j,h)/rho_0))))**2) * iSix
			end do
		end do
	end do
	! ----------------------------------------------------------------
	! ----------------------------------------------------------------

	! ----------------------------------------------------------------
	! ------------------------  vx, vy, vz ---------------------------
	! ----------------------------------------------------------------
	do h=1,Nz
    		do j=1,Ny
    			do i=1, Nx        				
      				vx(i,j,h) = ((f(1,i,j,h) - f(2,i,j,h) + &
        				f(7,i,j,h) - f(8,i,j,h) + f(9,i,j,h) - f(10,i,j,h)  + & 
        				f(13,i,j,h) - f(14,i,j,h) + f(15,i,j,h) - f(16,i,j,h)) + &
      					half * Force_x(i,j,h) ) * irho(i,j,h)	! Normalized
      				vy(i,j,h) = ((f(3,i,j,h) - f(4,i,j,h) + &
        				f(7,i,j,h) - f(8,i,j,h) + & 
        				f(11,i,j,h) - f(12,i,j,h) - f(13,i,j,h) + f(14,i,j,h) + &
        				f(17,i,j,h) - f(18,i,j,h)) + &
      					half * Force_y(i,j,h) ) * irho(i,j,h)	! Normalized
      				vz(i,j,h) = ((f(5,i,j,h) - f(6,i,j,h) + f(9,i,j,h) - f(10,i,j,h)  + & 
        				f(11,i,j,h) - f(12,i,j,h) - f(15,i,j,h) + f(16,i,j,h)  - &
        				f(17,i,j,h) + f(18,i,j,h)) + &
      					half * Force_z(i,j,h) ) * irho(i,j,h)	! Normalized
      			end do
    		end do
  	end do
  	! ----------------------------------------------------------------
	! ----------------------------------------------------------------
	! ----------------------------------------------------------------
	! ------------------------ Steadiness  ---------------------------
	! ----------------------------------------------------------------
	do h=1,Nz
    		do j=1,Ny
    			do i=1, Nx        				
				steady_x = steady_x + abs (vx(i,j,h)-vx_old(i,j,h))**2
				steady_y = steady_y + abs (vy(i,j,h)-vy_old(i,j,h))**2
				steady_z = steady_z + abs (vz(i,j,h)-vz_old(i,j,h))**2
				qn_x=qn_x + vx_old(i,j,h)**2
				qn_y=qn_y + vy_old(i,j,h)**2
				qn_z=qn_z + vz_old(i,j,h)**2
			end do
		end do 
	end do 
	
	ep_x = sqrt(steady_x/qn_x)
	ep_y = sqrt(steady_y/qn_y)
	ep_z = sqrt(steady_z/qn_z)
	
	
	
	if (.NOT. flag_steady ) then
		if ((ep_x.le.1.0e-6) .and. (ep_y.le.1.0e-6)  ) then 
			print*, 'We have reached the steady state, at t = ', t
			flag_steady = .true.
		end if
	end if 
	! ----------------------------------------------------------------
	! ----------------------------------------------------------------

end subroutine
