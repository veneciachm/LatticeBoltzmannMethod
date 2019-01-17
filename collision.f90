subroutine collision

	use arrays
  	use global_numbers

  	implicit none

  	real(kind=8), dimension(0:18,0:Nx,0:Ny,0:Nz) :: ff
  	integer i,j,h,k
	integer ii,jj,hh
	integer il,ip,jl,jp,hl,hp
	real (kind=8) omega, omega1, omega2
	real(kind=8) den, ux, uy, uz, u2

	omega=one/tau
	omega2=(one-half*omega)
	omega1=one-omega

	Momentum = zero
        TotalMass = zero 
	
	call ShanChenForce()
	
	do h=1, Nz
		do j=1, Ny
			do i=1, Nx
				den = rho(i,j,h)
				ux = vx(i,j,h)
				uy = vy(i,j,h)
				uz = vz(i,j,h)
				u2 = (vx(i,j,h)**2+vy(i,j,h)**2+vz(i,j,h)**2)*1.50d0
    				
    				feq(0,i,j,h) = den*w(0) * ( one - u2)
    				feq(1,i,j,h) = den*w(1) * ( one + three*(ux) + 4.5d0*(ux)**2 - u2)
    				feq(2,i,j,h) = den*w(2) * ( one + three*(-ux) + 4.5d0*(-ux)**2 - u2)
    				feq(3,i,j,h) = den*w(3) * ( one + three*(uy) + 4.5d0*(uy)**2 - u2)
				feq(4,i,j,h) = den*w(4) * ( one + three*(-uy) + 4.5d0*(-uy)**2 - u2)
    				feq(5,i,j,h) = den*w(5) * ( one + three*(uz) + 4.5d0*(uz)**2 - u2)
    				feq(6,i,j,h) = den*w(6) * ( one + three*(-uz) + 4.5d0*(-uz)**2 - u2)
    				feq(7,i,j,h) = den*w(7) * ( one + three*(ux+uy) + 4.5d0*(ux+uy)**2 - u2)
    				feq(8,i,j,h) = den*w(8) * ( one + three*(-ux-uy) + 4.5d0*(-ux-uy)**2 - u2)
   	 			feq(9,i,j,h) = den*w(9) * ( one + three*(ux+uz) + 4.5d0*(ux+uz)**2 - u2)
    				feq(10,i,j,h) = den*w(10) * ( one + three*(-ux-uz) + 4.5d0*(-ux-uz)**2 - u2)
    				feq(11,i,j,h) = den*w(11) * ( one + three*(uy+uz) + 4.5d0*(uy+uz)**2 - u2)
    				feq(12,i,j,h) = den*w(12) * ( one + three*(-uy-uz) + 4.5d0*(-uy-uz)**2 - u2)
    				feq(13,i,j,h) = den*w(13) * ( one + three*(ux-uy) + 4.5d0*(ux-uy)**2 - u2)
    				feq(14,i,j,h) = den*w(14) * ( one + three*(-ux+uy) + 4.5d0*(-ux+uy)**2 - u2)    			
    				feq(15,i,j,h) = den*w(15) * ( one + three*(ux-uz) + 4.5d0*(ux-uz)**2 - u2)    			
    				feq(16,i,j,h) = den*w(16) * ( one + three*(-ux+uz) + 4.5d0*(-ux+uz)**2 - u2)
    				feq(17,i,j,h) = den*w(17) * ( one + three*(uy-uz) + 4.5d0*(uy-uz)**2 - u2)
	    			feq(18,i,j,h) = den*w(18) * ( one + three*(-uy+uz) + 4.5d0*(-uy+uz)**2 - u2)
	
				ip = mod(i+Nx,Nx)+1
				il = mod(i+Nx-2,Nx)+1
				jp = mod(j+Ny,Ny)+1
				jl = mod(j+Ny-2,Ny)+1
				hp = mod(h+Nz,Nz)+1
				hl = mod(h+Nz-2,Nz)+1
					
			
				!------------------------ Here we obtain the Forcing term ----------------------------	
				Forcing (0,i,j,h) = w(0) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )
					
				Forcing (1,i,j,h) = w(1) * ( (three * (one-vx(i,j,h)) + &
							9.0d0 * ( vx(i,j,h)  ) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )
					
				Forcing (2,i,j,h) = w(2) * ( (three * (-one-vx(i,j,h)) - &
							9.0d0 * ( -vx(i,j,h)  ) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )

				Forcing (3,i,j,h) = w(3) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (one-vy(i,j,h)) + &
							9.0d0 *  vy(i,j,h)  )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )

				Forcing (4,i,j,h) = w(4) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-one-vy(i,j,h)) - &
							9.0d0 * ( -vy(i,j,h) ) )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )

				Forcing (5,i,j,h) = w(5) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (one-vz(i,j,h)) + &
							9.0d0 * vz(i,j,h) )*Force_z(i,j,h) )
							
				Forcing (6,i,j,h) = w(6) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (-one-vz(i,j,h)) - &
							9.0d0 * ( -vz(i,j,h)  ) )*Force_z(i,j,h) )
					
				Forcing (7,i,j,h) = w(7) * ( (three * (one-vx(i,j,h)) + &
							9.0d0 * ( vx(i,j,h) + vy(i,j,h) ) )*Force_x(i,j,h) + &
							(three * (one-vy(i,j,h)) + &
							9.0d0 * ( vx(i,j,h) + vy(i,j,h) ) )*Force_y(i,j,h) + &
										(three * (-vz(i,j,h)) )*Force_z(i,j,h) )
												
				Forcing (8,i,j,h) = w(8) * ( (three * (-one-vx(i,j,h)) - &
							9.0d0 * (-vx(i,j,h) -vy(i,j,h) ) )*Force_x(i,j,h) + &
							(three * (-one-vy(i,j,h)) - &
							9.0d0 * ( -vx(i,j,h) - vy(i,j,h) ) )*Force_y(i,j,h) + &
										(three * (-vz(i,j,h)) )*Force_z(i,j,h) )

				Forcing (9,i,j,h) = w(9) * ( (three * (one-vx(i,j,h)) + &
							9.0d0 * ( vx(i,j,h) + vz(i,j,h)  ) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (one-vz(i,j,h)) + &
							9.0d0 * ( vx(i,j,h) + vz(i,j,h)  ) )*Force_z(i,j,h) )
					
				Forcing (10,i,j,h) = w(10) * ( (three * (-one-vx(i,j,h)) - &
							9.0d0 * ( -vx(i,j,h) - vz(i,j,h)  ) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (-one-vz(i,j,h)) - &
							9.0d0 * ( -vx(i,j,h) - vz(i,j,h)  ) )*Force_z(i,j,h) )
					
				Forcing (11,i,j,h) = w(11) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (one-vy(i,j,h)) + &
							9.0d0 * ( vy(i,j,h) + vz(i,j,h)  ) )*Force_y(i,j,h) + &
							(three * (one-vz(i,j,h)) + &
							9.0d0 * ( vy(i,j,h) + vz(i,j,h)  ) )*Force_z(i,j,h) )

				Forcing (12,i,j,h) = w(12) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-one-vy(i,j,h)) - &
							9.0d0 * ( -vy(i,j,h) - vz(i,j,h)  ) )*Force_y(i,j,h) + &
							(three * (-one-vz(i,j,h)) - &
							9.0d0 * ( -vy(i,j,h) - vz(i,j,h)  ) )*Force_z(i,j,h) )
	
				Forcing (13,i,j,h) = w(13) * ( (three * (one-vx(i,j,h)) + &
							9.0d0 * ( vx(i,j,h) -vy(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-one-vy(i,j,h)) - &
							9.0d0 * ( vx(i,j,h) - vy(i,j,h) ) )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )
					
				Forcing (14,i,j,h) = w(14) * ( (three * (-one-vx(i,j,h)) - &
							9.0d0 * ( -vx(i,j,h) + vy(i,j,h) ) )*Force_x(i,j,h) + &
							(three * (one-vy(i,j,h)) + &
							9.0d0 * ( -vx(i,j,h) + vy(i,j,h) ) )*Force_y(i,j,h) + &
							(three * (-vz(i,j,h)) )*Force_z(i,j,h) )

				Forcing (15,i,j,h) = w(15) * ( (three * (one-vx(i,j,h)) + &
							9.0d0 * ( vx(i,j,h) - vz(i,j,h)  ) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (-one-vz(i,j,h)) - &
							9.0d0 * ( vx(i,j,h) - vz(i,j,h)  ) )*Force_z(i,j,h) )
				
				Forcing (16,i,j,h) = w(16) * ( (three * (-one-vx(i,j,h)) - &
							9.0d0 * ( -vx(i,j,h) + vz(i,j,h)  ) )*Force_x(i,j,h) + &
							(three * (-vy(i,j,h)) )*Force_y(i,j,h) + &
							(three * (one-vz(i,j,h)) + &
										9.0d0 * ( -vx(i,j,h) + vz(i,j,h)  )  )*Force_z(i,j,h) )

				Forcing (17,i,j,h) = w(17) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (one-vy(i,j,h)) + &
							9.0d0 * ( vy(i,j,h) - vz(i,j,h)  ) )*Force_y(i,j,h) + &
							(three * (-one-vz(i,j,h)) - &
							9.0d0 * ( vy(i,j,h) - vz(i,j,h)  ) )*Force_z(i,j,h) )

				Forcing (18,i,j,h) = w(18) * ( (three * (-vx(i,j,h)) )*Force_x(i,j,h) + &
							(three * (-one-vy(i,j,h)) - &
							9.0d0 * ( -vy(i,j,h) + vz(i,j,h)  ) )*Force_y(i,j,h) + &
							(three * (one-vz(i,j,h)) + &
							9.0d0 * ( -vy(i,j,h) + vz(i,j,h)  ) )*Force_z(i,j,h) )
					
				!---------------------- Here we add the Forcing to our system ------------------------
				ff(0,i,j,h) = f(0,i,j,h)*(omega1)+feq(0,i,j,h)*(omega) + omega2*Forcing (0,i,j,h)
				ff(1,ip,j,h) = f(1,i,j,h)*(omega1)+feq(1,i,j,h)*(omega) + omega2*Forcing (1,i,j,h)
				ff(2,il,j,h) = f(2,i,j,h)*(omega1)+feq(2,i,j,h)*(omega) + omega2*Forcing (2,i,j,h)
				ff(3,i,jp,h) = f(3,i,j,h)*(omega1)+feq(3,i,j,h)*(omega) + omega2*Forcing (3,i,j,h)
				ff(4,i,jl,h) = f(4,i,j,h)*(omega1)+feq(4,i,j,h)*(omega) + omega2*Forcing (4,i,j,h)
				ff(5,i,j,hp) = f(5,i,j,h)*(omega1)+feq(5,i,j,h)*(omega) + omega2*Forcing (5,i,j,h)
				ff(6,i,j,hl) = f(6,i,j,h)*(omega1)+feq(6,i,j,h)*(omega) + omega2*Forcing (6,i,j,h)
				ff(7,ip,jp,h) = f(7,i,j,h)*(omega1)+feq(7,i,j,h)*(omega) + omega2*Forcing (7,i,j,h)
				ff(8,il,jl,h) = f(8,i,j,h)*(omega1)+feq(8,i,j,h)*(omega) + omega2*Forcing (8,i,j,h)
				ff(9,ip,j,hp) = f(9,i,j,h)*(omega1)+feq(9,i,j,h)*(omega) + omega2*Forcing (9,i,j,h)
				ff(10,il,j,hl) = f(10,i,j,h)*(omega1)+feq(10,i,j,h)*(omega) + omega2*Forcing (10,i,j,h)
				ff(11,i,jp,hp) = f(11,i,j,h)*(omega1)+feq(11,i,j,h)*(omega) + omega2*Forcing (11,i,j,h)
				ff(12,i,jl,hl) = f(12,i,j,h)*(omega1)+feq(12,i,j,h)*(omega) + omega2*Forcing (12,i,j,h)
				ff(13,ip,jl,h) = f(13,i,j,h)*(omega1)+feq(13,i,j,h)*(omega) + omega2*Forcing (13,i,j,h)
				ff(14,il,jp,h) = f(14,i,j,h)*(omega1)+feq(14,i,j,h)*(omega) + omega2*Forcing (14,i,j,h)
				ff(15,ip,j,hl) = f(15,i,j,h)*(omega1)+feq(15,i,j,h)*(omega) + omega2*Forcing (15,i,j,h)
				ff(16,il,j,hp) = f(16,i,j,h)*(omega1)+feq(16,i,j,h)*(omega) + omega2*Forcing (16,i,j,h)
				ff(17,i,jp,hl) = f(17,i,j,h)*(omega1)+feq(17,i,j,h)*(omega) + omega2*Forcing (17,i,j,h)
				ff(18,i,jl,hp) = f(18,i,j,h)*(omega1)+feq(18,i,j,h)*(omega) + omega2*Forcing (18,i,j,h)

    				
    				!!!!---------------- ONLY DO THIS EVERY CERTAIN AMOUNT OF TIME STEPS -----------------!!!!
    				!if (mod(l,Sanity_check).eq.0) then
    				!	!---------Here we check for NaN's or if the distribution function is negative-------------
    				!	!-----------------------------------------------------------------------------------------
    				!	if (ff(k,i,j,h).lt.0.0d0) then 
					!		print*, 'La función de distribución es negativa' , k, i, j, h, ff(k,i,j,h), t
					!		!For the BGK collision operator, a sufficient stability condition is the non-negativity of all equilibrium populations
					!		!Pag. 130, Krüger 
					!		!stop							
					!	end if
					!	if (feq(k,i,j,h).lt.0.0d0) then 
					!		print*, 'La función de distribución en equilibrio es negativa' , k, i, j, h, feq(k,i,j,h), t
					!		!For the BGK collision operator, a sufficient stability condition is the non-negativity of all equilibrium populations 
					!		!Pag. 130, Krüger 
					!		!stop
					!	end if
					!	if (feq(k,i,j,h).ne.feq(k,i,j,h)) then 
					!		print*, 'we have a NAN at feq in collision', k, i, j, h, feq(k,i,j,h), t
					!		stop		
					!	end if
					!	if (ff(k,i,j,h).ne.ff(k,i,j,h)) then 
					!		print*, 'we have a NAN at f in collision', k, i, j, h, ff(k,i,j,h), t
					!		stop		
					!	end if
					!end if
    				!-----------------------------------------------------------------------------------------
    				!end do
				do k=0, 18
        				Momentum (i,j,h) = Momentum (i,j,h) + Forcing (k,i,j,h)
         				TotalMass = TotalMass + ff(k,i,j,h)
				end do

  				if (Momentum (i,j,h) .gt. 1.0e-8) then
        				print*, 'Mass source not conserved', Momentum(i,j,h)
           				stop
        			end if
  			end do
  		end do
  	end do
	
	if (initial_data.eq.'Initial_TanWave') then
		do j=1, Ny
			do i=1, Nx
			
				ip = mod(i+Nx,Nx)+1
				il = mod(i+Nx-2,Nx)+1
				jp = mod(j+Ny,Ny)+1
				jl = mod(j+Ny-2,Ny)+1
  				!----------------------------------- TOP BOUNDARY ----------------
  				! ------------------------------TOP FACE [bounce back] -----------
    				ff(15,ip,j,Nz) = f(16,i,j,Nz)*(omega1)+feq(16,i,j,Nz)*(omega) + omega2*Forcing (16,i,j,Nz)
    				ff(6,i,j,Nz) = f(5,i,j,Nz)*(omega1)+feq(5,i,j,Nz)*(omega) + omega2*Forcing (5,i,j,Nz)
    				ff(10,il,j,Nz) = f(9,i,j,Nz)*(omega1)+feq(9,i,j,Nz)*(omega) + omega2*Forcing (9,i,j,Nz)
    				ff(17,i,jp,Nz) = f(18,i,j,Nz)*(omega1)+feq(18,i,j,Nz)*(omega) + omega2*Forcing (18,i,j,Nz)
    				ff(12,i,jl,Nz) = f(11,i,j,Nz)*(omega1)+feq(11,i,j,Nz)*(omega) + omega2*Forcing (11,i,j,Nz)
  				!----------------------------------- BOTTOM BOUNDARY ----------------
  				! ------------------------------ BOTTOM FACE [bounce back] -----------
    				ff(9,ip,j,1) = f(10,i,j,1)*(omega1)+feq(10,i,j,1)*(omega) + omega2*Forcing (10,i,j,1)
    				ff(5,i,j,1) = f(6,i,j,1)*(omega1)+feq(6,i,j,1)*(omega) + omega2*Forcing (6,i,j,1)
    				ff(16,il,j,1) = f(15,i,j,1)*(omega1)+feq(15,i,j,1)*(omega) + omega2*Forcing (15,i,j,1)
    				ff(11,i,jp,1) = f(12,i,j,1)*(omega1)+feq(12,i,j,1)*(omega) + omega2*Forcing (12,i,j,1)
    				ff(18,i,jl,1) = f(17,i,j,1)*(omega1)+feq(17,i,j,1)*(omega) + omega2*Forcing (17,i,j,1)
    			end do
    		end do
	end if
 	f=ff
end subroutine
