subroutine ShanChenForce()
	use arrays
  	use global_numbers

  	implicit none

  	integer i,j,h,k
  	integer ii,jj,hh
  	integer ip, il, jp, jl, hp, hl
	real (kind = 8) iRho_0
	
	! ----------------------------------------------------------------------------------
	rho_0 = 1.0d0 !reference density for pseudopotential (usually set to one)
	iRho_0 = one / rho_0
	! ----------------------------------------------------------------------------------


	do h=1, Nz       
		do j=1, Ny
			do i=1, Nx
				ip = mod(i+Nx,Nx)+1
				il = mod(i+Nx-2,Nx)+1
				jp = mod(j+Ny,Ny)+1
				jl = mod(j+Ny-2,Ny)+1
				hp = mod(h+Nz,Nz)+1
				hl = mod(h+Nz-2,Nz)+1


				Force_x(i,j,h)= w(1)*((one-exp(-(rho(ip,j,h)*iRho_0))))-w(2)*((one-exp(-(rho(il,j,h)*iRho_0))))+&
					w(7)*((one-exp(-(rho(ip,jp,h)*iRho_0))))- w(8)*((one-exp(-(rho(il,jl,h)*iRho_0)))) + &
					w(9)*((one-exp(-(rho(ip,j,hp)*iRho_0)))) - w(10)*((one-exp(-(rho(il,j,hl)*iRho_0)))) + &
					w(13)*((one-exp(-(rho(ip,jl,h)*iRho_0)))) - w(14)*((one-exp(-(rho(il,jp,h)*iRho_0)))) + &
					w(15)*((one-exp(-(rho(ip,j,hl)*iRho_0)))) - w(16)*((one-exp(-(rho(il,j,hp)*iRho_0))))

				Force_y(i,j,h)= w(3)*(one-exp(-(rho(i,jp,h)*iRho_0)))-w(4)*(one-exp(-(rho(i,jl,h)*iRho_0))) + &
					w(7)*(one-exp(-(rho(ip,jp,h)*iRho_0))) - w(8)*(one-exp(-(rho(il,jl,h)*iRho_0))) + &
					w(11)*(one-exp(-(rho(i,jp,hp)*iRho_0))) - w(12)*(one-exp(-(rho(i,jl,hl)*iRho_0))) - &
					w(13)*(one-exp(-(rho(ip,jl,h)*iRho_0))) + w(14)*(one-exp(-(rho(il,jp,h)*iRho_0))) + &
					w(17)*(one-exp(-(rho(i,jp,hl)*iRho_0))) - w(18)*(one-exp(-(rho(i,jl,hp)*iRho_0)))

				Force_z(i,j,h)= w(5)*(one-exp(-(rho(i,j,hp)*iRho_0))) - w(6)*(one-exp(-(rho(i,j,hl)*iRho_0))) + &
					w(9)*(one-exp(-(rho(ip,j,hp)*iRho_0))) - w(10)*(one-exp(-(rho(il,j,hl)*iRho_0))) + &
					w(11)*(one-exp(-(rho(i,jp,hp)*iRho_0))) - w(12)*(one-exp(-(rho(i,jl,hl)*iRho_0))) - &
					w(15)*(one-exp(-(rho(ip,j,hl)*iRho_0))) + w(16)*(one-exp(-(rho(il,j,hp)*iRho_0))) - &
					w(17)*(one-exp(-(rho(i,jp,hl)*iRho_0))) + w(18)*(one-exp(-(rho(i,jl,hp)*iRho_0)))



				Force_x(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_x(i,j,h))
				Force_y(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_y(i,j,h))
				Force_z(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_z(i,j,h))
			end do
		end do
	end do
	!----------------------------------------------------------------------------------------------------------
	!---------------------------------------------- BOUNDARIES ------------------------------------------------
	!----------------------------------------------------------------------------------------------------------
	do j=1, Ny
			do i=1, Nx
				ip = mod(i+Nx,Nx)+1
				il = mod(i+Nx-2,Nx)+1
				jp = mod(j+Ny,Ny)+1
				jl = mod(j+Ny-2,Ny)+1
				
				h=1
				hp = mod(h+Nz,Nz)+1
				Force_x(i,j,h)= w(1)*((one-exp(-(rho(ip,j,h)*iRho_0))))-w(2)*((one-exp(-(rho(il,j,h)*iRho_0))))+&
					w(7)*((one-exp(-(rho(ip,jp,h)*iRho_0))))- w(8)*((one-exp(-(rho(il,jl,h)*iRho_0)))) + &
					w(9)*((one-exp(-(rho(ip,j,hp)*iRho_0)))) - w(10)*((one-exp(-(rho_l*iRho_0)))) + &
					w(13)*((one-exp(-(rho(ip,jl,h)*iRho_0)))) - w(14)*((one-exp(-(rho(il,jp,h)*iRho_0)))) + &
					w(15)*((one-exp(-(rho_l*iRho_0)))) - w(16)*((one-exp(-(rho(il,j,hp)*iRho_0))))

				Force_y(i,j,h)= w(3)*(one-exp(-(rho(i,jp,h)*iRho_0)))-w(4)*(one-exp(-(rho(i,jl,h)*iRho_0))) + &
					w(7)*(one-exp(-(rho(ip,jp,h)*iRho_0))) - w(8)*(one-exp(-(rho(il,jl,h)*iRho_0))) + &
					w(11)*(one-exp(-(rho(i,jp,hp)*iRho_0))) - w(12)*(one-exp(-(rho_l*iRho_0))) - &
					w(13)*(one-exp(-(rho(ip,jl,h)*iRho_0))) + w(14)*(one-exp(-(rho(il,jp,h)*iRho_0))) + &
					w(17)*(one-exp(-(rho_l*iRho_0))) - w(18)*(one-exp(-(rho(i,jl,hp)*iRho_0)))

				Force_z(i,j,h)= w(5)*(one-exp(-(rho(i,j,hp)*iRho_0))) - w(6)*(one-exp(-(rho_l*iRho_0))) + &
					w(9)*(one-exp(-(rho(ip,j,hp)*iRho_0))) - w(10)*(one-exp(-(rho_l*iRho_0))) + &
					w(11)*(one-exp(-(rho(i,jp,hp)*iRho_0))) - w(12)*(one-exp(-(rho_l*iRho_0))) - &
					w(15)*(one-exp(-(rho_l*iRho_0))) + w(16)*(one-exp(-(rho(il,j,hp)*iRho_0))) - &
					w(17)*(one-exp(-(rho_l*iRho_0))) + w(18)*(one-exp(-(rho(i,jl,hp)*iRho_0)))

				Force_x(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_x(i,j,h))
				Force_y(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_y(i,j,h))
				Force_z(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_z(i,j,h))
				
				h=Nz
				hl = mod(h+Nz-2,Nz)+1
				Force_x(i,j,h)= w(1)*((one-exp(-(rho(ip,j,h)*iRho_0))))-w(2)*((one-exp(-(rho(il,j,h)*iRho_0))))+&
					w(7)*((one-exp(-(rho(ip,jp,h)*iRho_0))))- w(8)*((one-exp(-(rho(il,jl,h)*iRho_0)))) + &
					w(9)*((one-exp(-(rho_g*iRho_0)))) - w(10)*((one-exp(-(rho(il,j,hl)*iRho_0)))) + &
					w(13)*((one-exp(-(rho(ip,jl,h)*iRho_0)))) - w(14)*((one-exp(-(rho(il,jp,h)*iRho_0)))) + &
					w(15)*((one-exp(-(rho(ip,j,hl)*iRho_0)))) - w(16)*((one-exp(-(rho_g*iRho_0))))

				Force_y(i,j,h)= w(3)*(one-exp(-(rho(i,jp,h)*iRho_0)))-w(4)*(one-exp(-(rho(i,jl,h)*iRho_0))) + &
					w(7)*(one-exp(-(rho(ip,jp,h)*iRho_0))) - w(8)*(one-exp(-(rho(il,jl,h)*iRho_0))) + &
					w(11)*(one-exp(-(rho_g*iRho_0))) - w(12)*(one-exp(-(rho(i,jl,hl)*iRho_0))) - &
					w(13)*(one-exp(-(rho(ip,jl,h)*iRho_0))) + w(14)*(one-exp(-(rho(il,jp,h)*iRho_0))) + &
					w(17)*(one-exp(-(rho(i,jp,hl)*iRho_0))) - w(18)*(one-exp(-(rho_g*iRho_0)))

				Force_z(i,j,h)= w(5)*(one-exp(-(rho_g*iRho_0))) - w(6)*(one-exp(-(rho(i,j,hl)*iRho_0))) + &
					w(9)*(one-exp(-(rho_g*iRho_0))) - w(10)*(one-exp(-(rho(il,j,hl)*iRho_0))) + &
					w(11)*(one-exp(-(rho_g*iRho_0))) - w(12)*(one-exp(-(rho(i,jl,hl)*iRho_0))) - &
					w(15)*(one-exp(-(rho(ip,j,hl)*iRho_0))) + w(16)*(one-exp(-(rho_g*iRho_0))) - &
					w(17)*(one-exp(-(rho(i,jp,hl)*iRho_0))) + w(18)*(one-exp(-(rho_g*iRho_0)))

				Force_x(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_x(i,j,h))
				Force_y(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_y(i,j,h))
				Force_z(i,j,h)= (-G*(rho_0**2*(one-exp(-(rho(i,j,h)*iRho_0))))*Force_z(i,j,h))

			end do
		end do


end subroutine





