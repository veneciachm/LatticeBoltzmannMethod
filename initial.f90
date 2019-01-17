subroutine initial

	use arrays
 	use global_numbers
	
  	implicit none
  
	integer i,j,h,k, Ny2
	real(kind=8) disp
	
	real(kind=8) den, ux, uy, uz, u2
	
	! -------------------------------   NUMBERS   ---------------------------------------
  	zero  = 0.0D0
  	third = 1.0D0/3.0D0   
  	half  = 0.5D0
  	one   = 1.0D0
  	two   = 2.0D0
  	four  = 4.0D0
  	three = 3.0D0
  	pi = four*atan(one)
  	Ny2 = Ny/2
	! ----------------------------------------------------------------------------------
	! ------------------------------ Set up the grid -------------------------------------
	call memory
    	call LB_Units 
	! ----------------------------------------------------------------------------------
    	!------------------------------ Initial Conditions ------------------------------------
	if (initial_data.eq.'Initial_TanWave') then
  		do j=1, Ny
  			disp = real(x0) + A_wave*sin(two*pi*real(j)/Wave_len)
  			do h=1,Nz
				rho(:,j,h) = A_tan * tanh (-real(h-disp)/(two*wdh)) + b
			end do
		end do
	
  	else
    	print *, 'Those IC are not available'
    	stop
  	end if
	! ----------------------------------------------------------------------------------
	! ------------------------------     LBM D3Q19 stuff     ---------------------------
	! Setting weigths
  	w(0) = 1.0d0/3.0d0
 	w(1) = 1.0d0/18.0d0
 	w(2) = 1.0d0/18.0d0
 	w(3) = 1.0d0/18.0d0
 	w(4) = 1.0d0/18.0d0
 	w(5) = 1.0d0/18.0d0
 	w(6) = 1.0d0/18.0d0
  	w(7) = 1.0d0/36.0d0
  	w(8) = 1.0d0/36.0d0
  	w(9) = 1.0d0/36.0d0
  	w(10) = 1.0d0/36.0d0
  	w(11) = 1.0d0/36.0d0
  	w(12) = 1.0d0/36.0d0
  	w(13) = 1.0d0/36.0d0
  	w(14) = 1.0d0/36.0d0
  	w(15) = 1.0d0/36.0d0
  	w(16) = 1.0d0/36.0d0
  	w(17) = 1.0d0/36.0d0
  	w(18) = 1.0d0/36.0d0

	! Setting cell velocities
  	cx(0) = zero
  	cx(1) = one
  	cx(2) = -one
  	cx(3) = zero
  	cx(4) = zero
  	cx(5) = zero
  	cx(6) = zero
  	cx(7) = one
  	cx(8) = -one
	cx(9) = one
  	cx(10) = -one
  	cx(11) = zero
  	cx(12) = zero
  	cx(13) = one
  	cx(14) = -one
  	cx(15) = one
  	cx(16) = -one
  	cx(17) = zero
  	cx(18) = zero

  	cy(0) = zero
  	cy(1) = zero
  	cy(2) = zero
  	cy(3) = one
  	cy(4) = -one
  	cy(5) = zero
  	cy(6) = zero
  	cy(7) = one
  	cy(8) = -one
	cy(9) = zero
	cy(10) = zero
	cy(11) = one
	cy(12) = -one
	cy(13) = -one
	cy(14) = one
	cy(15) = zero
	cy(16) = zero
	cy(17) = one
	cy(18) = -one
	
	cz(0) = zero
	cz(1) = zero
	cz(2) = zero
	cz(3) = zero
	cz(4) = zero
	cz(5) = one
	cz(6) = -one
	cz(7) = zero
	cz(8) = zero
	cz(9) = one
	cz(10) = -one
	cz(11) = one
	cz(12) = -one
	cz(13) = zero
	cz(14) = zero
	cz(15) = -one
	cz(16) = one
	cz(17) = -one
	cz(18) = one

	do h=1, Nz
                do j=1, Ny
                        do i=1, Nx
                                den = rho(i,j,h)
                                ux = vx(i,j,h)
                                uy = vy(i,j,h)
                                uz = vz(i,j,h)
                                u2 = vx(i,j,h)**2+vy(i,j,h)**2+vz(i,j,h)**2

                                feq(0,i,j,h) = den*w(0) * ( one - 1.5d0*u2)
                                feq(1,i,j,h) = den*w(1) * ( one + three*(ux) + 4.5d0*(ux)**2 - 1.5d0*u2)
                                feq(2,i,j,h) = den*w(2) * ( one + three*(-ux) + 4.5d0*(-ux)**2 - 1.5d0*u2)
                                feq(3,i,j,h) = den*w(3) * ( one + three*(uy) + 4.5d0*(uy)**2 - 1.5d0*u2)
                                feq(4,i,j,h) = den*w(4) * ( one + three*(-uy) + 4.5d0*(-uy)**2 - 1.5d0*u2)
                                feq(5,i,j,h) = den*w(5) * ( one + three*(uz) + 4.5d0*(uz)**2 - 1.5d0*u2)
                                feq(6,i,j,h) = den*w(6) * ( one + three*(-uz) + 4.5d0*(-uz)**2 - 1.5d0*u2)
                                feq(7,i,j,h) = den*w(7) * ( one + three*(ux+uy) + 4.5d0*(ux+uy)**2 - 1.5d0*u2)
                                feq(8,i,j,h) = den*w(8) * ( one + three*(-ux-uy) + 4.5d0*(-ux-uy)**2 - 1.5d0*u2)
                                feq(9,i,j,h) = den*w(9) * ( one + three*(ux+uz) + 4.5d0*(ux+uz)**2 - 1.5d0*u2)
                                feq(10,i,j,h) = den*w(10) * ( one + three*(-ux-uz) + 4.5d0*(-ux-uz)**2 - 1.5d0*u2)
                                feq(11,i,j,h) = den*w(11) * ( one + three*(uy+uz) + 4.5d0*(uy+uz)**2 - 1.5d0*u2)
                                feq(12,i,j,h) = den*w(12) * ( one + three*(-uy-uz) + 4.5d0*(-uy-uz)**2 - 1.5d0*u2)
                                feq(13,i,j,h) = den*w(13) * ( one + three*(ux-uy) + 4.5d0*(ux-uy)**2 - 1.5d0*u2)
                                feq(14,i,j,h) = den*w(14) * ( one + three*(-ux+uy) + 4.5d0*(-ux+uy)**2 - 1.5d0*u2)
                                feq(15,i,j,h) = den*w(15) * ( one + three*(ux-uz) + 4.5d0*(ux-uz)**2 - 1.5d0*u2)
                                feq(16,i,j,h) = den*w(16) * ( one + three*(-ux+uz) + 4.5d0*(-ux+uz)**2 - 1.5d0*u2)
                                feq(17,i,j,h) = den*w(17) * ( one + three*(uy-uz) + 4.5d0*(uy-uz)**2 - 1.5d0*u2)
                                feq(18,i,j,h) = den*w(18) * ( one + three*(-uy+uz) + 4.5d0*(-uy+uz)**2 - 1.5d0*u2)
                                do k=0, 18
                                        f(k,i,j,h) = feq (k,i,j,h)
                                        TotalMass = TotalMass + f(k,i,j,h)
                                end do
                        end do
                end do
        end do
        call hydro_variables

	! N-S equation parameters
  	nu = third*(tau-half)
    
	! ----------------------------------------------------------------------------------
	! ------------------------- Checking some important parameters --------------------- 
	print *, '------------------------------------------------------------------'
    	print *, 'dt=',dt, 'dx=', dx, 'dy=', dy,'dz=', dz, 'Nt=', Nt
    	print *, '------------------------------------------------------------------'
    	print *, 'nu(dimensionless)=', nu
    	print *, '------------------------------------------------------------------'
    	print *, 'tau=', tau, '(DIMENSIONLESS TAU)(≥1/2) sufficent stability condition for BGK operator'
    	print *, '------------------------------------------------------------------'  
	! ----------------------------------------------------------------------------------
	! ----------------------------------------------------------------------------------

! -------------------------------   END   ------------------------------------------
end subroutine initial
