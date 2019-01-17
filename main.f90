!-----------------------------------------------------------------------------------------
!-------------------------*************************************---------------------------
!-------------------------***                               ***---------------------------
!-------------------------***  3D LATTICE BOLTZMANN METHOD  ***---------------------------
!-------------------------***          Shan Chen            ***---------------------------
!-------------------------***            D3Q19              ***---------------------------
!-------------------------***       Venecia Chavez          ***---------------------------
!-------------------------***            2018               ***---------------------------
!-------------------------***                               ***---------------------------
!-------------------------*************************************---------------------------
!-----------------------------------------------------------------------------------------
program main
	use global_numbers
	implicit none
	
	integer Nxx,Nyy,Nzz 

	! ---   GET PARAMETERS   ---
	Namelist /CW_Input/ res_num, &
                      initial_data, &
                      xmin, xmax, &
                      ymin, ymax, &
                      zmin, zmax, &
                      Nxx, Nyy, Nzz, &
                      every_0Dt, every_1Dt, every_2Dt, &
                      method,t_final, tau, &
                      G, rho_l, rho_g, Sanity_check, A_tan, wdh, b, x0, A_wave, Wave_len

	! Default parameter values
	res_num     = 1
   	xmin        = -1.0
   	xmax        = 1.0
    	ymin        = -1.
    	ymax        = 1.
    	zmin        = -1.
    	zmax        = 1.
    	Nxx         = 100
    	Nyy         = 100
    	Nzz         = 100   
    	every_0Dt   = 1
    	every_1Dt   = 10
    	every_2Dt   = 10
    	t_final     = 1.0

    	open (3, file='iii.par', status = 'old' )
    	read (3, nml = CW_Input)
    	close(3)

	! ---   RESOLUTION   ---
	Nx = 2**(res_num-1)*Nxx
	Ny = 2**(res_num-1)*Nyy
	Nz = 2**(res_num-1)*Nzz
    
    

	!-------------------   Some info to the screen   --------------------
  	print *, '-----------------------------------------------------------'
  	print *, 'Computational Domain  ', 'x [',xmin,',',xmax,']'
  	print *, 'Computational Domain  ', 'y [',ymin,',',ymax,']'
  	print *, 'Computational Domain  ', 'z [',zmin,',',zmax,']'
  	print *, '-----------------------------------------------------------'

  	!-----------------HERE WHERE THE MAGIC HAPPENS-------------------
  	!----------------------------------------------------------------
  	call evolve
  	!----------------------------------------------------------------
  	!----------------------------------------------------------------

  	print *
  	print *, 'PROGRAM CapillaryWaves-LBM/ShanChen-3D HAS FINISHED'
  	print *
  	!-----------------HERE WHERE THE MAGIC ENDS----------------------
  	!----------------------------------------------------------------
end program main

