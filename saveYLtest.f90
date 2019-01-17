!-----------------------------------------------------------------------------------------
!------------------ SAVES THE VALUES OF YOUNG-LAPLACE TEST FOR EACH TIME -----------------
!-----------------------------------------------------------------------------------------
subroutine saveYLtest(rho_g_aver, rho_l_aver, rho_ratio,delta_pressure, R, gamma,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

	real (kind=8) rho_g_aver, rho_l_aver, rho_ratio,delta_pressure, R, gamma
  	character(len=256) :: filename

  	integer i,j,h,first_index

  	if (res_num.eq.1) then
    		filename = trim('YLTest')  // trim(initial_data) //'_1.t'
  	else if (res_num.eq.2) then
    		filename = trim('YLTest')  // trim(initial_data) //'_2.t'
  	else if (res_num.eq.3) then
    		filename = trim('YLTest')  // trim(initial_data) //'_3.t'
  	else if (res_num.eq.4) then
    		filename = trim('YLTest')  // trim(initial_data) //'_4.t'
  	else if (res_num.eq.5) then
    		filename = trim('YLTest')  // trim(initial_data) //'_5.t'
  	end if

  	if (first_index.eq.0) then
     		filestatus = 'replace'
  	else
     		filestatus = 'old'
  	end if



	! ---------------------------------   Saving data   --------------------------

  	if (filestatus=='replace') then
	     	open(1,file=filename,form='formatted',status=filestatus)
  		write(1,*) '#Time//rho_g//rho_l//Density_ratio//DeltaPressure//Droplet_Radius//SurfaceTension'
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
    
    	write(1,*) t, rho_g_aver, rho_l_aver, rho_ratio,delta_pressure, R, gamma
  	close(1)

end subroutine

