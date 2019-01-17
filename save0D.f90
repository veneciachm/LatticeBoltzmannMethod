!-----------------------------------------------------------------------------------------
!---------SAVES THE MAXIMUM AND CENTRAL VALUE OF THE QUIANTITY yval FOR EACH TIME---------
!-----------------------------------------------------------------------------------------
subroutine save0D(yval,base_name,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), dimension(1:Nx,1:Ny,1:Nz), intent(IN) :: yval

  	character(len=256) :: filename

  	integer i,j,h,first_index
  	real(kind=8) min, max, nm1, nm2, central

  	if (res_num.eq.1) then
	    	filename = trim(base_name) // trim(initial_data) //'_1.t'
  	else if (res_num.eq.2) then
    		filename = trim(base_name) // trim(initial_data) //'_2.t'
  	else if (res_num.eq.3) then
    		filename = trim(base_name) // trim(initial_data) //'_3.t'
  	else if (res_num.eq.4) then
    		filename = trim(base_name) // trim(initial_data) //'_4.t'
  	else if (res_num.eq.5) then
    		filename = trim(base_name) // trim(initial_data) //'_5.t'
  	end if

  	if (first_index.eq.0) then
     	filestatus = 'replace'
  	else
     	filestatus = 'old'
  	end if


	! -----------------------   Calculating scalars   ----------------------------

  	max = yval(1,1,1)
  	min = yval(1,1,1)

  	do h=1,Nz
     		do j=1,Ny
     			do i=1, Nx
        			if (yval(i,j,h)>max) max = yval(i,j,h)
        			if (yval(i,j,h)<min) min = yval(i,j,h)
     			end do
  		end do
	end do
	
  	central = yval(Nx/2,Ny/2,Nz/2)

	! -------------------------   Saving data   ----------------------------------

  	if (filestatus=='replace') then
	     	open(1,file=filename,form='formatted',status=filestatus)
  		write(1,*) '#Time                     max                     central'
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
    
    	write(1,*) t,max,central
  	close(1)
end subroutine save0D

