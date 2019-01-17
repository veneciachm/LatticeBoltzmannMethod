!-----------------------------------------------------------------------------------------
!-----------------SAVES THE VALUE xval ALONG THE x AXIS AT A FIXED y ---------------------
!-----------------------------------------------------------------------------------------
subroutine save1Dx(xval,base_name,first_index)

	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), dimension(1:Nx), intent(IN) :: xval

  	character(len=256) :: filename

  	integer i,first_index

  	if (res_num.eq.1) then
	    	filename = trim(base_name) // trim(initial_data) //'_1.x'
  	else if (res_num.eq.2) then
  		filename = trim(base_name) // trim(initial_data) //'_2.x'
  	else if (res_num.eq.3) then
    		filename = trim(base_name) // trim(initial_data) //'_3.x'
  	else if (res_num.eq.4) then
  	  	filename = trim(base_name) // trim(initial_data) //'_4.x'
  	else if (res_num.eq.5) then
    		filename = trim(base_name) // trim(initial_data) //'_5.x'
  	end if

  	if (first_index.eq.0) then
     		filestatus = 'replace'
  	else
	     	filestatus = 'old'
  	end if


	! -------------------------   Saving data   ----------------------------

  	if (filestatus=='replace') then
	     	open(1,file=filename,form='formatted',status=filestatus)
  	else
	     	open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) ''
  	write(1,*) '#Time = ',t
  	do i=1,Nx, 2**(res_num-1)
	     	write(1,"(2ES14.6)") x(i,Ny/2,Nz/2),xval(i)
  	end do
  	write(1,*)
  	close(1)

end subroutine save1Dx

