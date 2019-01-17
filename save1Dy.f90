!-----------------------------------------------------------------------------------------
!-----------------SAVES THE VALUE yval ALONG THE y AXIS AT A FIXED x ---------------------
!-----------------------------------------------------------------------------------------
subroutine save1Dy(yval,base_name,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), dimension(1:Ny), intent(IN) :: yval

  	character(len=256) :: filename

  	integer j,first_index

  	if (res_num.eq.1) then
  	  filename = trim(base_name) // trim(initial_data) //'_1.y'
  	else if (res_num.eq.2) then
  	  filename = trim(base_name) // trim(initial_data) //'_2.y'
  	else if (res_num.eq.3) then
  	  filename = trim(base_name) // trim(initial_data) //'_3.y'
  	else if (res_num.eq.4) then
  	  filename = trim(base_name) // trim(initial_data) //'_4.y'
  	else if (res_num.eq.5) then
  	  filename = trim(base_name) // trim(initial_data) //'_5.y'
  	end if

  	if (first_index.eq.0) then
  	   filestatus = 'replace'
  	else
  	   filestatus = 'old'
  	end if

	! -----------------------------   Saving data   ---------------------
  	if (filestatus=='replace') then
  	   open(1,file=filename,form='formatted',status=filestatus)
  	else
  	   open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) ''
  	write(1,*) '#Time = ',t
  	do j=1,Ny, 2**(res_num-1)
  	   write(1,"(2ES14.6)") y(Nx/2,j,Nz/2),yval(j)
  	end do
  	write(1,*)
  	close(1)

end subroutine save1Dy

