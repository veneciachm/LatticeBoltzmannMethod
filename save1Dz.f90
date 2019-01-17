!-----------------------------------------------------------------------------------------
!-----------------SAVES THE VALUE zval ALONG THE z AXIS AT A FIXED y ---------------------
!-----------------------------------------------------------------------------------------
subroutine save1Dz(zval,base_name,first_index)

	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), dimension(1:Nz), intent(IN) :: zval

  	character(len=256) :: filename

  	integer h,first_index

  	if (res_num.eq.1) then
    	filename = trim(base_name) // trim(initial_data) //'_1.z'
  	else if (res_num.eq.2) then
  		filename = trim(base_name) // trim(initial_data) //'_2.z'
  	else if (res_num.eq.3) then
    	filename = trim(base_name) // trim(initial_data) //'_3.z'
  	else if (res_num.eq.4) then
  	  	filename = trim(base_name) // trim(initial_data) //'_4.z'
  	else if (res_num.eq.5) then
    	filename = trim(base_name) // trim(initial_data) //'_5.z'
  	end if

  	if (first_index.eq.0) then
     	filestatus = 'replace'
  	else
     	filestatus = 'old'
  	end if


	! ---------------------------   Saving data   -------------------------
  	if (filestatus=='replace') then
 	    	open(1,file=filename,form='formatted',status=filestatus)
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) ''
  	write(1,*) '#Time = ',t
  	do h=1,Nz, 2**(res_num-1)
     		write(1,"(2ES14.6)") z(Nx/2,Ny/2,h),zval(h)
  	end do
  	write(1,*)
  	close(1)

end subroutine save1Dz

