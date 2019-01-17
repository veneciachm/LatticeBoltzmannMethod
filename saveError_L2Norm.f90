!-----------------------------------------------------------------------------------------
!-------------SAVES THE VALUE OF THE L_2 ERROR NORM (plot)--------------------
!-----------------------------------------------------------------------------------------
subroutine saveError_L2Norm(error_x, error_y, error_z, base_name,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8) error_x, error_y, error_z !Value we are looking to save

  	character(len=256) :: filename

  	integer first_index

  	if (res_num.eq.1) then
	    	filename = base_name // trim(initial_data) //'_1.t'
  	else if (res_num.eq.2) then
 	   	filename = base_name // trim(initial_data) //'_2.t'
  	else if (res_num.eq.3) then
    		filename = base_name // trim(initial_data) //'_3.t'
  	else if (res_num.eq.4) then
    		filename = base_name // trim(initial_data) //'_4.t'
  	else if (res_num.eq.5) then
    		filename = base_name // trim(initial_data) //'_5.t'
  	end if

  	if (first_index.eq.0) then
     		filestatus = 'replace'
  	else
     		filestatus = 'old'
  	end if

	! ------------------------   Saving data   -----------------------
  	if (filestatus=='replace') then
	     	open(1,file=filename,form='formatted',status=filestatus)
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) t, error_x, error_y, error_z
  	close(1)

end subroutine saveError_L2Norm
