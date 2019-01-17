!-----------------------------------------------------------------------------------------
!----------------------- SAVES THE GIVEN SCALR VALUE FOR EACH TIME -----------------------
!-----------------------------------------------------------------------------------------
subroutine saveEscalar(val,base_name,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), intent(IN) :: val

  	character(len=256) :: filename

  	integer i,j,h,first_index

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


! --->   Saving data   <---

  	if (filestatus=='replace') then
     	open(1,file=filename,form='formatted',status=filestatus)
  		write(1,*) '#Time                     Scalar'
  	else
     	open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
    
	write(1,*) t, val
  	close(1)


! --->   END   <---

  end subroutine saveEscalar

