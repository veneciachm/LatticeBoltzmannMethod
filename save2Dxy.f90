!-----------------------------------------------------------------------------------------
!-------------SAVES THE VALUE OF A FUNCTION zval FOR A 3D PLOT (splot)--------------------
!-----------------------------------------------------------------------------------------
subroutine save2Dxy(zval,base_name,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), dimension(1:Nx,1:Ny), intent(IN) :: zval !Value we are looking to save

  	character(len=256) :: filename

  	integer i,j,first_index

  	if (res_num.eq.1) then
	    	filename = base_name // trim(initial_data) //'_1.xy'
  	else if (res_num.eq.2) then
 	   	filename = base_name // trim(initial_data) //'_2.xy'
  	else if (res_num.eq.3) then
    		filename = base_name // trim(initial_data) //'_3.xy'
  	else if (res_num.eq.4) then
    		filename = base_name // trim(initial_data) //'_4.xy'
  	else if (res_num.eq.5) then
    		filename = base_name // trim(initial_data) //'_5.xy'
  	end if

  	if (first_index.eq.0) then
     		filestatus = 'replace'
  	else
	     	filestatus = 'old'
  	end if

	! ------------------------   Saving data   ---------------------------
  	if (filestatus=='replace') then
	     	open(1,file=filename,form='formatted',status=filestatus)
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) ''
  	write(1,*) '#Time = ',t

  	do i=1,Nx,2**(res_num-1)
  		write(1,*)
    		do j=1,Ny,2**(res_num-1)
      			write(1,*) x(i,j,Nz/2),y(i,j,Nz/2),zval(i,j)
    		end do
  	end do
  	write(1,*)
  	close(1)

end subroutine save2Dxy

