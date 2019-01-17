!-----------------------------------------------------------------------------------------
!-------------SAVES THE VALUE OF A FUNCTION zval FOR A 3D PLOT (splot)--------------------
!-----------------------------------------------------------------------------------------
subroutine save2Dzx(zval,base_name,first_index)

  	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), dimension(1:Nx,1:Nz), intent(IN) :: zval !Value we are looking to save

  	character(len=256) :: filename

  	integer i,j,first_index

  	if (res_num.eq.1) then
	    	filename = base_name // trim(initial_data) //'_1.zx'
  	else if (res_num.eq.2) then
 	   	filename = base_name // trim(initial_data) //'_2.zx'
  	else if (res_num.eq.3) then
    		filename = base_name // trim(initial_data) //'_3.zx'
  	else if (res_num.eq.4) then
    		filename = base_name // trim(initial_data) //'_4.zx'
  	else if (res_num.eq.5) then
    		filename = base_name // trim(initial_data) //'_5.zx'
  	end if

  	if (first_index.eq.0) then
     		filestatus = 'replace'
  	else
     		filestatus = 'old'
  	end if

	! ---------------------------   Saving data   ------------------------
  	if (filestatus=='replace') then
	     	open(1,file=filename,form='formatted',status=filestatus)
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) ''
  	write(1,*) '#Time = ',t

  	do j=1,Nz,2**(res_num-1)
  		write(1,*)
    		do i=1,Nx,2**(res_num-1)
      			write(1,*) x(i,Ny/2,j),z(i,Ny/2,j),zval(i,j)
    		end do
  	end do
  	write(1,*)
  	close(1)

end subroutine save2Dzx

