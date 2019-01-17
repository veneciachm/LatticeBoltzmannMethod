subroutine saveVFieldxy(hor_comp,ver_comp,downsampling,base_name,first_index)
	use arrays
  	use global_numbers

  	implicit none

  	character(len=20) filestatus
  	logical firstcall
  	data firstcall / .true. /
  	save firstcall

  	character(len=*), intent(IN) :: base_name
  	real(kind=8), &
  	dimension(1:Nx,1:Ny), intent(IN) :: hor_comp,ver_comp

  	character(len=256) :: filename

  	integer i,j,k,m,first_index,downsampling

  	if (res_num.eq.1) then
    		filename = trim(base_name) // trim(initial_data) //'_1.vxy'
  	else if (res_num.eq.2) then
    		filename = trim(base_name) // trim(initial_data) //'_2.vxy'
  	else if (res_num.eq.3) then
    		filename = trim(base_name) // trim(initial_data) //'_3.vxy'
  	else if (res_num.eq.4) then
    		filename = trim(base_name) // trim(initial_data) //'_4.vxy'
  	else if (res_num.eq.5) then
    		filename = trim(base_name) // trim(initial_data) //'_5.vxy'
  	end if

  	if (first_index.eq.0) then
     		filestatus = 'replace'
  	else
     		filestatus = 'old'
  	end if

	! ----------------------------   Saving data   -----------------------
  	if (filestatus=='replace') then
     		open(1,file=filename,form='formatted',status=filestatus)
  	else
     		open(1,file=filename,form='formatted',status=filestatus,position='append')
  	end if
  	write(1,*) ''
  	write(1,*) '#Time = ',t

    	do i=1,Nx,downsampling
      		do j=1,Ny,downsampling
        		write(1,*) x(i,j,Nz/2),y(i,j,Nz/2),hor_comp(i,j),ver_comp(i,j)
      		end do
    	end do

  	write(1,*)
  	close(1)

  end subroutine saveVFieldxy
