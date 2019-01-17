subroutine evolve
	use arrays
  	use global_numbers

  	implicit none

  	integer i,j,h,k
	! ----------------------------------------------------------------------------------
  	t = zero
  	flag_steady= .false. 
	! ----------------------------------------------------------------------------------
  	call initial      
	! ----------------------------------------------------------------------------------
	print *,'-------------------------------------------'
        print *,'|  Time step  |     Time     |  TotalMass |'
        print *,'-------------------------------------------'
        write(*,"(A5,I6,A6,ES9.2,A6,ES9.2,A3)") ' |   ',0,'    | ',t,'    | ',TotalMass,'  |'
	! ----------------------------------------------------------------------------------
	! -----------------------------   SAVE INITIAL DATA  -------------------------------

  	!call save0D(rho,'rho',0)
	call saveEscalar(TotalMass, 'TotalMass', 0)  

  	!call save1Dz(rho(Nx/2,Wave_len/4,:),'rho',0)
  
  	call save2Dyz(rho(Nx/2,:,:),'rhoYZ',0)   

  	!call save2Dzx(rho(:,Wave_len/4,:),'rhoZX',0)   
  	!call save2Dyz(sqrt((Cu*vy(Nx/2,:,:))**2+(Cu*vz(Nx/2,:,:))**2),'vel2_x2',0)

    

	if (initial_data.eq.'Initial_TanWave' ) then
		call saveInterface_Position (rho(Nx/2,Wave_len/4,:),0)
	end if
	
	! ----------------------------------------------------------------------------------
	! ----------------------------------------------------------------------------------
	! ---------------------------------  EVOLUTION -------------------------------------
	! ----------------------------------------------------------------------------------
	! ----------------------------------------------------------------------------------
	l=0	
	do while (t.le.t_final)
		l=l+1	
     		t = t + dt

    		if (method.eq.'3DQ19') then
       			call collision   			
       			call hydro_variables  
     		else 
			print*, 'We have no other method programmed'
			stop
    		end if
		! ------------------------------------------------------------------------------
		! ------------------------   SAVE DATA TO FILE   -------------------------------
		if (mod(l,every_0D).eq.0) then
			!call save0D(rho,'rho',1)
			call saveEscalar(TotalMass, 'TotalMass', 1)  

	    		if (initial_data.eq.'Initial_TanWave') then
				call saveInterface_Position (rho(Nx/2,Wave_len/4,:),1)
			end if
    		end if
     
    		!if (mod(l,every_1D).eq.0) then
  		!	call save1Dz(rho(Nx/2,Wave_len/4,:),'rho',1)
	     	!end if
		if (mod(l,every_2D).eq.0) then
       			call save2Dyz(rho(Nx/2,:,:),'rhoYZ',1)   
  		!	call save2Dzx(rho(:,Wave_len/4,:),'rhoZX',1)   
  	
  		!	call save2Dyz(sqrt((Cu*vy(Nx/2,:,:))**2+(Cu*vz(Nx/2,:,:))**2),'vel2_x2',1)
       		  	
       			if (flag_steady) then
       				print *, 'Finished at t=',t
       				exit
     			end if
     		end if

     		if (mod(l,every_1D).eq.0) then
			write(*,"(A5,I6,A6,ES9.2,A6,ES9.2,A3)") ' |   ',l,'    | ',t,'    | ',TotalMass,'  |'
     		end if
     		if (t.ge.t_final) then
       			print *, 'Finished at t=',t
       			exit
     		end if
		! ----------------------------------------------------------------------------------
		! ----------------------------------------------------------------------------------
	end do
  	print *,'-----------------------------'
  	! ----------------------------------------------------------------------------------
	! ----------------------------------------------------------------------------------

end subroutine evolve

