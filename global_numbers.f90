module global_numbers

  implicit none

  real(kind=8) dx,dy, dz, xmin,xmax,ymin,ymax, zmin, zmax
  
  real (kind=8) Cu, C_rho, Cp
  
  real(kind=8) t,dt,t_final
  
  
  real(kind=8) tau, nu
  real(kind=8) zero,half,one,two,third,four,three
  real(kind=8) pi
 
  real(kind=8) TotalMass
 

  integer*8 Nt, l
  integer Sanity_check
  integer Nx,Ny,Nz
  integer res_num
  integer every_0D,every_1D,every_2D
  real(kind=8) every_0Dt,every_1Dt,every_2Dt

  character(len=30) :: method,initial_data

  ! Lattice-Boltzmann stuff
  real(kind=8) steady_x, steady_y, steady_z, ep_x, ep_y, ep_z
  logical flag_steady, FuerzaBool
  real (kind = 8) nu_dim	  
  
  !For the initial TangentH
  real (kind=8) A_tan, wdh, x0 , b, A_wave
  integer  Wave_len
  
  real (kind=8) rho_0, G, rho_l, rho_g
end module global_numbers
  
