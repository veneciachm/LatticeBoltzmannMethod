# LatticeBoltzmannMethod
Lattice-Boltzmann code for fluid mechanics, with the implementation of the Shan-Chen model for multiphase simulations of capillary waves at liquid-vapor interfaces.

### Author: Venecia Chávez Medina
* **Affiliation**: Institute of Physics and Mathematics, Universidad Michoacana de San Nicolás de Hidalgo Mexico.
* **Email**: veneciachm(*at*)ifm.umich.mx

## Project:
* This is a 3D LBM code in Fortran 90, with a Shan Chen model implementation that uses Guo's Forcing Scheme in order to simulate a 2 phase fluid that evolves a stationary capillary wave. 
* Shan Chen model is a diffuse interface, bottom-up approach, meaning that the postulation of a microscopic interaction between fluid molecules, leads to a macroscopic separation of phase which implies a hyperbolic tangent-like transition of the density between one phase and the other. This way, surface tension is an emergent effect of the method. 
* The initial conditions of the system (hyperbolic tangent parameters in iii.par file) are based on previous results from this code. 

![alt text](https://github.com/veneciachm/LatticeBoltzmannMethod/blob/master/CapillaryWave.gif)

* ***This is a work in progress*** as part of my Masters Project. 
* It reduces the system to a 2-dimensional domain by establishing a symmetry along the x-direction (Nx=1).
* How original is it? Totally home made, no libraries required.

### The code:
The numerical methods are implemented in the following files: main.f90, evolve.f90, LB_Units.f90, initial.f90, collision.f90, ShanChenForce.f90, hydro_variables.f90. 

Some other helpful subroutines are those defining global variables and memory assignment: global_numbers.f90, arrays.f90, memory.f90

Files generating the output data: save0D.f90, save1Dx.f90, save1Dy.f90, save1Dz.f90, save2Dxy.f90, save2Dyz.f90, save2Dzx.f90, saveEscalar.f90, saveInterface_Position.f90, saveVFieldxy.f90. 

Input files specifying parameters: xxx/iii.par

Scripts that generate a movie whit the output data: xxx/movie.gnu, xxx/DensityProfile.gnu.

And a Makefile to compile the project:
```
make CW
```
this will create an executable and place it in the directory xxx/ where you can find a parameter file iii.par. Then type:
```
cd xxx/
```
Here, the parameter file contains the necessary input to generate a capillary wave (Amplitude << Wave length) that will evolve during 15000 iterations in time and print the output data every 100 iterations. 

In order to execute the program (assuming you are at xxx/ directory) type:
```
./xCW
```
Using the parameters in iii.par, the simulation will run for about 11 minutes (consider this serial version allows the usage of only one core).

This example uses Lattice Units, it is possible to modify this in LD_Units.f90 and change it to physical (MKS system). 

### Plotting:
To generate a movie, type from xxx/ :
```
gnuplot movie.gnu
```
that will produce a .gif file like the one shown above. 
