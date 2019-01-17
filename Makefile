FC=gfortran
#FC=/opt/intel/fc/10.1.012/bin/ifort

FFLAGS = -O3
#FFLAGS = -g

LNK=gfortran

OBJS = evolve.o initial.o main.o ShanChenForce.o  save1Dz.o save0D.o save1Dx.o save1Dy.o save2Dxy.o memory.o collision.o hydro_variables.o LB_Units.o saveError_L2Norm.o save2Dyz.o save2Dzx.o YoungLaplaceTest.o saveYLtest.o saveVFieldxy.o saveInterface_Position.o saveEscalar.o

MODS = arrays.o global_numbers.o

$(OBJS):	$(MODS)

CW:	$(OBJS) $(MODS)
		$(LNK) $(FFLAGS) -o xCW $(OBJS) $(MODS) 
	@ mkdir -p xxx
	@ mv xCW xxx
#	@ cp input.par xxx

.PHONY:	clean

clean:
	-rm -f *.o *.mod xxx/xCW xxx/*.x xxx/*.t xxx/*.y xxx/*.xy xxx/*.yz xxx/*.vxy

%.o : %.f90
	$(FC) -c $(FFLAGS) $< -o $@
