set pm3d map
if(iter<n) \
	splot "rhoYZInitial_TanWave_1.yz" i iter ; \
	print iter; \
	iter=iter+1; \
	pause 0.1;\
	reread

