OBJS   = div_mod.o \
         fuga_mod.o \
         molecules_prop.o \
         calc_fugacity_bin.o \
         fluid_properties.o \
         main_calc_fugacity.o \

FOPS = -g 

fuga: $(OBJS) 
	@echo "Linking fuga ..."
	@ifort $(FOPS) $(OBJS) -o fuga.exe
	@echo "done"

clean:;	@ rm -f *.o *.mod


%.o: %.f90
	ifort -c $(FOPS) $^
