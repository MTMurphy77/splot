SHELL = tcsh
TARGET = ${HOME}/bin/splot
FC = gfortran
FFLAGS = -O
# Linux
# LIBS = -lm -lpgplot -lX11
# Max OS X
LIBS = -ldl -lm -lX11 -L/opt/local/lib -lpgplot

OBJECTS = getstats.o plotdat.o rspec.o spec_min.o main.o rchar.o rsplotrc.o \
	spec_stats.o pg_anot.o rdble.o selectplot.o splot.o cpparms.o	    \
	pg_data.o redisp.o setdefs.o curspos.o pg_label.o resize.o	    \
	setvplims.o getlen.o pg_panl.o rint.o spec_copy.o wsplotrc.o	    \
	getncols.o pg_range.o rprevspec.o spec_max.o ratomdat.o sepchar.o   \
	nametrans.o transname.o register.o rounddble.o gather.o gsmooth.o

splot:	$(OBJECTS)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJECTS) $(LIBS)

clean:	
	rm -f *.o *~

splot.o: splot.f splot_inc.f charlen_inc.f ratomdat_inc.f

cpparms.o: cpparms.f splot_inc.f 

curspos.o: curspos.f splot_inc.f

getstats.o: getstats.f splot_inc.f

main.o: main.f splot_inc.f ratomdat_inc.f charlen_inc.f

pg_anot.o: pg_anot.f splot_inc.f charlen_inc.f

pg_data.o: pg_data.f splot_inc.f

pg_label.o: pg_label.f splot_inc.f charlen_inc.f

pg_range.o: pg_range.f splot_inc.f

plotdat.o: plotdat.f splot_inc.f charlen_inc.f

redisp.o: redisp.f splot_inc.f constants_inc.f

rprevspec.o: rprevspec.f splot_inc.f charlen_inc.f

rspec.o: rspec.f splot_inc.f charlen_inc.f

rsplotrc.o: rsplotrc.f splot_inc.f charlen_inc.f

selectplot.o:selectplot.f splot_inc.f charlen_inc.f

setdefs.o: setdefs.f splot_inc.f charlen_inc.f

setvplims.o: setvplims.f splot_inc.f

spec_copy.o: spec_copy.f splot_inc.f

spec_max.o: spec_max.f splot_inc.f

spec_min.o: spec_min.f splot_inc.f

spec_stats.o: spec_stats.f splot_inc.f

wsplotrc.o: wsplotrc.f splot_inc.f charlen_inc.f

ratomdat.o: ratomdat.f charlen_inc.f ratomdat_inc.f

sepchar.o: sepchar.f charlen_inc.f

nametrans.o: nametrans.f charlen_inc.f ratomdat_inc.f

transname.o: transname.f charlen_inc.f ratomdat_inc.f

register.o: register.f splot_inc.f ratomdat_inc.f charlen_inc.f constants_inc.f

rounddble.o: rounddble.f charlen_inc.f

gather.o: gather.f splot_inc.f charlen_inc.f ratomdat_inc.f

