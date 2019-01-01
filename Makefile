
all:
	$(MAKE) -C examples OPT=$(OPT)

argless:
	$(MAKE) -C examples argless OPT=$(OPT)

clean:
	delp .
	delp lib
	delp lib/sys
	delp examples/console/units
	$(MAKE) -C examples clean
	rm -f *~
