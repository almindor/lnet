PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm -FU../units -FE.
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) lserver
	$(PP) $(ARGS) lclient

argless:
	$(PP) $(BASEARGS) $(OPT) lserver
	$(PP) $(BASEARGS) $(OPT) lclient

clean:
	delp .
	rm -f lserver
	rm -f lclient
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
