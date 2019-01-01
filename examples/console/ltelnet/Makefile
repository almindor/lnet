PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm -FU../units -FE.
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) ltclient

argless:
	$(PP) $(BASEARGS) $(OPT) ltclient

clean:
	delp .
	rm -f ltclient
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
