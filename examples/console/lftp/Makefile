PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm -FU../units -FE.
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) lftpclient

argless:
	$(PP) $(BASEARGS) $(OPT) lftpclient

clean:
	delp .
	rm -f lftpclient
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
