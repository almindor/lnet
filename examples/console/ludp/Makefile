PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm -FU../units -FE.
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) ludp

argless:
	$(PP) $(BASEARGS) $(OPT) ludp

clean:
	delp .
	rm -f ludp
	rm -f *~

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
