PP=fpc
BASEARGS=-Fu../../../lib -Fi../../../lib/sys -Sm -FU../units -FE.
ARGS=$(BASEARGS) $(OPT) -O2 -XX -Xs

all:
	$(PP) $(ARGS) fphttpd
	$(PP) $(ARGS) fpget

argless:
	$(PP) $(BASEARGS) $(OPT) fphttpd
	$(PP) $(BASEARGS) $(OPT) fpget

clean:
	delp .
	rm -f fphttpd
	rm -f *~
	rm -f fpget

clear:
	$(MAKE) -C . clean
	$(MAKE) -C ../../../ clean
