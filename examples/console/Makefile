
all:
	$(MAKE) -C ltcp OPT=$(OPT)
	$(MAKE) -C ludp OPT=$(OPT)
	$(MAKE) -C lsmtp OPT=$(OPT)
	$(MAKE) -C lftp OPT=$(OPT)
	$(MAKE) -C lhttp OPT=$(OPT)
	$(MAKE) -C ltelnet OPT=$(OPT)

argless:
	$(MAKE) -C ltcp argless OPT=$(OPT)
	$(MAKE) -C ludp argless OPT=$(OPT)
	$(MAKE) -C lsmtp argless OPT=$(OPT)
	$(MAKE) -C lftp argless OPT=$(OPT)
	$(MAKE) -C lhttp argless OPT=$(OPT)
	$(MAKE) -C ltelnet argless OPT=$(OPT)

clean:
	$(MAKE) -C ltcp clean
	$(MAKE) -C ludp clean
	$(MAKE) -C lsmtp clean
	$(MAKE) -C lftp clean
	$(MAKE) -C lhttp clean
	$(MAKE) -C ltelnet clean
	rm -f *~
