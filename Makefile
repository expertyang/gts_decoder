PACKAGE = gts_decoder
tar:
	tar -cvf $(PACKAGE).tar src Makefile *.bash util gts_sttnid_final* 
	mv $(PACKAGE).tar $(PACKAGE)-`date +%Y%m%d%H%M`.tar
	gzip *.tar

