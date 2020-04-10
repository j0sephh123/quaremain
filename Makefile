EXECUTABLE=quaremain
LISP ?= sbcl
PREFIX=/opt
DESTDIR=$(PREFIX)/$(EXECUTABLE)
DATABASE=$(DESTDIR)/var/$(EXECUTABLE).db

all: $(EXECUTABLE).asd
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE))" \
                --eval "(asdf:make :$(EXECUTABLE))" \
		--eval "(uiop:quit)"

install: $(EXECUTABLE)
	mkdir -p $(DESTDIR)/var
	mkdir $(DESTDIR)/bin
	install $(EXECUTABLE) $(DESTDIR)
	touch $(DATABASE)
	chmod -R o+rw $(DESTDIR)/var
	echo "#!/bin/sh" >> $(DESTDIR)/bin/$(EXECUTABLE)
	echo "cd $(DESTDIR) && ./$(EXECUTABLE)" >> $(DESTDIR)/bin/$(EXECUTABLE)
	chmod o+rx $(DESTDIR)/bin/$(EXECUTABLE)

uninstall:
	rm -f $(DATABASE)
	rm -f $(DESTDIR)/$(EXECUTABLE)
	rm -f $(DESTDIR)/bin/$(EXECUTABLE)
	rmdir $(DESTDIR)/bin
	rmdir $(DESTDIR)/var
	rmdir $(DESTDIR)

clean: $(EXECUTABLE) var/$(EXECUTABLE).db
	rm -f $(EXECUTABLE)
	rm -f var/$(EXECUTABLE).db
