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
	cp app.lisp $(DESTDIR)
	cp -r templates $(DESTDIR)
	cp -r static $(DESTDIR)
	touch $(DATABASE)
	chmod -R o+rw $(DESTDIR)/var
	echo "#!/bin/sh" >> $(DESTDIR)/bin/$(EXECUTABLE)
	echo "cd $(DESTDIR) && ./$(EXECUTABLE)" >> $(DESTDIR)/bin/$(EXECUTABLE)
	chmod o+rx $(DESTDIR)/bin/$(EXECUTABLE)

uninstall:
	rm -rf $(DESTDIR)/templates
	rm -rf $(DESTDIR)/static
	rm -f $(DESTDIR)/app.lisp
	rm -f $(DATABASE)
	rm -f $(DESTDIR)/$(EXECUTABLE)
	rm -f $(DESTDIR)/bin/$(EXECUTABLE)
	rmdir $(DESTDIR)/bin
	rmdir $(DESTDIR)/var
	rmdir $(DESTDIR)

clean:
	rm -f $(EXECUTABLE)
	rm -f var/$(EXECUTABLE).db
