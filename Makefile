EXECUTABLE=quaremain
LISP ?= sbcl
PREFIX=/opt
DESTDIR=$(PREFIX)/$(EXECUTABLE)
DATABASE=$(DESTDIR)/var/$(EXECUTABLE).db
CC=gcc
CFLAGS=pkg-config --libs --cflags webkit2gtk-4.0
CLIENT_SOURCE=quaremain-client.c
CLIENT_EXECUTABLE=quaremain-client

.PHONY: webkit-client
all: $(EXECUTABLE).asd webkit-client
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE))" \
                --eval "(asdf:make :$(EXECUTABLE))" \
		--eval "(uiop:quit)"
	cp bin/$(EXECUTABLE) .
	cp $(CLIENT_EXECUTABLE) bin/
	cp dist-data/* bin/
	cp -r static/ bin/
	cp -r templates bin/
	mkdir -p bin/var

webkit-client: $(CLIENT_SOURCE)
	$(CC) $(CLIENT_SOURCE) -o $(CLIENT_EXECUTABLE) `$(CFLAGS)`

install: $(EXECUTABLE) $(CLIENT_EXECUTABLE)
	mkdir -p $(DESTDIR)/var
	mkdir $(DESTDIR)/bin
	install $(EXECUTABLE) $(DESTDIR)
	install $(CLIENT_EXECUTABLE) $(DESTDIR)/bin
	cp -r templates $(DESTDIR)
	cp -r static $(DESTDIR)
	touch $(DATABASE)
	chmod -R o+rw $(DESTDIR)/var
	echo "#!/bin/sh" >> $(DESTDIR)/bin/$(EXECUTABLE)
	echo "cd $(DESTDIR) && ./$(EXECUTABLE)" >> $(DESTDIR)/bin/$(EXECUTABLE)
	chmod o+rx $(DESTDIR)/bin/$(EXECUTABLE)

uninstall:
	rm -rf $(DESTDIR)/templates
	rm -rf $(DESTDIR)/bin
	rm -rf $(DESTDIR)/static
	rm -f $(DESTDIR)/app.lisp
	rm -f $(DATABASE)
	rm -f $(DESTDIR)/$(EXECUTABLE)
	rmdir $(DESTDIR)/var
	rmdir $(DESTDIR)

clean:
	rm -rf bin/
	rm -f $(EXECUTABLE)
	rm -f var/$(EXECUTABLE).db
	rm -f $(CLIENT_EXECUTABLE)
