EXECUTABLE=quaremain
LISP ?= sbcl
PREFIX=/opt
DESTDIR=$(PREFIX)/$(EXECUTABLE)
DATABASE=$(DESTDIR)/var/$(EXECUTABLE).db
CC=gcc
CFLAGS=pkg-config --libs --cflags webkit2gtk-4.0
CLIENT_SOURCE=quaremain-client.c
CLIENT_EXECUTABLE=quaremain-client
VERSION=0.2.0

.PHONY: all server webkit-client ubuntu20.04-tarball opensusetumbleweed-tarball
all: server webkit-client

	mkdir -p bin/var
	cp bin/$(EXECUTABLE) .
	cp -f $(CLIENT_EXECUTABLE) bin/
	cp dist-data/* bin/
	cp -r static/ bin/
	cp -r templates bin/
	rm -f bin/libssl* # don't want any trouble with weird licensing issues


server: $(EXECUTABLE).asd
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE))" \
                --eval "(asdf:make :$(EXECUTABLE))" \
		--eval "(uiop:quit)"

webkit-client: $(CLIENT_SOURCE)
	$(CC) $(CLIENT_SOURCE) -o $(CLIENT_EXECUTABLE) `$(CFLAGS)`

test: $(EXECUTABLE).asd
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE) :silent t)" \
		--eval "(asdf:test-system :$(EXECUTABLE))" \
		--eval "(uiop:quit)"

ubuntu20.04-tarball: all
	cp -r bin/ $(EXECUTABLE)-ubuntu20.04-$(VERSION)
	tar -acf $(EXECUTABLE)-ubuntu20.04-$(VERSION).tar.gz $(EXECUTABLE)-ubuntu20.04-$(VERSION)
	rm -rf $(EXECUTABLE)-ubuntu20.04-$(VERSION)

opensusetumbleweed-tarball: all
	cp -r bin/ $(EXECUTABLE)-opensusetumbleweed-$(VERSION)
	tar -acf $(EXECUTABLE)-opensusetumbleweed-$(VERSION).tar.gz $(EXECUTABLE)-opensusetumbleweed-$(VERSION)
	rm -rf $(EXECUTABLE)-opensusetumbleweed-$(VERSION)

all-tarballs: ubuntu20.04-tarball opensusetumbleweed-tarball

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
	rm -f $(DATABASE)
	rm -f $(DESTDIR)/$(EXECUTABLE)
	rmdir $(DESTDIR)/var
	rmdir $(DESTDIR)

clean:
	rm -rf bin/
	rm -f $(EXECUTABLE)
	rm -f var/$(EXECUTABLE).db
	rm -f $(CLIENT_EXECUTABLE)
	rm -f $(EXECUTABLE)-ubuntu20.04-$(VERSION).tar.gz
	rm -f $(EXECUTABLE)-opensusetumbleweed-$(VERSION).tar.gz
