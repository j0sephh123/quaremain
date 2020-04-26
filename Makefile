EXECUTABLE=quaremain
LISP ?= sbcl
PREFIX=/opt
DESTDIR=$(PREFIX)/$(EXECUTABLE)
DATABASE=$(DESTDIR)/var/$(EXECUTABLE).db
CC=gcc
CFLAGS=pkg-config --libs --cflags webkit2gtk-4.0
CLIENT_SOURCE=quaremain-client.c
CLIENT_EXECUTABLE=quaremain-client
VERSION_BUMPER_SCRIPT=version-bumper.sh
VERSION=0.6.0

.PHONY: all server webkit-client ubuntu20.04-tarball opensusetumbleweed-tarball
all: server webkit-client

	mkdir -p bin/var
	cp bin/$(EXECUTABLE) .
	cp -f $(CLIENT_EXECUTABLE) bin/
	cp dist-data/* bin/
	cp -r static/ bin/
	cp -r templates bin/
	cp Quaremain bin/
	rm -f bin/libssl* # don't want any trouble with weird licensing issues
	chmod a+x $(VERSION_BUMPER_SCRIPT)
	./$(VERSION_BUMPER_SCRIPT) ${VERSION}


server: $(EXECUTABLE).asd
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE))" \
                --eval "(asdf:make :$(EXECUTABLE))" \
		--eval "(uiop:quit)"

webkit-client: $(CLIENT_SOURCE)
	$(CC) $(CLIENT_SOURCE) -o $(CLIENT_EXECUTABLE) `$(CFLAGS)`

test: $(EXECUTABLE).asd
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE)/tests :silent t)" \
		--eval "(asdf:test-system :$(EXECUTABLE))" \
		--eval "(uiop:quit)"

ubuntu20.04-tarball: all
	cp -r bin/ $(EXECUTABLE)-$(VERSION)-ubuntu20.04
	tar -acf $(EXECUTABLE)-$(VERSION)-ubuntu20.04.tar.gz $(EXECUTABLE)-$(VERSION)-ubuntu20.04/
	rm -rf $(EXECUTABLE)-$(VERSION)-ubuntu20.04/

opensusetumbleweed-tarball: all
	cp -r bin/ $(EXECUTABLE)-$(VERSION)-opensusetumbleweed
	tar -acf $(EXECUTABLE)-$(VERSION)-opensusetumbleweed.tar.gz $(EXECUTABLE)-$(VERSION)-opensusetumbleweed/
	rm -rf $(EXECUTABLE)-$(VERSION)-opensusetumbleweed/

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
	rm -f $(EXECUTABLE)-$(VERSION)-ubuntu20.04.tar.gz
	rm -f $(EXECUTABLE)-$(VERSION)-opensusetumbleweed.tar.gz
