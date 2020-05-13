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
VERSION=0.7.7

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
	rm -f $(EXECUTABLE)
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE))" \
                --eval "(asdf:make :$(EXECUTABLE))" \
		--eval "(uiop:quit)"

webkit-client: $(CLIENT_SOURCE)
	$(CC) -Wall -Wextra $(CLIENT_SOURCE) -o $(CLIENT_EXECUTABLE) `$(CFLAGS)`

test-unit:
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE)/tests/unit :silent t)" \
		--eval "(asdf:test-system :$(EXECUTABLE)/tests/unit)" \
		--eval "(uiop:quit)"

test-functional:
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE)/tests/functional :silent t)" \
		--eval "(asdf:test-system :$(EXECUTABLE)/tests/functional)" \
		--eval "(uiop:quit)"

test:
	$(LISP) --eval "(ql:quickload '(\"quaremain/tests/unit\" \"quaremain/tests/functional\") )" \
		--eval "(asdf:test-system :quaremain)" \
		--eval "(uiop:quit)"

migrate-seeds: $(EXECUTABLE.asd)
	$(LISP) --eval "(ql:quickload :$(EXECUTABLE) :silent t)" \
		--eval "($(EXECUTABLE).utilities.database:migrate-tables)" \
		--eval "($(EXECUTABLE).utilities.database::migrate-seeds)" \
		--eval "(uiop:quit)"

ubuntu20.04-tarball: all
	cp -r bin/ $(EXECUTABLE)-$(VERSION)-ubuntu20.04
	tar -acf $(EXECUTABLE)-$(VERSION)-ubuntu20.04.tar.gz $(EXECUTABLE)-$(VERSION)-ubuntu20.04/
	rm -rf $(EXECUTABLE)-$(VERSION)-ubuntu20.04/

ubuntu18.04-tarball: all
	cp -r bin/ $(EXECUTABLE)-$(VERSION)-ubuntu18.04
	tar -acf $(EXECUTABLE)-$(VERSION)-ubuntu18.04.tar.gz $(EXECUTABLE)-$(VERSION)-ubuntu18.04/
	rm -rf $(EXECUTABLE)-$(VERSION)-ubuntu18.04/

opensusetumbleweed-tarball: all
	cp -r bin/ $(EXECUTABLE)-$(VERSION)-opensusetumbleweed
	tar -acf $(EXECUTABLE)-$(VERSION)-opensusetumbleweed.tar.gz $(EXECUTABLE)-$(VERSION)-opensusetumbleweed/
	rm -rf $(EXECUTABLE)-$(VERSION)-opensusetumbleweed/

all-tarballs: ubuntu20.04-tarball opensusetumbleweed-tarball

manpage: README.md
	ronn -r README.md
	mv README dist-data/quaremain.1

migrate-client:
	cd client;npm run build;cd ..
	rm -rf templates static
	mkdir templates static
	mv client/dist/index.html templates/experimental.html
	cp -r client/dist/* static/

clean:
	rm -rf bin/
	rm -f $(EXECUTABLE)
	rm -f var/$(EXECUTABLE).db
	rm -f $(CLIENT_EXECUTABLE)
	rm -f $(EXECUTABLE)-$(VERSION)-ubuntu20.04.tar.gz
	rm -f $(EXECUTABLE)-$(VERSION)-opensusetumbleweed.tar.gz
	rm -f tests/*.fasl
