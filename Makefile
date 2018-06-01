SERVICE := mtcamlx
DESTDIR ?= dist_root
SERVICEDIR ?= /srv/$(SERVICE)

HOME = $(CURDIR)

.PHONY: build install

build: authentication messages trading frontend

lib:
	$(MAKE) -C src/lib
	$(MAKE)	-C src/lib check


authentication: lib
	$(MAKE) -C src/authentication

messages:
	$(MAKE) -C src/messages

trading:
	$(MAKE) -C src/trading

frontend:
	$(MAKE) -C src/frontend prepare
	$(MAKE) -C src/frontend

install: build
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/src/frontend/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/src/authentication/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/src/messages/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/src/lib/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/frontend/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/authentication/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/messages/
	install -m 755 -d $(DESTDIR)$(SERVICEDIR)/trading/

	# install -m 700 -d $(DESTDIR)$(SERVICEDIR)/frontend/data/
	# install -m 700 -d $(DESTDIR)$(SERVICEDIR)/authentication/data/
	# install -m 700 -d $(DESTDIR)$(SERVICEDIR)/messages/data/
	# install -m 700 -d $(DESTDIR)$(SERVICEDIR)/trading/data/

	install -m 755 -d $(DESTDIR)/etc/systemd/system/

	install -m 755 src/authentication/authentication $(DESTDIR)$(SERVICEDIR)/authentication/
	install -m 755 src/authentication/keygen         $(DESTDIR)$(SERVICEDIR)/

	install -m 755 src/messages/messages             $(DESTDIR)$(SERVICEDIR)/messages/

	install -m 755 src/trading/trading               $(DESTDIR)$(SERVICEDIR)/trading/

	install -m 755 src/frontend/frontend             $(DESTDIR)$(SERVICEDIR)/frontend
	install -m 755 src/frontend/frontend.sh          $(DESTDIR)$(SERVICEDIR)/frontend

	install -m 644 src/frontend/*.hs                 $(DESTDIR)$(SERVICEDIR)/src/frontend/
	install -m 644 src/authentication/*.ml           $(DESTDIR)$(SERVICEDIR)/src/authentication/
	install -m 644 src/messages/*.ml                 $(DESTDIR)$(SERVICEDIR)/src/messages/
	install -m 644 src/lib/*.ml                      $(DESTDIR)$(SERVICEDIR)/src/lib/

	install -m 644 README.md                         $(DESTDIR)$(SERVICEDIR)/

	install -m 644 src/frontend/mtcamlx-frontend@.service      $(DESTDIR)/etc/systemd/system/
	install -m 644 src/frontend/mtcamlx-frontend.socket        $(DESTDIR)/etc/systemd/system/
	install -m 644 src/trading/mtcamlx-trading.service                 $(DESTDIR)/etc/systemd/system/
	install -m 644 src/authentication/mtcamlx-authentication.service   $(DESTDIR)/etc/systemd/system/
	install -m 644 src/authentication/mtcamlx-keygen.service           $(DESTDIR)/etc/systemd/system/
	install -m 644 src/messages/mtcamlx-messages.service               $(DESTDIR)/etc/systemd/system/

keygen:
	src/authentication/keygen serverParam src/authentication/secret.json
	src/authentication/keygen clientParam src/messages/secret.json
	src/authentication/keygen clientParam src/frontend/secret.json
	src/authentication/keygen clientParam src/trading/secret.json
	src/authentication/keygen publicParam src/authentication/public.json src/authentication/secret.json
	src/authentication/keygen publicParam src/messages/public.json       src/authentication/secret.json
	src/authentication/keygen publicParam src/frontend/public.json       src/authentication/secret.json
	src/authentication/keygen publicParam src/trading/public.json        src/authentication/secret.json
