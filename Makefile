OCB=ocamlbuild
OCBF=-use-ocamlfind
OCF=ocamlfind
NAME=bap-types
PIQI=piqi
TARGETS=$(NAME).cma $(NAME).cmxa
all: $(TARGETS)

.PHONY: %.cmxa
%.cmxa:
	$(OCB) $(OCBF) $@
.PHONY: %.cma
%.cma:
	$(OCB) $(OCBF) $@

INSTALL_FILES = $(patsubst %,_build/%,$(TARGETS)) META

.PHONY: install
install: $(TARGETS)
	$(OCF) install $(NAME) $(INSTALL_FILES)

.PHONY: test
test: all

.PHONY: check
check: check-piqi

.PHONY: check-piqi
check-piqi: piqi/*.piqi
	for piqifile in $^; do $(PIQI) check --strict $$piqifile; done

.PHONY: uninstall
uninstall:
	$(OCF) remove $(NAME)

.PHONY: clean
clean:
	rm -rf _build
