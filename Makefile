OCB=ocamlbuild
OCBF=-use-ocamlfind
OCF=ocamlfind
NAME=bap-types
PIQI=piqi
OCI=ocp-indent
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
check: check-piqi check-ocp-indent

.PHONY: check-piqi
check-piqi: piqi/*.piqi
	for piqifile in $^; do $(PIQI) check --strict $$piqifile; done

.PHONY: check-ocp-indent
check-ocp-indent: *.ml
	for mlfile in $^; do $(OCI) $$mlfile | diff - $$mlfile; done

.PHONY: uninstall
uninstall:
	$(OCF) remove $(NAME)

.PHONY: clean
clean:
	$(OCB) -classic-display -clean
