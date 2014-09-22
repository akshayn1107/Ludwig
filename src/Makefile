SUBDIRS = util,parse,type,codegen,top
LIBS = str
MAIN = go

OCAMLBUILD = ocamlbuild
OCAMLBUILDFLAGS = 

DEBUG = $(MAIN).d.byte
NATIVE = $(MAIN).native
TOPLEVEL = $(MAIN).top

all: debug

ludwig: bin/ludwig
bin/ludwig: native
	install $(NATIVE) $@

debug: $(DEBUG)
native: $(NATIVE)
top: $(TOPLEVEL) 

OCAMLOPTS = $(OCAMLBUILDFLAGS) -Is $(SUBDIRS) -libs $(LIBS)
OCAMLBRUN = $(OCAMLBUILD) $(OCAMLOPTS)

%.native: always
	$(OCAMLBRUN) $@
%.d.byte: always
	$(OCAMLBRUN) -cflags -annot $@
top: always
	$(OCAMLBRUN) $@

clean:
	$(OCAMLBUILD) -Is $(SUBDIRS) -clean
	@echo			# $(OCAMLBUILD) -clean needs a newline
	rm -f bin/ludwig

always:

.PHONY: ludwig debug native top clean always
