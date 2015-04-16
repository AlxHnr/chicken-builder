CSC := $(shell which csc)
CSC_FLAGS =  -O3

PROGRAM = chicken-builder-init
SYNTAX_FILES = chb-syntax.import.scm chb-target.scm

INSTALL_PREFIX ?= /usr/local
SYNTAX_INSTALL_PATH = $(INSTALL_PREFIX)/share/chicken-builder/

# Build targets.
.PHONY: all clean
all: $(PROGRAM) $(SYNTAX_FILES)

$(PROGRAM): $(PROGRAM).scm
	$(CSC) $(CSC_FLAGS) $< -o $@

%.import.scm: %.scm
	$(CSC) $(CSC_FLAGS) -J -c $< -o $*.o
	rm $*.o

clean:
	- rm $(PROGRAM) *.import.scm

.PHONY: install uninstall
install: all
	mkdir -p "$(INSTALL_PREFIX)/bin/"
	mkdir -p "$(SYNTAX_INSTALL_PATH)"
	cp $(PROGRAM) "$(INSTALL_PREFIX)/bin/"
	cp $(SYNTAX_FILES) "$(SYNTAX_INSTALL_PATH)"

uninstall:
	rm "$(INSTALL_PREFIX)/bin/$(PROGRAM)"
	rm -rfv "$(SYNTAX_INSTALL_PATH)"
	rmdir --ignore-fail-on-non-empty "$(INSTALL_PREFIX)/bin/" \
	  "$(INSTALL_PREFIX)/share/"
