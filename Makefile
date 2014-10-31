CSC         := $(shell which csc)
CSC_LDFLAGS :=

CSC_FLAGS := -O3

PROGRAMS := ${shell grep -rl '^(ch-program' src/}
PROGRAMS := $(PROGRAMS:src/%.scm=build/%)

TESTS := ${shell grep -rl '^(ch-test' test/}
TESTS := $(TESTS:test/%.scm=build/test/%)

.SECONDARY: $(patsubst src/%.scm,build/%.o,$(wildcard src/*.scm))
.SECONDARY: $(patsubst test/%.scm,build/test/%.o,$(wildcard test/*.scm))

# Build targets.
.PHONY: all test clean
all: $(PROGRAMS)

-include dependencies.makefile
dependencies.makefile: generate-dependencies.scm
	csi -s $<

build/ch-syntax.import.scm: chicken-bundle/ch-syntax.scm | build/
	cd build/ && $(CSC) $(CSC_FLAGS) -J -c ../$< -o ch-syntax.o
	rm build/ch-syntax.o

build/ build/test/:
	mkdir -p $@

test: $(TESTS)
	mkdir -p test/tmp/
	@(for test in $(TESTS); do \
		"$$test" || exit; \
	done)
	rm -rf test/tmp/

clean:
	- rm dependencies.makefile
	- rm -rf build/
	- rm -rf test/tmp/