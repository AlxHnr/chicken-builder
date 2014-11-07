# Chicken Builder

This bundle contains a small build system and a convenient wrapper around
[CHICKEN Scheme's](http://call-cc.org) module system. It is very
lightweight and was intended to be distributed with your project to avoid
additional dependencies.

## Usage
### Adding chicken-builder to your project

In order to use chicken-builder, you must add it to your project. One way
to do so, is to include it as a git submodule:

```sh
git submodule add https://github.com/AlxHnr/chicken-builder
```

Another way would be to add download the files directly into your project
directory:

```sh
wget https://github.com/AlxHnr/chicken-builder/archive/master.zip
unzip master.zip
mv chicken-builder-master chicken-builder
rm master.zip chicken-builder/README.md
```

### Project structure

Your project must be structured like this:

* some-project/
  * src/
  * test/
  * build/
  * Makefile

The **src/** directory contains your code. Chicken-builder will search this
directory for [ch-modules](#ch-module) and [ch-programs](#ch-program), to
generate **build/extra.makefile**. This extra makefile contains all the
rules needed to build your project and can be included by your main
Makefile.

The **test/** directory contains your unit tests. This feature implicitly
depends on the [test](http://wiki.call-cc.org/eggref/4/test) egg. Before
any tests are run, chicken-builder will create the directory **test/tmp/**.
This directory can by used by unit tests to create temporary files. If all
tests pass, **test/tmp/** will be removed again, so make sure not to store
important data there. See [ch-test](#ch-test) for more informations.

The **build/** directory will be created the first time you generate the
extra makefile. It contains only build specific data which may be
overwritten or removed.

The **Makefile** is the main makefile which will build your project. The
only thing it needs to do, is to generate **build/extra.makefile** and
include it. The main makefile can be used to extend the extra makefile, or
even add custom rules. Here an example:

```make
# Optional flags, which are passed to csc.
CSC_FLAGS   = -O3
CSC_LDFLAGS = -lSomeLib

# This is the mandatory bit, which builds the extra makefile and includes
# it. But consider that this will resolve the dependencies only once. So to
# regenerate them, you must eiter clean and rebuild your project, or run
# "chicken-builder/generate-extra-makefile.scm" manually.
-include build/extra.makefile
build/extra.makefile:
	csi -s chicken-builder/generate-extra-makefile.scm
```

### Building

Now you can build your project using **make** or **make all**. Additionally
you can run **make test** and **make clean**, which will either test or
clean up your build.

## Module wrapper

Chicken-builder adds the following syntactical extensions to simplify the
usage of CHICKEN's build-in module system. You can only have either
ch-module, ch-program or ch-test once in a source file.

### ch-module

This can be used is pretty much like a normal CHICKEN module. The
difference it, that each ch-module is also a CHICKEN unit. The dependency
generator will search your **src/** directory for such definitions and
generate rules to build object files from them. The build process also
ensures that all dependencies are handled properly, and takes care of
\*.import.scm and \*.types files. Every ch-module will implicitly import
chicken, scheme and ch-syntax. Ch-modules must be in the **src/**
directory. The module's name must be the name of the file with its
extension removed, and can't be named "main" or "ch-syntax".

```scheme
(ch-module NAME (EXPORT ...)
  ...)
```

### ch-program

This is similar to [ch-module](#ch-module), but with some differences: It
has no module name and the source file will not be treated as a unit.
Neither \*.import.scm nor \*.types files will be created for ch-programs.
Chicken-builder will build stand alone executables from ch-programs.

```scheme
(ch-program
  ...)
```

### ch-test

Ch-test is like [ch-program](#ch-program), but with the difference that one
must specify the name of the test as a string. Ch-tests must be in the
**test/** directory. Ch-test will implicitly use the
[test](http://wiki.call-cc.org/eggref/4/test) egg and wrap your code into a
test-begin and test-end block. It will also call test-exit for you.

```scheme
(ch-test "test name"
  ...)
```

### ch-import

This is how you tell chicken-builder to recognize dependencies for building
and linking. Ch-import combines CHICKEN's import and uses declarations with
automatic dependency resolving. Circular dependencies are forbidden.
Ch-import is only available inside ch-modules, ch-programs or ch-tests.

```scheme
(ch-module foo
  (ch-import bar foobar)
  ...)
```

## License

Chicken-builder was released under the zlib license.
