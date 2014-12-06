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
directory for [chb-modules](#chb-module) and [chb-programs](#chb-program),
to generate "build/extra.makefile". This extra makefile contains all the
rules needed to build your project and can be included by your main
Makefile.

The **test/** directory contains your unit tests. This feature implicitly
depends on the [test](http://wiki.call-cc.org/eggref/4/test) egg. Before
any tests are run, chicken-builder will create the directory **test/tmp/**.
This directory can by used by unit tests to create temporary files. If all
tests pass, **test/tmp/** will be removed again, so make sure not to store
important data there. See [chb-test](#chb-test) for more informations.

The **build/** directory will be created the first time you generate the
extra makefile. It contains only build specific data which may be
overwritten or removed.

The **Makefile** is the main makefile which will build your project. The
only thing it needs to do, is to generate "build/extra.makefile" and
include it. The main makefile can be used to extend the extra makefile, or
even add custom rules. Here an example:

```make
# Optional flags, which are passed to csc.
CSC_FLAGS   = -O3
CSC_LDFLAGS = -lSomeLib

# This is the mandatory part, which builds the extra makefile and includes
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
chb-module, chb-program or chb-test once in a source file.

### chb-module

This can be used is pretty much like a normal CHICKEN module. The
difference is, that each chb-module is also a CHICKEN unit. The dependency
generator will search your **src/** directory for chb-modules and generate
rules to build them. The build process also ensures that all dependencies
are handled properly, and takes care of \*.import.scm and \*.types files.
Every chb-module will implicitly import chicken, scheme and chb-syntax.
Chb-modules must be in the **src/** directory. The module's name must be
the name of the file with its extension removed, and can't be named "main"
or "chb-syntax".

```scheme
(chb-module NAME (EXPORT ...)
  ...)
```

### chb-program

This is similar to [chb-module](#chb-module), but with some differences: It
has no module name and the source file will not be treated as a unit.
Neither \*.import.scm nor \*.types files will be created for chb-programs.
Chicken-builder will build stand alone executables from chb-programs.

```scheme
(chb-program
  ...)
```

### chb-test

Chb-test is like [chb-program](#chb-program), but with the difference that one
must specify the name of the test as a string. Chb-tests must be in the
**test/** directory. Chb-test will implicitly use the
[test](http://wiki.call-cc.org/eggref/4/test) egg and wrap your code into a
test-begin and test-end block. It will also call test-exit for you.

```scheme
(chb-test "test name"
  ...)
```

### chb-import

This is how you tell chicken-builder to recognize dependencies for building
and linking. Chb-import combines CHICKEN's _import_ and _uses_ declarations
with automatic dependency resolving. Circular dependencies are forbidden.
Chb-import is only available inside chb-modules, chb-programs or chb-tests.

```scheme
(chb-module foo
  (chb-import bar foobar)
  ...)
```

## License

Chicken-builder was released under the zlib license.
