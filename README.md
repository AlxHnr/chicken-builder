# Chicken Builder

This bundle contains a makefile generator and a convenient wrapper around
[CHICKEN Scheme's](http://call-cc.org) module system. The module wrapper
simplifies CHICKEN's module syntax and is less ambiguous, so the makefile
generator can derive a dependency tree from your source code.

## Usage

After successful [installation](#requirements-and-installation), you must
chdir into a directory, which contains a project conforming to the required
[project structure](#project-structure). To initialize your build and
generate an extra makefile, simply run `chicken-builder-init`. This will
create the file "build/extra.makefile", which must be included by your main
makefile.

Now you can build your project using `make`. The _all_, _test_ and _clean_
targets are defined in the extra.makefile.

To prevent chicken-builder from generating rules for specific targets you
can disable them via the `--skip-targets=...` argument. You can separate
multiple build targets using commas. Here are some examples:

```sh
chicken-builder-init --skip-targets=test,clean --skip-targets=foo
chicken-builder-init --skip-targets=build/foo.o,build/test/string

# Objects which directly belong to a program or a test can only be disabled
# via the program or test itself:

chicken-builder-init --skip-targets=build/main-program

# This will disable build/main-program and build/main-program.o.
```

## Requirements and installation

[CHICKEN Scheme](http://call-cc.org) must be installed on your system.
After cloning this repository and changing into its directory, you can
build and install it with the following commands:

```sh
make
make install # Eventually this command must be executed as root.
```

By default it will be installed to `/usr/local`, but you can change the
installation path by setting *INSTALL_PREFIX*. This variable _must_ be set
before building Chicken-builder.

### Uninstallation

Uninstallation is pretty much like installation. Just make sure, that
*INSTALL_PREFIX* is setup exactly like during its installation. Then run
`make uninstall` from Chicken-builder's source directory.

## Project structure

Your project must be structured like this:

* some-project/
  * src/
  * test/
  * build/
  * Makefile

The **src/** directory contains your code. Chicken-builder will search this
directory for [chb-modules](#chb-module) and [chb-programs](#chb-program)
to generate the extra makefile. This makefile contains all the rules needed
to build your project and can be included by your main Makefile.

The **test/** directory contains your unit tests. This feature implicitly
depends on the [test](http://wiki.call-cc.org/eggref/4/test) egg. Before
any tests are run, chicken-builder will create the directory **test/tmp/**.
This directory can by used by unit tests to create temporary files. If all
tests pass, **test/tmp/** will be removed again, so make sure not to store
important data there. See [chb-test](#chb-test) for more informations.

The **build/** directory will be created the first time you generate the
extra makefile. It contains only build specific data which may be
overwritten or removed.

The **Makefile** is the main makefile which will build your project.

## Module wrapper

Chicken-builder adds the following syntactical extensions to simplify the
usage of CHICKEN's build-in module system. You can only have either
chb-module, chb-program or chb-test once in a source file.

### chb-module

This is pretty much like a normal CHICKEN module. The difference is, that
each chb-module is also a CHICKEN unit. The dependency generator will
search your **src/** directory for chb-modules and generate rules to build
them. The build process also ensures that all dependencies are handled
properly, and takes care of \*.import.scm and \*.types files. Every
chb-module will implicitly import chicken, scheme and chb-syntax.
Chb-modules must be in the **src/** directory. The module's name must be
the name of the file with its extension removed and can't be named "main"
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
