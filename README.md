# Chicken Bundle

This bundle contains various syntactical extensions and a small build
system for CHICKEN Scheme. It is very lightweight and was intended to be
distributed with your project to avoid additional dependencies.

## Usage
### Adding chicken-bundle bundle to your project

First you need to download the bundle into your project directory:

```sh
cd your-chicken-project/

MASTER_PATH="https://raw.githubusercontent.com/AlxHnr/chicken-bundle/master"
curl -O "$MASTER_PATH/Makefile"

mkdir -p chicken-bundle/ && cd chicken-bundle/
curl -O "$MASTER_PATH/chicken-bundle/ch-syntax.scm"
curl -O "$MASTER_PATH/chicken-bundle/ch-target.scm"
curl -O "$MASTER_PATH/chicken-bundle/generate-dependencies.scm"
unset MASTER_PATH && cd -
```

### Project structure

Your project should have a src/ directory. It will be scanned to generate
the dependency file. See [features](#features) for more informations about
how to make the build process aware of your source files.

Your project may also have a test/ directory, which will be scanned to
generate rules for building and running unit tests. Unit tests require the
[test](http://wiki.call-cc.org/eggref/4/test) egg. See [ch-test](#ch-test)
for more informations.

When building the project, the file "dependencies.makefile" will be placed
in the root directory of your project. The build process also involves the
creation of a temporary build/ directory. Do not store any important files
there directory, otherwise they may be overwritten or deleted.

Before tests are run, chicken-bundle will create a directory named tmp/
inside the test/ directory. If all tests pass, test/tmp/ will be removed
again, so make sure not to store important data there. Unit tests are free
to create arbitrary data inside test/tmp/.

### Building

Now you can build your project using `make` or `make all`. Additionally you
can run `make test` and `make clean`, which will either test or clean up
your build.

Dependencies are only generated on the first time you build. To regenerate
them, you must clean and rebuild the project.

## Features

Chicken-bundle adds the following syntactical extensions. You can only have
either ch-module, ch-program or ch-test once in a source file.

### ch-module

It is comparable to CHICKEN's build-in module, but is much simpler. It
unifies modules with units. The dependency generator will search your src/
directory for such definitions and generate rules to build stand alone
compilation units from them. The build process also ensures that all
dependencies are handled properly, and that \*.import.scm and \*.types
files are created and used. Every ch-module will automatically import
chicken, scheme and ch-syntax. Ch-modules must be in the src/ directory.
The module name must be the name of the file with its extension removed. A
module can't be named "main".

```scheme
(ch-module module-name
  ...)
```

### ch-program

This is comparable to [ch-module](#ch-module), but with some differences:
the source file will not be treated as a unit and neither \*.import.scm nor
\*.types files will be emitted. Ch-programs must be in the src/ directory.

```scheme
(ch-program
  ...)
```

### ch-test

Ch-test is like [ch-program](#ch-program), but with the difference that it
takes the test name as an argument. Ch-tests must be in the test/
directory. Ch-test will implicitly use the test egg and wrap your code into
a test-begin and test-end block. It will also call test-exit for you.

```scheme
(ch-test "test name"
  ...)
```

### ch-import

Combines CHICKEN's import and (declare (uses ...)) statements with
automatic dependency resolving. This is how you tell chicken-bundle to
recognize dependencies for building and linking. Circular dependencies are
forbidden. 'ch-import' is only available inside ch-modules, ch-programs or
ch-tests.

```scheme
(ch-module foo
  (ch-import bar foobar)
  ...)
```

## License

The Makefile is public domain. All files in the subdirectory
"chicken-bundle/" are released under the zlib license. This includes
syntactical extensions and the dependency generator.
