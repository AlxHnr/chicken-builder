# Chicken Bundle

This bundle contains various syntactical extensions and a small build
system for CHICKEN Scheme. It is very lightweight and it was intended to be
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
unset MASTER_PATH && cd ../
```

### Project structure

Your project should have a src/ directory. It will be scanned to generate
the dependency file, which will be used by the Makefile. See
[Features](#features) for more informations about how to make the build
process aware of your source files.

Your project may also have a test/ directory, which will be scanned to
generate rules for building and running unit tests. Unit tests require the
[test](http://wiki.call-cc.org/eggref/4/test) egg. See
[Features](#features) for more informations.

When building the project, the file "dependencies.makefile" will be placed
in the root dir of your project. The build process also involves the
creation of a temporary build/ directory. Do not store any important files
there directory, otherwise they will be overwritten or wiped while
building.

### Building

Now you can build your project using `make` or `make all`. Additionally you
can run `make test` and `make clean`, which are pretty self explaining.

To regenerate the dependencies, you must clean and build your project
again.

## Features

Chicken-bundle adds the following syntactical extensions. You can only have
either ch-module, ch-program or ch-test once in a source file.

### ch-module

It is comparable to CHICKEN's build-in module, but is much simpler. It also
unifies modules with units. The dependency generator will search your src/
directory for such definitions and generate rules to build stand alone
compilation units from them. The build process also ensures that all
dependencies are handled properly, and that \*.import.scm and \*.types
files are created and used. Every ch-module will automatically import
chicken, scheme and ch-syntax. Ch-modules must be in the src/ directory.
The module name must be the name of the file with its extension removed and
can't be named "main".

```scheme
(ch-module module-name
  ...)
```

### ch-program

This is comparable to [ch-module](#ch-module), but with the difference that
the source file will not be treated as an unit. \*.import.scm and \*.types
files will not be generated for ch-programs. Every ch-program will be build
and linked into "build/file-name", where filename is the name of the file
with its extension removed. Ch-programs must be in the src/ directory.

```scheme
(ch-program
  ...)
```

### ch-test

Ch-test is like [ch-program](#ch-program), but must be in the test/
directory. Ch-test will implicitly use the test egg and wrap your code into
a test-begin and test-end block. It will also call test-exit for you.

```scheme
(ch-test
  ...)
```

### ch-import

Combines CHICKEN's (import ...) and (declare (uses ...)) with automatic
dependency resolving. This is how you tell chicken-bundle to recognize,
build and link dependencies. "ch-import" is only available inside
ch-modules, ch-programs or ch-tests.

```scheme
(ch-module foo
  (ch-import bar foobar)
  ...)
```

### function

This is a wrapper around (define (function arg1 arg2 ...)) which will be
exported implicitly from the module. It also allows you to annotate types.
Just see the example below. If you annotate types, the function relies on
the assumption that you actually pass correctly typed values or it will
lead to undefined behaviour. But as long as you use only chicken-bundle
specific constructs to build your project you should get at least type
warnings during compilation. "function" is only available inside
ch-modules, ch-programs or ch-tests.

```scheme
(function foo (name age)
  (print name " is " age " years old")
  ...)

# Annotate argument types.
(function foo ((name string) (age fixnum))
  (print name " is " age " years old")
  ...)

# Annotate return and argument types.
(function (foo string) ((name string) (age fixnum))
  (string-append name " is " (number->string age) " years old"))
```

## License

The Makefile is public domain. All files in the subdirectory
"chicken-bundle/" are released under the zlib license. This includes
syntactical extensions and the dependency generator.