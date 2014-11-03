; Copyright (c) 2014 Alexander Heinrich <alxhnr@nudelpost.de>
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
;    1. The origin of this software must not be misrepresented; you must
;       not claim that you wrote the original software. If you use this
;       software in a product, an acknowledgment in the product
;       documentation would be appreciated but is not required.
;
;    2. Altered source versions must be plainly marked as such, and must
;       not be misrepresented as being the original software.
;
;    3. This notice may not be removed or altered from any source
;       distribution.

(use extras posix irregex srfi-1 srfi-13 srfi-69)

;; Display an error message and exits with failure.
(define-syntax die
  (syntax-rules ()
    ((_ messages ...)
     (begin
       (print "Error: " messages ...)
       (exit 1)))))

;; Returns a list with all .scm files in a given directory. The path to the
;; directory must end with a slash ("/"). If the directory does not exist,
;; it returns an empty list.
(define (get-scheme-files dirname)
  (if (directory? dirname)
    (map
      (lambda (filename)
        (string-append dirname filename))
      (filter
        (lambda (filename)
          (string-suffix-ci? ".scm" filename))
        (directory dirname)))
    '()))

;; Returns the path without the file including the trailing slash.
(define get-filepath
  (let ((splitter (irregex "^(.+\\/)[^\\/]+$")))
    (lambda (path)
      (irregex-replace splitter path 1))))

;; Returns the filename with its path and extension removed.
(define get-filename-stem
  (let ((splitter (irregex "^(.*\\/)?(.+)\\.scm$")))
    (lambda (filename)
      (irregex-replace splitter filename 2))))

;; Takes a list and collects all cdrs from all sublists whose first element
;; is 'ch-import.
(define (get-imports lst)
  (let collect-imports ((cdr-list '()) (rest lst))
    (if (null? rest)
      cdr-list
      (collect-imports
        (if (and (list? (car rest)) (eq? 'ch-import (caar rest)))
          (append cdr-list (cdar rest))
          cdr-list)
        (cdr rest)))))

;; Returns true, if a list is a valid ch-target body.
(define (is-ch-target? lst)
  (and (list? lst)
       (or (eq? (car lst) 'ch-module)
           (eq? (car lst) 'ch-program)
           (eq? (car lst) 'ch-test))))

;; These hash tables associate targets with a list of symbols describing
;; their dependencies.
(define programs (make-hash-table))
(define modules (make-hash-table))
(define tests (make-hash-table))

;; Populate the hash tables defined above by collecting all targets from
;; src/ and test/.
(for-each
  (lambda (filename)
    (define target-body (find is-ch-target? (read-file filename)))
    (when target-body
      (define target-type (car target-body))
      (define target-name
        (string->symbol (get-filename-stem filename)))
      (define filepath (get-filepath filename))
      (define dep-table
        ; Get the correct hash table and die on any kind of mismatches.
        (cond
          ((and (string=? filepath "src/") (eq? target-type 'ch-program))
           programs)
          ((and (string=? filepath "src/") (eq? target-type 'ch-module))
           (cond
             ((eq? (cadr target-body) 'main)
              (die filename ": A module cannot be named \"main\"."))
             ((not (eq? (cadr target-body) target-name))
              (die filename ": The module name \"" (cadr target-body)
                   "\" must be \"" target-name "\".")))
           modules)
          ((and (string=? filepath "test/") (eq? target-type 'ch-test))
           tests)
          (else
            (die
              filename ": the target type \"" target-type
              "\" does not belong into the \"" filepath "\" directory."))))
      (hash-table-set! dep-table target-name (get-imports target-body))))
  (append (get-scheme-files "src/") (get-scheme-files "test/")))

;; Gets the full, recursive dependency list for a target, containing of all
;; dependencies and their dependencies. The first argument is the name
;; (symbol) of the target, for which the dependencies should be determined.
;; If it is 'main', it will never result in a circular dependency error.
;; The second argument is the list containing the targets direct
;; dependencies. This function will exit this script if it detects circular
;; dependencies.
(define (get-recursive-dependencies target-name root-deps)
  (define dep-table (make-hash-table))
  ; Populate the dep-table recursively by adding dependencies and their
  ; dependencies.
  (let add-deps ((deps root-deps))
    (for-each
      (lambda (dep)
        ; Search for circular dependencies. Modules cannot depend on
        ; programs/tests and they cannot be named main. Thus circular
        ; dependencies can only be detected if the search starts from a
        ; module.
        (if (eq? dep target-name)
          (die "Module \"" dep "\" results in a circular dependency."))
        (unless (hash-table-ref/default dep-table dep #f)
          (hash-table-set! dep-table dep #t)
          (condition-case
            (add-deps (hash-table-ref modules dep))
            ((exn)
             (print "error: module '" dep "' does not exist.")
             (exit 1)))))
      deps))
  (hash-table-keys dep-table))

;; Builds a string by appending a list of symbols. Each symbol will be
;; converted to a string and pre- and suffixed by given substrings.
(define (append-symbol-list lst prefix suffix)
  (fold
    (lambda (sym str)
      (string-append str " " prefix (symbol->string sym) suffix))
    "" lst))

;; Builds the head of a makefile target. It takes the target name, the
;; path to the source ending with a slash, and the path to the build
;; directory for this target ending with a slash. The last argument is a
;; list of dependencies.
(define (build-common-rule-head name src-path build-path deps)
  (string-append
    build-path name ".o: " src-path name ".scm |"
    (if (string=? build-path "build/")
      "" (string-append " " build-path))
    " build/ch-syntax.import.scm" (append-symbol-list deps "build/" ".o")))

;; Builds the body of a makefile target. It takes a string of extra targets
;; which will be passed additionally to $(CSC). It can be empty.
(define (build-common-rule-body name extra-args)
  (string-append
    "\tcd build/ && $(CSC) $(CSC_FLAGS) -prologue"
    " ../chicken-bundle/ch-target.scm \\\n\t\t" extra-args
    " -c ../$< -o ../$@\n"))

;; Builds the rules needed to build an entire program. See
;; 'build-common-rule-head' for further explanation of the arguments.
(define (build-program-rule name src-path build-path deps)
  (string-append
    build-path name ": " build-path name ".o"
    (append-symbol-list
      (get-recursive-dependencies 'main deps) "build/" ".o")
    "\n\t$(CSC) $(CSC_LDFLAGS) $^ -o $@\n\n"
    (build-common-rule-head name src-path build-path deps) "\n"
    (build-common-rule-body
      name (append-symbol-list deps "-types " ".types"))))

;; Builds the rules needed to build all programs in a given table and
;; writes them to an output port. It takes the path to the source and the
;; build directory of the program. Both paths can either be empty or must
;; end with a slash.
(define (write-program-rules table src-path build-path out)
  (hash-table-for-each table
    (lambda (program deps)
      (write-line
        (build-program-rule
          (symbol->string program) src-path build-path deps)
        out))))

;; Writes the rules needed to build all modules in a given table to an
;; output port.
(define (write-module-rules table out)
  (hash-table-for-each table
    (lambda (mod deps)
      (define name (symbol->string mod))
      (define type-flags (append-symbol-list deps "-types " ".types"))
      (write-line
        (string-append
          (build-common-rule-head name "src/" "build/" deps)
          "\n\tcd build/ && $(CSC) -A -prologue"
          " ../chicken-bundle/ch-target.scm -specialize -strict-types"
          " \\\n\t\t-local ../$<" type-flags " -emit-type-file " name
          ".types\n"
          (build-common-rule-body
            name (string-append type-flags " -J -unit " name)))
        out))))

; Generate recursive dependency lists for all modules, to provoke circular
; dependency errors.
(hash-table-for-each modules
  (lambda (key value)
    (get-recursive-dependencies
      key (hash-table-ref modules key))))

; Generate the dependency file.
(call-with-output-file "dependencies.makefile"
  (lambda (out)
    (write-program-rules programs "src/" "build/" out)
    (write-module-rules modules out)
    (write-program-rules tests "test/" "build/test/" out)))
