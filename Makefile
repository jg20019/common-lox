LISP ?= sbcl

all: clean build

clean:
	rm -rf ~/.lisp-bin/hn

build:
	$(LISP) \
		--eval '(ql:quickload :common-lox/bin)' \
		--eval '(asdf:make :common-lox/bin)'
