.POSIX:

EMACS   = emacs
# Rely on EMACSLOADPATH for everything but the current directory.
BATCH   = $(EMACS) --batch -Q -L . -L tests

EL = piem.el piem-b4.el piem-elfeed.el piem-eww.el piem-gnus.el \
     piem-lei.el piem-maildir.el piem-notmuch.el piem-rmail.el \
     tests/piem-rmail-tests.el tests/piem-tests.el
ELC = $(EL:.el=.elc)

all: compile Documentation/piem.info piem-autoloads.el

compile: $(ELC)

check: test
test: $(ELC)
	$(BATCH) -l tests/piem-tests.elc -f ert-run-tests-batch

piem-autoloads.el: $(EL)
	$(BATCH) -l package --eval \
	  '(package-generate-autoloads "piem" default-directory)'

clean:
	rm -f Documentation/piem.info Documentation/piem.html piem-autoloads.el
	rm -f $(ELC)
	rm -rf html/

docs: Documentation/piem.html Documentation/piem.info
	rm -rf Documentation/html/
	makeinfo --html --css-ref=../manual.css -o Documentation/html/ \
		-c TOP_NODE_UP_URL=/ Documentation/piem.texi

piem-b4.elc: piem-b4.el piem.elc
piem-elfeed.elc: piem-elfeed.el piem.elc
piem-eww.elc: piem-eww.el piem.elc
piem-gnus.elc: piem-gnus.el piem.elc
piem-lei.elc: piem-lei.el piem.elc
piem-maildir.elc: piem-maildir.el
piem-notmuch.elc: piem-notmuch.el piem.elc
piem-rmail.elc: piem-rmail.el piem.elc
piem.elc: piem.el piem-maildir.elc
tests/piem-rmail-tests.elc: tests/piem-rmail-tests.el piem-rmail.elc
tests/piem-tests.elc: tests/piem-tests.el piem.elc

.SUFFIXES: .el .elc .texi .info .html

.el.elc:
	$(BATCH) -f batch-byte-compile $<

.texi.info:
	makeinfo -o $@ $<

.texi.html:
	makeinfo --html --css-ref=manual.css -c TOP_NODE_UP_URL=/ --no-split \
		-o $@  $<
