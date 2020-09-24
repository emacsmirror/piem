.POSIX:

EMACS   = emacs
# Rely on EMACSLOADPATH for everything but the current directory.
BATCH   = $(EMACS) --batch -Q -L .

EL = piem.el piem-b4.el piem-elfeed.el piem-eww.el piem-gnus.el \
     piem-maildir.el piem-notmuch.el
ELC = $(EL:.el=.elc)

all: compile piem.info piem-autoloads.el

compile: $(ELC)

piem-autoloads.el: $(EL)
	$(BATCH) -l package --eval \
	  '(package-generate-autoloads "piem" default-directory)'

clean:
	rm -f piem.info piem.html piem-autoloads.el $(ELC)
	rm -rf html/

docs: piem.html piem.info
	rm -rf html/
	makeinfo --html -o html/ -c TOP_NODE_UP_URL=/ piem.texi

piem-b4.elc: piem-b4.el piem.elc
piem-elfeed.elc: piem-elfeed.el piem.elc
piem-eww.elc: piem-eww.el piem.elc
piem-gnus.elc: piem-gnus.el piem.elc
piem-maildir.elc: piem-maildir.el
piem-notmuch.elc: piem-notmuch.el piem.elc
piem.elc: piem.el piem-maildir.elc

.SUFFIXES: .el .elc .texi .info .html

.el.elc:
	$(BATCH) -f batch-byte-compile $<

.texi.info:
	makeinfo $<

.texi.html:
	makeinfo --html -c TOP_NODE_UP_URL=/ --no-split $<
