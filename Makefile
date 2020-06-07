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
	rm -f piem.info piem-autoloads.el $(ELC)

piem-b4.elc: piem-b4.el piem.elc
piem-elfeed.elc: piem-elfeed.el piem.elc
piem-eww.elc: piem-eww.el piem.elc
piem-gnus.elc: piem-gnus.el piem.elc
piem-maildir.elc: piem-maildir.el
piem-notmuch.elc: piem-notmuch.el piem.elc
piem.elc: piem.el

.SUFFIXES: .el .elc .texi .info

.el.elc:
	$(BATCH) -f batch-byte-compile $<

.texi.info:
	makeinfo $<
