
-include config.mk

# Rely on EMACSLOADPATH for everything but the current directory.
BATCH = emacs -Q --batch -L .

all: loadpath piem.info piem.elc \
	piem-b4.elc piem-elfeed.elc piem-eww.elc \
	piem-gnus.elc piem-notmuch.elc

piem-autoloads.el:
	$(BATCH) -l package --eval \
	  '(package-generate-autoloads "piem" default-directory)'

.PHONY: clean
clean:
	$(RM) *.elc *-autoloads.el *.info

.PHONY: loadpath
loadpath:
	@echo ";;; EMACSLOADPATH=$(EMACSLOADPATH)"

piem-b4.elc: piem-b4.el piem.elc
piem-elfeed.elc: piem-elfeed.el piem.elc
piem-eww.elc: piem-eww.el piem.elc
piem-gnus.elc: piem-gnus.el piem.elc
piem-notmuch.elc: piem-notmuch.el piem.elc
piem.elc: piem.el

%.info: %.texi
	makeinfo $<

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<
