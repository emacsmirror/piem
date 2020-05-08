
-include config.mk

# Rely on EMACSLOADPATH for everything but the current directory.
BATCH = emacs -Q --batch -L .

all: loadpath piem.elc piem-b4.elc piem.info

.PHONY: clean
clean:
	$(RM) *.elc *-autoloads.el *.info

.PHONY: loadpath
loadpath:
	@echo ";;; EMACSLOADPATH=$(EMACSLOADPATH)"

%.info: %.texi
	makeinfo $<

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

%-autoloads.el: %.el
	$(BATCH) --eval \
	"(let ((make-backup-files nil)) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"
