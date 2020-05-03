
BATCH = emacs -Q --batch

all: piem.elc piem.info

.PHONY: clean
clean:
	$(RM) *.elc *-autoloads.el *.info

%.info: %.texi
	makeinfo $<

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

%-autoloads.el: %.el
	$(BATCH) --eval \
	"(let ((make-backup-files nil)) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"
