
-include config.mk

TRANSIENT_DIR ?= /dev/null

LOAD_PATH = -L . -L $(TRANSIENT_DIR)
BATCH = emacs -Q --batch $(LOAD_PATH)

all: piem.elc piem-b4.elc piem.info

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
