
BATCH = emacs -Q --batch

all: piem.info

.PHONY: clean
clean:
	$(RM) *.info

%.info: %.texi
	makeinfo $<
