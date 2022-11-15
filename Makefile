CASK  ?= cask
WGET  ?= wget
EMACS  = emacs

EMACSFLAGS =
EMACSBATCH = $(EMACS) --batch -Q $(EMACSFLAGS)

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS := $(shell EMACS=$(EMACS) $(CASK) files)
OBJS  = $(SRCS:.el=.elc)

.PHONY: all compile test clean

all: compile

compile: $(OBJS)

test:
	$(CASK) exec $(EMACSBATCH) -L . -l sudo-edit-test.el -f ert-run-tests-batch-and-exit

README.md: make-readme-markdown.el sudo-edit.el
	$(CASK) exec $(EMACSBATCH) --script $< <sudo-edit.el >$@ 2>/dev/null

make-readme-markdown.el:
	$(WGET) -q -O $@ "https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el"

.INTERMEDIATE: make-readme-markdown.el

clean: clean-elc

clean-elc:
	$(CASK) clean-elc

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)
