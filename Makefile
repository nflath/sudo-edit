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

clean: clean-elc

clean-elc:
	$(CASK) clean-elc

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)
