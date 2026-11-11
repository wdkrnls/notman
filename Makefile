# notman -- Makefile
#
# Development (needs guile and guile-xapian, as from `guix shell`):
#   make check             compile-check both scripts
#   make index             incremental index into ./man.db
#   make rebuild           discard and rebuild ./man.db
#   make search Q='...'    query ./man.db
#   make render P=...      print a page as man produces it
#   make sections          list the section headings in ./man.db
#
# Installation (defaults to PREFIX=/usr/local, so usually `sudo make install`;
# `make install PREFIX=$HOME/.local` installs into your home instead):
#   make install
#   make uninstall
#
# Scripts go in $(LIBDIR) ($(PREFIX)/lib/notman).  No data is installed: the
# user builds the database later, at
# ${XDG_DATA_HOME:-$HOME/.local/share}/notman/man.db, so nothing under PREFIX
# is created or removed by the index, and uninstall cannot delete it.
#
# DESTDIR is honoured for staged installs.

GUILE ?= guile
GUIX  ?= guix shell guile guile-xapian --
DB    ?= man.db
Q     ?=
P     ?=
NOTMAN_LANG ?=

PREFIX  ?= /usr/local
BINDIR  ?= $(PREFIX)/bin
LIBDIR  ?= $(PREFIX)/lib/notman
DOCDIR  ?= $(PREFIX)/share/doc/notman

SRC      := man-index.scm man-search.scm
SIDECARS := $(DB) $(DB).manifest $(DB).sections $(DB).stem

.PHONY: all check index rebuild search render sections install uninstall clean

all: check

# Compile both scripts so unbound-variable warnings surface before you run.
check:
	$(GUIX) guild compile $(SRC)

# Incremental: only pages whose mtime or size changed are re-rendered.
index:
	$(GUIX) $(GUILE) man-index.scm $(DB) $(if $(NOTMAN_LANG),--lang=$(NOTMAN_LANG),)

# Full: required after any change to prefixes, idterms, headings, or the
# file filter.  Wipes every sidecar, including .stem.
rebuild:
	rm -rf $(SIDECARS)
	$(GUIX) $(GUILE) man-index.scm $(DB) $(if $(NOTMAN_LANG),--lang=$(NOTMAN_LANG),)

# make search Q='name:thread-based'   make search Q='--point lisp'
# make search Q='has:reporting_bugs'  make search Q='--file-path --point lisp'
search:
	$(GUIX) $(GUILE) man-search.scm $(DB) $(Q)

# make render P=/usr/share/man/man1/ls.1.gz -- inspect offsets by eye
render:
	$(GUIX) $(GUILE) man-index.scm --render $(P)

sections:
	@wc -l < $(DB).sections
	@sort $(DB).sections | column -c 100

# The dispatcher goes in bin/ and finds the .scm files through
# ../lib/notman, so no paths are baked in and this works from any prefix.
install:
	install -d "$(DESTDIR)$(BINDIR)" "$(DESTDIR)$(LIBDIR)" "$(DESTDIR)$(DOCDIR)"
	install -m 755 notman "$(DESTDIR)$(BINDIR)/notman"
	install -m 644 $(SRC) "$(DESTDIR)$(LIBDIR)/"
	install -m 644 README.md "$(DESTDIR)$(DOCDIR)/README.md"

uninstall:
	rm -f "$(DESTDIR)$(BINDIR)/notman"
	rm -rf "$(DESTDIR)$(LIBDIR)" "$(DESTDIR)$(DOCDIR)"

clean:
	rm -rf $(SIDECARS) *.go
