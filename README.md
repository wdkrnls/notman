# notman

Indexed full-text search over the manual pages on your system.

`notman` renders every page once, stores it in a Xapian database along with
the character offset of each section heading, and searches it.  Because the
offsets refer to notman's own render, a hit can be reopened at exactly the
right character — which makes an editor front-end possible.

## Requirements

* Guile 3.0 and guile-xapian
* man-db (`man`, `manpath`), coreutils (`cksum`), findutils (`find`, `xargs`)

## Installing

With Guix:

```
    guix install notman          # from the notman channel; see README there
```

Note that the repository ships with a guix-package.scm file which you can use to install it via:

```
    guix package -f path/to/notman/guix-package.scm
```

Without Guix, from a checkout:

```
    make install PREFIX=$HOME/.local
```

`guile` and `guile-xapian` must be visible to `guile` at run time, which
normally means installing them into the same prefix or having them on
`GUILE_LOAD_PATH`.  `DESTDIR` is honoured for staged installs.

## Usage

```
    notman index  [--rebuild] [--lang=LANG] [--man-root=PATH]
                  [--file=PATH[,PATH,...]] [--db-path=DIR]
    notman search [--context[=N]] [--point] [--file-path] [--debug]
                  [--db-path=DIR] QUERY...
    notman paths
    notman render PATH
```

`notman index` is incremental: a page is re-rendered only if its compressed
size or cksum changed.  `--rebuild` discards the database first, which is
the only way to remove documents for pages that have disappeared.

## Query syntax

* bare words search the whole page
* `name:printf`, `see_also:git`: search inside one section
* `has:environment`: pages that contain that section, listed by their NAME
  line rather than by a match
* `section:3`: the manual section number

## Where the database lives

`$XDG_DATA_HOME/notman` (that is, `~/.local/share/notman`), overridable with
`--db-path=DIR` or `$NOTMAN_DB_DIR`.  It is data, not a cache: a full index
takes minutes, so nothing should be free to delete it.

## Notes on the design

Pages are rendered exactly once, with `MANWIDTH=80 man --nh -Tutf8 -l`, and
every stored offset is a character offset into that text.  `--nh` matters:
with hyphenation on, a word can be split across a line break and become
unfindable.  The manifest beside the database records each page's size and
cksum together with an `index-version`, so a change to how documents are
built discards the memo and triggers a full reindex rather than leaving the
database half old and half new.

## Future plans

Allowing the user to specify a `MANWIDTH` other than 80 characters sounds somewhat appealing, although this has to be done on index. This makes changing the width quite expensive in practice. That is why it hasn't been done already.
