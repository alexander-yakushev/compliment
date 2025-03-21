# Compliment [![](https://img.shields.io/circleci/project/github/alexander-yakushev/compliment/master.svg)](https://circleci.com/gh/alexander-yakushev/compliment) [![](https://img.shields.io/codecov/c/github/alexander-yakushev/compliment.svg?token=FZTqeuK1hQ)](https://codecov.io/github/alexander-yakushev/compliment) [![](https://img.shields.io/badge/dependencies-none-brightgreen.svg)](https://versions.deps.co/alexander-yakushev/compliment) [![](https://img.shields.io/clojars/dt/compliment.svg?color=teal)](https://clojars.org/compliment) [![](https://img.shields.io/badge/-changelog-blue.svg)](CHANGELOG.md)

Compliment is a fast and smart completion library for Clojure. It can complete
[vars](https://github.com/alexander-yakushev/compliment/wiki/Examples#vars),
[namespaces](https://github.com/alexander-yakushev/compliment/wiki/Examples#namespaces),
[classes](https://github.com/alexander-yakushev/compliment/wiki/Examples#classes),
[class
members](https://github.com/alexander-yakushev/compliment/wiki/Examples#static-members),
[keywords](https://github.com/alexander-yakushev/compliment/wiki/Examples#keywords),
[locals](https://github.com/alexander-yakushev/compliment/wiki/Examples#local-bindings).
Users and library authors can also extend Compliment with custom completion
sources.

Compliment is used as a completion backend in the following editors/IDEs:

- Emacs — [CIDER](https://cider.readthedocs.io/en/latest/code_completion/)
- Vim — [vim-fireplace](https://github.com/tpope/vim-fireplace)
- Visual Studio Code — [Calva](https://calva.io/)
- [rebel-readline](https://github.com/bhauman/rebel-readline/)

Most importantly, I am very glad you came here. You look gorgeous today.

## Rationale

I wrote Compliment specifically for you because you are amazing and I believe
you deserve a matching completion lib. Here are the features it boasts:

- **Speed.** Your time is too precious to wait for completion to happen.
Compliment is designed to be fast and is carefully benchmarked to make sure no
sudden performance drops appear.
- **Smart completion.** Such a smart person like you is entitled to completion
being smart as well. Default Compliment sources use various techniques to give
more meaningful completion depending on the context, and allow some fuzziness in
prefix.
- **Extensibility.** Your insatiable passion for exploration won't be satisfied
by a set in stone completion list. For this reason Compliment allows every
library developer to write custom sources, so later other users of the library
will have better experience utilizing it.

## Installation

In most Clojure IDEs that use Compliment, you don't have to install anything at
all — it will already be there for you. In case you are a CIDER user you'll also
need to install [company-mode](http://company-mode.github.io/) and
[company-quickhelp](https://github.com/expez/company-quickhelp) for optimal
results. This [guide](https://docs.cider.mx/cider/usage/code_completion.html)
will help you configure it.

To embed Compliment directly into your program add this to the `:dependencies`:

[![](https://clojars.org/compliment/latest-version.svg)](https://clojars.org/compliment)

If you don't want to pull a dependency, check [Compliment-lite](lite).

## For users

Here are the
[examples](https://github.com/alexander-yakushev/compliment/wiki/Examples) of
different completion scenarios Compliment supports.

You can advise Compliment to provide special treatment for certain Vars by
attaching metadata to them. See the
[Metadata](https://github.com/alexander-yakushev/compliment/wiki/Metadata) page
for details.

## For developers

Compliment's primary public function is `completions`:

```clj
=> (require 'compliment.core)
=> (compliment.core/completions "compl")
({:candidate "complement", :type :function, :ns "clojure.core"}
 {:candidate "completing", :type :function, :ns "clojure.core"}
 {:candidate "compliment.core", :type :namespace, :file "compliment/core.clj"}
 {:candidate "compliment.utils", :type :namespace, :file "compliment/utils.clj"}
 ...)
```

`completions` can accept a map of options as a second argument. The supported options are:

- `:ns` — complete within the provided namespace
- `:context` — enable richer completions, see
  [Context](https://github.com/alexander-yakushev/compliment/wiki/Context).
- `:sort-order` — how to sort resulting candidates, either `:by-length`
  (default) or `:by-name`.
- `:extra-metadata` — a list of keywords for extra metadata to attach to each
  candidate. Only used for completing vars. The values can be `:doc`,
  `:arglists`, `:deprecated`, `:private`.
- `:sources` — a list of sources names that should be used when generating
  candidates. The name of the source is a keyword passed to `defsource` in each
  [source namespace](src/compliment/sources). If not provided, all sources will
  be used.

The second public function is `documentation`. It can be used for generating tooltip messages when the completion candidate is highlighted in the UI:

```clj
=> (println (compliment.core/documentation ".substring"))

java.lang.StringBuilder.substring
  (int int) -> String (public volatile)
  (int) -> String (public volatile)

java.lang.StringBuffer.substring
  (int int) -> String (public synchronized)
  (int) -> String (public synchronized)

java.lang.String.substring
  (int int) -> String (public)
  (int) -> String (public)
```

See [tests](test/compliment/t_core.clj) for more examples and details.

Writing your own sources is explained in [Custom
sources](https://github.com/alexander-yakushev/compliment/wiki/Custom-sources).

## License

Copyright © 2013-2025 Oleksandr Yakushev. Distributed under the Eclipse Public
License. See
[LICENSE](https://github.com/alexander-yakushev/compliment/blob/master/LICENSE).
