# Change log

### master (unreleased)

- [#83](https://github.com/alexander-yakushev/compliment/pull/83): Complete
  local bindings declared in map destructuring with namespaced keywords.
- [#83](https://github.com/alexander-yakushev/compliment/pull/83): Fix completion
  of local bindings when a context contain namespaced keywords.

### 0.3.12 (2021-10-21)

- [#77](https://github.com/alexander-yakushev/compliment/issues/77): Add ability
  to tune Compliment behavior with var's metadata.
- [#79](https://github.com/alexander-yakushev/compliment/issues/79): Fix
  completion of Java packages/classes on Windows.

### 0.3.11 (2020-10-06)

- Silence reflection Complete local bindings declared in `:strs` and `:syms` map destructuring.
- [#75](https://github.com/alexander-yakushev/compliment/issues/75): Suppress
  reflection warnings in JDK9-related code.

### 0.3.10 (2020-01-18)

- Complete local bindings declared in `:strs` and `:syms` map destructuring.
- [#71](https://github.com/alexander-yakushev/compliment/issues/71): Allow to
  explicitly filter sources when calling `compliment.core/documentation`.

### 0.3.9 (2019-02-23)

- [#53](https://github.com/alexander-yakushev/compliment/issues/53): Make context
  scanning more robust to unfinished forms.

### 0.3.8 (2018-12-17)

- Fix not completing methods/fields for freshly imported classes.
- [#58](https://github.com/alexander-yakushev/compliment/issues/58): Complete
  members for unimportant class if the context class is found.
- Retrieve context class from next form's type tag or from type tag of the same
  symbol if it was tagged in the same lexical scope.

### 0.3.7 (2018-11-22)

- Fix completion of unimported classes on JDK9+.
- Drop support for JDK1.7 and Clojure 1.7. They might still work for some time
  but it is not guaranteed.


### 0.3.6 (2018-02-22)

- [#50](https://github.com/alexander-yakushev/compliment/issues/50): Fixed
  namespace `user` overriding the alias `user` when suggesting candidates.


### 0.3.5 (2017-12-27)

- Complete local bindings introduced by `letfn`, `with-open`, `dotimes`, and
  `as->`.


### 0.3.4 (2017-04-07)

Bugfix release.

- [#47](https://github.com/alexander-yakushev/compliment/issues/47): Don't throw
  NPE when completing `::/`.


### 0.3.3 (2017-02-21)

- [#43](https://github.com/alexander-yakushev/compliment/issues/43): Allow dots
  in namespace aliases.
- [#45](https://github.com/alexander-yakushev/compliment/issues/45): Rework
  caching so that classname caches are automatically updated when classpath
  changes.


### 0.3.2 (2016-12-02)

- Autocomplete unimported classes in Boot.
- [#41](https://github.com/alexander-yakushev/compliment/issues/41): Don't follow
  symlinks when scanning classpath.


### 0.3.1 (2016-08-03)

- [clojure-emacs/cider#1818](https://github.com/clojure-emacs/cider/issues/1818):
  Don't fail if a class doesn't have a package.


### 0.3.0 (2016-06-17)

This release is a solidification of all the new features added to Compliment
since `0.2.0`.

Changes:

- All sources have been refactored to return a list of maps instead of a list of
  strings. Thus, there is no longer a concept of "tagging" candidates with extra
  data — this always happens now.
- `:tag-candidates` option to `compliment.core/completions` has been removed as
  this behavior has become the default one. `:plain-candidates true` option is
  added to revert to the previous behavior.
- Deprecated arities of `compliment.core/completions` have been removed, only
  `[prefix]` and `[prefix options-map]` remain.
- [#37](https://github.com/alexander-yakushev/compliment/issues/37): Complete
  namespace aliases with `::` prefix.
- [#39](https://github.com/alexander-yakushev/compliment/issues/39): Allow colon (`:`)
  inside var names.


### 0.2.7 (2016-02-02)

- [#35](https://github.com/alexander-yakushev/compliment/issues/35):
  `documentation` shouldn't crash when `""` is passed.


### 0.2.6 (2016-01-07)

- [#34](https://github.com/alexander-yakushev/compliment/issues/34): Include
  literals `true`, `false`, `nil` in completion results.


### 0.2.5 (2015-09-02)

- [clojure-emacs/cider-nrepl#224](https://github.com/clojure-emacs/cider-nrepl/issues/224):
  Improved handling of namespaced keywords.


### 0.2.4 (2015-06-04)

- [#31](https://github.com/alexander-yakushev/compliment/issues/31): Parse local
  `:let` bindings introduced by `doseq` and `for`.


### 0.2.3 (2015-05-17)

Hotfix release.

- [#28](https://github.com/alexander-yakushev/compliment/issues/28): Stop
  ignoring full-package candidates inside `(:import ...)` context.


### 0.2.2 (2015-04-28)

This release has several big features to mention:

- [#20](https://github.com/alexander-yakushev/compliment/issues/20): Classnames in
  the `:import` block of `ns` can now be completed by a simple name. E.g. typing
  `ArrayL` in that context will suggest `java.util.ArrayList`.
- [#24](https://github.com/alexander-yakushev/compliment/issues/24): New
  completion source for project resources.
- [#26](https://github.com/alexander-yakushev/compliment/issues/26):  New
  parameter `:extra-metadata` that takes a set of keywords, and tells Compliment
  which additional information should be attached when `:tag-candidates true` is
  active. Currently supported: `:doc`, `:arglists`.


### 0.2.1 (2015-03-05)

This release changes the Compliment API, particularly now
`compliment.core/completions` accepts a uniform `options-map` argument that
contains all necessary parameters (`ns`, `context` etc.). Other changes:

- [#16](https://github.com/alexander-yakushev/compliment/issues/16): Optional
  `:tag-candidates true` option makes Compliment return a list of maps instead
  of a list of strings. Each map has `:candidate` key with the candidate and
  possibly other keys that describe its type and other metadata.


### 0.2.0 (2014-11-12)

This release is a quite significant milestone in catching up with lost
`clojure-complete` capabilities. Here are the most important changes:

- [#14](https://github.com/alexander-yakushev/compliment/issues/14): All
  classnames can now be completed regardless whether they were imported. This
  works only on JVM where classpath scanning is possible.
- Unloaded namespaces are now completed too, using the same classpath scanning
  technique.
- [#17](https://github.com/alexander-yakushev/compliment/issues/17): Fuzzy
  completion became fuzzier, allowing to skip separators like `-` and `.` (see
  [Examples](https://github.com/alexander-yakushev/compliment/wiki/Examples))


### 0.1.4 (2014-10-03)

- Introduced `sort-order` parameter to `completions` (trptcolin/reply#153).
- Fixed various bugs related to context parsing and other issues that made CIDER
  freeze.


### 0.1.3 (2014-07-29)

- [#12](https://github.com/alexander-yakushev/compliment/issues/12): Add a
  completion source for local bindings introduced in macros like `let` and
  `defn`.
- Special forms are now only suggested if the completion is invoked at the first
  element of a list. If no context is provided, special forms are still
  suggested.
