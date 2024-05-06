# Change log

### 0.5.5 (2024-05-06)

- Fix fuzzy matching logic (`rema` didn't match `re-matches`).

### 0.5.4 (2024-05-03)

- Deprecate `:plain-candidates` in `complliment.core/completions`.

### 0.5.3 (2024-04-11)

- [#116](https://github.com/alexander-yakushev/compliment/issues/116): Add
  reader conditional support.

### 0.5.2 (2024-03-07)

- [#118](https://github.com/alexander-yakushev/compliment/issues/118): Fix no
  completion for locals when hashset literal is used within short lambda syntax.

### 0.5.1 (2023-11-30)

- [#113](https://github.com/alexander-yakushev/compliment/issues/113): Suggest
  members of the superclass for hinted symbols.
- Fix completing classes from local directory classpath on Windows.
- **BREAKING**: `compliment.context/cache-context` no longer accepts `:same` as
  the argument to use the previously supplied context string.

### 0.5.0 (2023-11-08)

- Add [Compliment-lite](lite).
- Improve performance and efficiency of initialization and repeated functions.
  Speed up initialization, reduce memory footprint and allocation rate.
- Protect initialization with a lock so that multiple inits can't be triggered.
- **BREAKING**: Split `namespaces-and-classes` source into two separate sources.
  Thus, for the purposes of enabling/disabling sources, their keyword names are
  now `:compliment.sources.namespaces/namespaces` and
  `:compliment.sources.classes/classes`.
- **BREAKING**: Rename `ns-mappings` source to `vars`. Thus, for the purposes of
  enabling/disabling sources, its keyword name is now
  `:compliment.sources.vars/vars`.
- **BREAKING**: Remove deprecated `compliment.utils/namespaces-on-classpath`.
  Instead, you can use `compliment.utils/namespaces&files-on-classpath`.
- **BREAKING**: `compliment.core/completions` no longer accepts a string as a
  second argument instead of a proper options map. This fallback has been
  deprecated long time ago.

### 0.4.4 (2023-10-10)

- [#109](https://github.com/alexander-yakushev/compliment/issues/109): Introduce
  support for offering completions within the context of a `some->`
  and `some->>` call.

### 0.4.3 (2023-09-21)

- [#107](https://github.com/alexander-yakushev/compliment/issues/107): Infer the
  class of string and coll literals, giving more accurate completions for those.
- [#108](https://github.com/alexander-yakushev/compliment/issues/108): Don't
  infer the class of lists.

### 0.4.2 (2023-09-17) - DO NOT USE

### 0.4.1 (2023-08-23)

- [#33](https://github.com/alexander-yakushev/compliment/issues/33): Demunge
  deftype field names.
- [#61](https://github.com/alexander-yakushev/compliment/issues/61): Introduce
  support for offering completions within the context of a  `->`, `->>` and
  `doto` call.

### 0.4.0 (2023-07-05)

- Support for Clojure 1.8 and 1.9 is dropped. Compliment will most likely
  continue to work with them for a while, but the compatibility is no longer
  guaranteed.
- [#98](https://github.com/alexander-yakushev/compliment/pull/98): Find private vars when using var quote literal.
- [#98](https://github.com/alexander-yakushev/compliment/pull/98): Support `:private` and `:deprecated` as extra-metadata.

### 0.3.16 (2023-06-23)

- [#97](https://github.com/alexander-yakushev/compliment/pull/97): Extend
  completion and getting docs for symbol-strings with leading literals.

### 0.3.15 (2023-06-22)

- Complete fully-qualified classnames by their shortname prefix anywhere in the
  file (previously worked only in the `:import` section of the `ns` form).
- Fix ' and #' being swallowed when completing vars prefixed by them.
- [#91](https://github.com/alexander-yakushev/compliment/pull/91): `compliment.utils/namespaces-on-classpath` (now deprecated) takes cljc files into account.
  Add replacement `compliment.utils/namespaces&files-on-classpath` that yields a collection of maps containing the filename.
  - [#94](https://github.com/alexander-yakushev/compliment/issues/94): `compliment.sources.namespaces-and-classes/doc` accepts ns-aliases as well.

### 0.3.14 (2022-07-11)

- [#87](https://github.com/alexander-yakushev/compliment/pull/87): Fix completion
  of local bindings when a context contains '{' or '}' characters.

### 0.3.13 (2022-06-18)

- [#82](https://github.com/alexander-yakushev/compliment/pull/82): Offer completions for quoted/var-quoted symbols.
- [#83](https://github.com/alexander-yakushev/compliment/pull/83): Complete
  local bindings declared in map destructuring with namespaced keywords.
- [#83](https://github.com/alexander-yakushev/compliment/pull/83): Fix completion
  of local bindings when a context contains namespaced keywords.
- [#84](https://github.com/alexander-yakushev/compliment/pull/84): Enable
  locals completion for `defmethod` form.

### 0.3.12 (2021-10-21)

- [#77](https://github.com/alexander-yakushev/compliment/issues/77): Add ability
  to tune Compliment behavior with var's metadata.
- [#79](https://github.com/alexander-yakushev/compliment/issues/79): Fix
  completion of Java packages/classes on Windows.

### 0.3.11 (2020-10-06)

- Complete local bindings declared in `:strs` and `:syms` map destructuring.
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
  members for unimported class if the context class is found.
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
