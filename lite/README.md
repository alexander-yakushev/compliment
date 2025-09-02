# Compliment-lite

Compliment-lite is a single-file simplified derivative of Compliment that can be
embedded into your project directly. It can be used in cases when you don't want
to pull Compliment as a dependency. Because of that, Compliment-lite is not
published to Clojars — you should just copy [lite.clj](src/compliment/lite.clj)
to your source files.

## Usage

There is a single namespace `compliment.lite` with the function `completions`.
The API is identical to mainline Compliment. Most options in the `options-map`
are supported.

```clj
=> (compliment.lite/completions "redu" {})
({:candidate "reduce", :type :function, :ns "clojure.core"}
 {:candidate "reduced", :type :function, :ns "clojure.core"}
 {:candidate "reduced?", :type :function, :ns "clojure.core"}
 {:candidate "reduce-kv", :type :function, :ns "clojure.core"}
 {:candidate "reductions", :type :function, :ns "clojure.core"})
```

## Differences from Compliment

Compliment-lite is a trimmed-down version of Compliment. These are things that
it does not support:

- **Context.** Completion features that require context (e.g., filtering methods
  by the class of the receiver object) don't work.
- **Local bindings and resources.** Those two sources of completions completely
  rely on context, so they are disabled in Compliment-lite.
- **Documentation.** `documentation` function is absent from Compliment-lite.
- **Extra metadata.** You cannot request extra metadata like `:arglists` or
  `:doc` for Var candidates.
- **Advanced sorting.** Compliment-lite sorts candidates alphabetically.

## How it is created

Running this command in the current directory will regenerate `lite.clj`:

    clj -i generate.clj

The script will go over Compliment source files and produce a single-file lite
version from them. The script also handles special `^{:lite ...}` metadata in
Compliment sources.

After running the script, some manual prettyfications need to be performed
because I couldn't configure zprint to do everything perfectly.
