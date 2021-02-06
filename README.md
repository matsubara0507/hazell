# hazell

## Example

```sh
$ stack build
$ stack exec -- hazell
# for WORKSPACE
load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "containers",
        "hpack",
        "prettyprinter",
    ],
    local_snapshot = "//:stack-snapshot.yaml",
)

# for BUILD.bazel
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_library",
)

haskell_library(
    name = "hazell-library",
    src_strip_prefix = "src",
    srcs = glob('src/**/*.hs'),
    deps = [
        "base",
        "containers",
        "hpack",
        "prettyprinter",
    ],
    compiler_flags = GHC_FLAGS,
)
```
