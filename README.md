# hazell

## Example

```sh
$ stack build
$ stack exec -- hazell
load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "hpack",
        "prettyprinter",
    ],
    local_snapshot = "//:stack-snapshot.yaml",
)
```
