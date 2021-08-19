# hazell

Hazell is a Bazel build file generator for Bazel projects from hpack config file.

## Usage

```
$ hazell --help
hazell [OPTIONS] [BAZEL_PROJECT_ROOT_PATH (default ./)]

Available options:
  -V  --version              show version
  -h  --help                 show usage
      --package-yaml[=PATH]  PATH for package.yaml (default is ./package.yaml)
      --stack-yaml[=PATH]    PATH for stack.yaml (default is ./stack.yaml)
      --build[=PATH]         PATH for BUILD.bazel from bazel project root (default is BUILD.bazel)
  -r  --recursive            Read all dependent cabal files to build files for bazel (e.g. for setup_deps)
```

## Example

```sh
$ stack build
$ stack exec -- hazell example
```
