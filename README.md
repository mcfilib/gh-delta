![](delta.png)

# gh-delta

Simple, opinionated, Github changelog generator written in Haskell.

**Please Note:** This project is a work in progress.

## Usage

```
gh-delta - changelog generator

Usage: gh-delta [--auth ARG] --owner ARG --repo ARG --since ARG [--version ARG]
  Simple, opinionated, Github changelog generator written in Haskell

Available options:
  -h,--help                Show this help text
  --auth ARG               Personal access token
  --owner ARG              Repository owner
  --repo ARG               Repository name
  --since ARG              Since SHA
  --version ARG            Version for changelog entry
```

## Example

```
gh-delta --owner username --repo reponame --since sha --auth token > CHANGELOG.md
```

You may also specify your
[personal access token](https://github.com/settings/tokens/new) by specifying it
in the `GH_DELTA_AUTH` environment variable.

## Development

```
% make
clean                          Clean Haskell local packages
format                         Format Haskell source
help                           Print available tasks
install                        Compile Haskell binary
repl                           Launch ghci
spec                           Run the specs
watch                          Compile on file changes
```
