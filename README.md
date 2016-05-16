![](delta.png)

# gh-delta

Simple, opinionated, Github changelog generator written in Haskell.

**Please Note:** This project is a work in progress.

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
