![](delta.png)

# gh-delta

Simple, opinionated, Github changelog generator written in Haskell.

**Please Note:** This project is a work in progress.

## Usage

```
gh-delta - changelog generator

Usage: gh-delta [--auth ARG] --owner ARG --repo ARG --since ARG [--until ARG]
                [--label ARG]
  Simple, opinionated, Github changelog generator written in Haskell

Available options:
  -h,--help                Show this help text
  --auth ARG               Personal access token
  --owner ARG              Repository owner
  --repo ARG               Repository name
  --since ARG              Since SHA
  --until ARG              Until SHA
  --label ARG            Label for changelog entry
```

You may also specify your
[personal access token](https://github.com/settings/tokens/new) by specifying it
in the `GH_DELTA_AUTH` environment variable.

## Example

```
gh-delta --owner filib --repo gh-delta --since f44caa05adf066ae441cbdbebe54010d94172e9a --label 0.1.0.0
```

```
## [0.1.0.0] 2016-05-14 to 2016-05-17

* Add version param to CLI and render - @filib https://github.com/filib/gh-delta/pull/5
* Refactor to expose setter of datatype - @filib https://github.com/filib/gh-delta/pull/4
* Make it possible to set personal token in ENV - @filib https://github.com/filib/gh-delta/pull/3
* Configure to use PullRequestOptions - @filib https://github.com/filib/gh-delta/pull/2
* Minor cleanup - @filib https://github.com/filib/gh-delta/pull/1
```

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
