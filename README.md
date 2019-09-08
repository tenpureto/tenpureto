# tenpureto [![Build Status](https://travis-ci.com/tenpureto/tenpureto.svg?branch=master)](https://travis-ci.com/tenpureto/tenpureto)

## Introduction

`tenpureto` is a tool that allows to easily bootstrap software projects from a
template. Almost every programming language ecosystem provides some
bootstrapping solution. What makes `tenpureto` different is simplicity with a
reasonable amount of flexibility:

* Templates can be split into independent features that you can selectively
  include.
* Projects created from a template can be updated to include upstream changes.
* Contributing to a template is as easy as contributing to any other project.

## Installation

### macOS

You can install `tenpureto` with [Homebrew](https://brew.sh):

```sh
$ brew tap rtimush/tap # you need it only once
$ brew install tenpureto
```

### Build from sources

You need to get [Haskell Stack](https://haskellstack.org) first.

If you are building on macOS, you will also need `icu4c`:

```sh
$ brew install icu4c
```

Let `Stack` know the location of the `icu4c` library by adding
the following snippet to your `~/.stack/config.yaml`:

```yaml
extra-lib-dirs:
  - /usr/local/opt/icu4c/lib

extra-include-dirs:
  - /usr/local/opt/icu4c/include
```

On Linux you will need to install `libicu-devel` (on RPM-based distributions)
or `libicu-dev` (on DEB-based distibutions).

Once you have everything set up, get `tenpureto` sources and run

```sh
# will install tenpureto binary to ~/.local/bin
$ stack install
```
or
```sh
# will run tenpureto without installing
$ stack run -- <tenpureto options here>
```

## Usage

To create a new project run the following command:

```sh
$ tenpureto create --template <repository> <directory>
```
where

* `<repository>` — Git URL of a template repository (for GitHub you can just
    use `organization/repo`).
* `<directory>` — where to create a project.

If you have a local clone of a Git repository with `--mirror`, you can pass its
absolute path instead of a Git URL as the `<repository>` argument.

You will be asked to choose a subset of template features you want to include,
and to provide some variable values, and then the project will be created.

If you want to incorporate changes that were made in a template into a project
that you previously created, run

```sh
$ tenpureto update <directory>
```
