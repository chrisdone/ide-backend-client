# stack-ide

Stack-based JSON interface to ide-backend

This is currently a work in progress. Patches welcome.

## Installing

Install with:

    git clone --recursive https://github.com/commercialhaskell/stack-ide.git
    cd stack-ide
    stack install \
        stack-ide \
        stack-ide-api \
        ide-backend/ide-backend \
        ide-backend/ide-backend-server \
        ide-backend/ide-backend-common

GHC 7.10 has some GHC API bug fixes which show up in ide-backend in
GHC 7.8, so the `stack.yaml` references a nightly Stackage build which
requires GHC 7.10.

Note that you should install these tools in the same Stack LTS/Nightly
resolver as the projects that you want to work on, so that the
necessary packages are in scope.

## Emacs

There is Emacs integration provided by stack-mode in this repository.

Add the following to your .emacs:

``` lisp
(add-to-list 'load-path "/path/to/stack-ide/stack-mode/")
(require 'stack-mode)
(add-hook 'haskell-mode-hook 'stack-mode)
```

When opening a .hs file it will figure out where your cabal package is
and start a `stack ide` session under that package's directory.

There is an example project on the `stack-mode` branch at
[emacs-haskell-config](https://github.com/chrisdone/emacs-haskell-config/tree/stack-mode). Follow
the instructions.

## Running manually

Make sure you have a recent Git master version of `stack` installed.

Go to a project and run:

    $ stack ide start

If your projects conflict, it might fail to start
successfully. `stack-ide` is such a project, so you should specify the
target explicitly, e.g.

``` javascript
$ stack ide start stack-ide
{"tag":"ResponseWelcome","contents":[0,1,1]}
…
```
