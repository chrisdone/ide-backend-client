# stack-ide

`stack-ide` is a minor mode enabling various features based on
 the [`stack-ide`][] external process.

## Features

- <kbd>M-.</kbd>      - Go to definition of thing at point.
- <kbd>C-c C-k</kbd>  - Clear the interaction buffer.
- <kbd>C-c C-t</kbd>  - Display type info of thing at point.
- <kbd>C-c C-i</kbd>  - Display the info of the thing at point.
- <kbd>C-c C-l</kbd>  - Load the current buffer's file.

`stack-ide` minor mode also integrates with Flycheck for on-the-fly
GHC compiler error and HLint warning reporting.

## Conceptual Overview

`stack-ide` minor mode is a combination of two external
processes, [`ide-backend`][] and [`stack`][], wrapped up into the
[`stack-ide`][] process. `ide-backend` drives the GHC API to
build, query, and run your code. `stack` is a cross-platform
program for developing Haskell projects.

[`stack-ide`]: https://github.com/commercialhaskell/stack-ide
[`ide-backend`]: https://github.com/fpco/ide-backend
[`stack`]: https://github.com/commercialhaskell/stack
