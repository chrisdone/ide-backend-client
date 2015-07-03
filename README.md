# stack-ide

Stack-based JSON interface to ide-backend

This is currently a work in progress. Patches welcome.

## Installing

Install with:

    $ git clone git@github.com:fpco/ide-backend.git
    $ git clone git@github.com:commercialhaskell/stack-ide.git
    $ cd stack-ide
    $ stack install \
        stack-ide \
        stack-ide-api \
        ../ide-backend/ide-backend \
        ../ide-backend/ide-backend-rts \
        ../ide-backend/ide-backend-server \
        ../ide-backend/ide-backend-common

GHC 7.10 has some GHC API bug fixes which show up in ide-backend in
GHC 7.8, so the `stack.yaml` references a nightly Stackage build which
requires GHC 7.10.

## Running

Make sure you have a recent `stack` installed.

Go to a project and run:

    $ stack ide

If your projects conflict, it might fail to start
successfully. `stack-ide` is such a project, so you should specify the
target explicitly:

``` javascript
$ stack ide stack-ide
{"tag":"ResponseWelcome","contents":[0,1,0]}
{"tag":"ResponseUpdateSession","contents":{"progressOrigMsg":"[1 of 5] Compiling Stack.Ide.Util.ValueStream ( /home/chris/Work/stack-ide/stack-ide/src/Stack/Ide/Util/ValueStream.hs, interpreted )","progressStep":1,"progressNumSteps":5,"progressParsedMsg":"Compiling Stack.Ide.Util.ValueStream"}}
{"tag":"ResponseUpdateSession","contents":{"progressOrigMsg":"[2 of 5] Compiling Stack.Ide.CmdLine ( /home/chris/Work/stack-ide/stack-ide/src/Stack/Ide/CmdLine.hs, interpreted )","progressStep":2,"progressNumSteps":5,"progressParsedMsg":"Compiling Stack.Ide.CmdLine"}}
{"tag":"ResponseUpdateSession","contents":{"progressOrigMsg":"[3 of 5] Compiling Stack.Ide.AnnotateHaskell ( /home/chris/Work/stack-ide/stack-ide/src/Stack/Ide/AnnotateHaskell.hs, interpreted )","progressStep":3,"progressNumSteps":5,"progressParsedMsg":"Compiling Stack.Ide.AnnotateHaskell"}}
{"tag":"ResponseUpdateSession","contents":{"progressOrigMsg":"[4 of 5] Compiling Stack.Ide.AnnotateMessage ( /home/chris/Work/stack-ide/stack-ide/src/Stack/Ide/AnnotateMessage.hs, interpreted )","progressStep":4,"progressNumSteps":5,"progressParsedMsg":"Compiling Stack.Ide.AnnotateMessage"}}
{"tag":"ResponseUpdateSession","contents":{"progressOrigMsg":"[5 of 5] Compiling Stack.Ide        ( /home/chris/Work/stack-ide/stack-ide/src/Stack/Ide.hs, interpreted )","progressStep":5,"progressNumSteps":5,"progressParsedMsg":"Compiling Stack.Ide"}}
{"tag":"ResponseUpdateSession","contents":null}
```
