Periodic task system
====================

What is Periodic?
-----------------

Periodic is a [Gearman](http://gearman.org) like task system,
provides a generic application framework to do the periodic work.

How Does Periodic work?
-----------------------
A Periodic powered application consists of three parts: a client, a worker, and a periodic server.
The client is responsible for creating a job to be run and sending it to a periodic server.
The periodic server will find a suitable worker that can run the job when then job time is up.
The worker performs the work requested by the client and sends a stat to the periodic server.
Periodic provides client and worker APIs that your applications call to talk with the periodic server (also known as `periodicd`) so you don’t need to deal with networking or mapping of jobs.

![Periodic](https://raw.githubusercontent.com/Lupino/periodic/master/resources/periodic.jpg)

Internally, the periodic client and worker APIs communicate with the periodic server using TCP sockets.
To explain how Periodic works in more detail, lets look at a simple application that will print a string on a random delay.
The example is given in [Haskell](https://www.haskell.org), although other APIs will look quite similar.


We start off by writing a client application that is responsible for sending off the job.
It does this by using the Periodic client API to send some data associated with a function name, in this case the function `random_print`. The code for this is:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Periodic.Client
clientEnv <- open "unix:///tmp/periodic.sock"
runClientM clientEnv $ submitJob "random_print" "Hello world!" Nothing Nothing
```

This code initializes a client class, configures it to use a periodic server with `open`,
and then tells the client API to run the `random_print` function with the workload “Hello world!”.
The function name and arguments are completely arbitrary as far as Periodic is concerned,
so you could send any data structure that is appropriate for your application (text or binary).
At this point the Periodic client API will package up the job into a Periodic protocol packet
and send it to the periodic server to find an appropriate worker that can run the `random_print` function.
Let’s now look at the worker code:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Periodic.Worker
import Periodic.Job
import Control.Monad.IO.Class (liftIO)

startWorkerM "unix:///tmp/periodic/sock" $ do
  addFunc "random_print" $ do
    n <- name
    liftIO $ putStrLn h
    c <- count
    if c > 10 then workDone
              else schedLater' 5 1

  work 1
```

This code defines a function `random_print` that takes a string and print the string on a random delay for 10 times.
It is used by a worker object to register a function named `random_print`
after it is setup to connect to the same periodic server as the client.
When the periodic server receives the job to be run,
it looks at the list of workers who have registered the function name `random_print`
and forwards the job on to one of the free workers.
The Periodic worker API then takes this request,
runs the function `random_print`, and sends the job stat of that function back to the periodic server.

![Random print](https://raw.githubusercontent.com/Lupino/periodic/master/resources/random_print.png)

As you can see, the client and worker APIs (along with the periodic server) deal with the job management and network communication so you can focus on the application parts.


Quick start
----------

### Install with [stack](http://haskellstack.org/)

    git clone https://github.com/Lupino/haskell-periodic.git
    cd haskell-periodic

#### Install with [stack](http://haskellstack.org/)

    stack install

#### Install with [nix](https://nixos.org/nix/)

    nix-env -f release.nix -i periodicd periodic-client

#### Static build

    $(nix-build --no-link -A fullBuildScript)

### Start periodic server

    periodicd

### Show file worker

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Periodic.Job (JobT, name, workDone)
import Periodic.Worker (addFunc, startWorkerM, work)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  startWorkerM "unix:///tmp/periodic.sock" $ do
    addFunc "show_file" showFile
    work 10

showFile :: JobM ()
showFile = do
  liftIO . putStrLn =<< name
  workDone
```

or use the `periodic-run` command

    periodic-run show_file echo

### Submit a job

    periodic submit show_file abc.md

### Show status

    periodic status


Periodic clients
----------------

* [node-periodic](https://github.com/Lupino/node-periodic)
* [python-aio-periodic](https://github.com/Lupino/python-aio-periodic)
* [python-periodic](https://github.com/Lupino/python-periodic)
* [haskell-periodic](https://github.com/Lupino/haskell-periodic/tree/master/periodic-client)
* [go-periodic](https://github.com/Lupino/go-periodic)
* write you owne client see [protocol](https://github.com/Lupino/haskell-periodic/blob/master/resources/protocol.txt).
