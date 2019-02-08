# hlivy

[![Build Status](https://travis-ci.com/EarnestResearch/hlivy.svg?branch=master)](https://travis-ci.org/EarnestResearch/hlivy)
[![Hackage Version](https://img.shields.io/hackage/v/hlivy.svg)](https://hackage.haskell.org/package/hlivy)

## Description
`hlivy` is a Haskell library that provides bindings to the [Apache Livy](https://livy.incubator.apache.org/) REST API, which enables one to easily launch Spark applications -- either in an interactive or batch fashion -- via HTTP requests to the Livy server running on the master node of a Spark cluster.

## Usage
Start with

``` haskell
import Network.Livy
```

which brings all functionality into scope.  In particular, this exposes a monad `Livy` that has all the capabilites required to run Livy actions with `runLivy`.  Generally, the format of a Livy action follows the pattern

``` haskell
send $ basicRequestObject requiredArg1 requiredArg2
     & requestLens1 ?~ optionalArg1
     & requestLens2 ?~ optionalArg2
```

This action is ran simply:

``` haskell
let req = basicRequestObject requiredArg1 requiredArg2
        & requestLens1 ?~ optionalArg1
        & requestLens2 ?~ optionalArg2
resp <- runLivy env $ send req
```

where `env` is a suitable environment. Concretely, if one wanted to create an interactive session, one would do something like this:

``` haskell
λ => import Network.Livy
λ => -- Create a default environment
λ => env <- newEnv "localhost" 8998
λ => resp <- runLivy env $ send createSession
```

The response body, in this case a `CreateSessionResponse`, should contain the the `Session` just created.

With this `Session` at hand, one can run "statements" -- snippets of Spark Scala, PySpark, SparkR, or SparkSQL -- in the given session.

``` haskell
λ => req = runStatement (SessionId 0) "val x = 1 + 1; println(x)" SparkSession
λ => resp <- runLivy env $ send req
```

This response object, in this case a `RunStatementResponse`, contains the information needed to check on the status of the statement or retrieve results if available.

Batch actions are organized in the `Network.Livy.Client.Batch` module, and are used similarly:

``` haskell
λ => import Control.Lens
λ => -- Application JAR in HDFS
λ => req = createBatch "/user/hadoop/my-app.jar"
λ => resp <- runLivy env (send req & cbClassName ?~ "com.company.my_app" ?~ cbExecutorCores ?~ 4)
```
See [examples](https://github.com/EarnestResearch/hlivy/tree/1.0.0/examples/) for more example use.
