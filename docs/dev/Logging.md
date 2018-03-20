# Logging in `ale-core`

For logging we use the [`log-warper`] library.
You can get initial understanding of how to use it from this
[very simple example from `log-warper` itself][log-warper/playground].
Alternatively, it’s already integrated into some modules in this project,
so that could be a good start too: [`ale-genesis`]() in `ale-core-core`.


## Adding a new logger

A **logger** is an [output handler][log-warper/LogHandler] labeled by
a unique [logger name][log-warper/LoggerName].

You need to create a new logger if you are creating a new executable.
If you’re not doing this (you’re only adding some logging messages to existing functions)
you can skip this part and go to the [next paragraph](#add-logging-messages-to-a-function-in-io).

To add a new logger you’ll need to follow these steps:

### Configuration file

Firstly you should consider choosing the name for the logger you create
according to the style we already have: start the name with `ale-`.
Let’s assume that you decided to have `ale-new-logger`, then you should add
the following line to the `log-config.yaml` file:

```yaml
  # logger named 'ale-new-logger'
  ale-new-logger:
      severity: Debug+
```

Add `file` option if you need to write messages to a file. It is not always the case,
for example logs from _tools_ don’t need to be written to files.

```yaml
  # logger named 'ale-new-logger'
  ale-new-logger:
      severity: Debug+
      file: logs/ale-new-logger.log
```

**_Note:_** All `.log` files belong to the `logs/` directory in `ale-core`.

### Executable module

In the executable module you’re creating the `main` function should start with
the [`setupAleLoggingWithName`] function from the [`Ale.Log`] module
with the name of the logger.

For example, if you had this in your `Main.hs`:

```haskell
main :: IO ()
main = do
    someFuncWhichMakesLoggingMessages
    ...
```

it should be transformed to:

```haskell
main :: IO ()
main = setupAleLoggingWithName "ale-new-logger" run

run :: WithLoggerIO m => m ()
run = do
    someFuncWhichMakesLoggingMessages
    ...
```

## Add logging messages to a function in `IO`

If you want to use logging in a function that already runs in the `IO` monad,
replace the monad with [`WithLoggerIO`][log-warper/WithLoggerIO] and `liftIO`
all your `IO` actions.

For example we had a function:

```haskell
inputLength :: IO ()
inputLength = do
    input <- getLine
    case length input of
        0 -> putStrLn "Should not be empty" >> inputLength
          -- v ^  you need to log these
        _ -> putStrLn "Well done" >> runAnythingElse
```

The logging can not be done in `IO`, so you need to change the type signature
to make the function work in a `WithLoggerIO` monad.
After this is done you can easily use [`logDebug`], [`logInfo`], [`logWarning`],
and [others][log-warper/logging-functions] to log messages:

```haskell
inputLength :: WithLoggerIO m => m ()
inputLength = do
    input <- liftIO getLine
    case length input of
        0 -> logError "Should not be empty" >> inputLength
        _ -> logInfo "Well done" >> runAnythingElse
```

**_Note:_** If the type of the `runAnythingElse` function from the example above
was `IO ()` and it didn’t have logging in it, you would need to change its type
to `runAnythingElse :: MonadIO m => m ()` or `liftIO` it.

If `runAnythingElse` had type `runAnythingElse :: IO a -> IO a` (the argument is
just `IO ()` with no logging support) but you wanted to feed to it a computation
that uses logging, then you would need to [`liftLogIO`] it:

```haskell
inputLength :: (MonadIO m, WithLogger m) => m ()
inputLength = do
    input <- liftIO getLine
    case length input of
        0 -> logError "Should not be empty" >> inputLength -- you need to log this
        _ -> do
            logInfo "Well done"
            liftLogIO $ runAnythingElse smthLogging
```

## Pure logging

`log-warper` supports pure logging, which is a very cool feature :sunglasses:.
All the related functions can be found in the [`System.Wlog.PureLogging`] module.

So if you want to log something in your function, but it doesn’t use the `IO` monad,
you need to get some monad that implements `WithLogger` in your monad stack.
Add the constraint and then use [`launchNamedPureLogWith`] to call it.

Here is an example:

```haskell
  newtype PureSmth a = ...
      deriving (MonadSmth, ...)

  instance MonadSmth m => MonadSmt (NamedLoggerName m)

  evalPureSmth :: PureSmth a -> a
  createField  :: MonadSmth m => FieldData -> m Field
  makeField    :: (MonadSmth m, WithLogger m) => FieldData -> m Field
  makeField fieldData = do
      logInfo $ "Creating Field from" <> show fieldData
      createField fieldData


  run :: (MonadIO m, WithLogger m) => m ()
  run = do
      fieldData  <- getData
      -- field :: Field
      field <- launchNamedPureLogWith evalPureSmth (makeField fieldData)
      --       ^ logging happens here
      ...
```

Also along with [`launchNamedPureLogWith`] there is [`launchNamedPureLog`] for your needs.


  [`log-warper!`]: https://hackage.haskell.org/package/log-warper
  [log-warper/playground]: https://hackage.haskell.org/package/log-warper
  [log-warper/LogHandler]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-LogHandler.html#t:LogHandler
  [log-warper/LoggerName]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-LoggerName.html#t:LoggerName
  [log-warper/WithLoggerIO]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#t:WithLoggerIO
  [log-warper/logging-functions]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#g:1
  [`logDebug`]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#v:logDebug
  [`logInfo`]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#v:logInfo
  [`logWarning`]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#v:logWarning
  [`liftLogIO`]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-CanLog.html#v:liftLogIO
  [`System.Wlog.PureLogging`]: https://github.com/serokell/log-warper/blob/master/src/System/Wlog/PureLogging.hs
  [`launchNamedPureLogWith`]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-PureLogging.html#v:launchNamedPureLogWith
  [`launchNamedPureLog`]: https://hackage.haskell.org/package/log-warper-1.7.1/docs/System-Wlog-PureLogging.html#v:launchNamedPureLog

  [`ale-genesis`]: https://github.com/serokell/ale-core/blob/master/tools-common/tools/genesis/Main.hs
  [`Ale.Log`]: https://github.com/serokell/ale-core/blob/master/core/src/Ale/Log.hs
  [`setupAleLoggingWithName`]: https://github.com/serokell/ale-core/blob/b7b13402d0c8fc2a19335cc8fad1680510a71531/core/src/Ale/Log.hs#L28
