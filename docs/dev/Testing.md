Running Tests
==============

We use [tasty] as our primary testing framework.
More precisely, we use it as a test-runner.

It is all integrated with stack, so running tests is as easy as:

```
stack test <package_name>
```

(e.g. `stack test ale-core-core`). When you do that, tasty will run the tests,
print a nice hierarchical report and exit with an appropriate exit code.

You can run only a subset of tests using the `-p` (`--pattern`) option.
Note that since you are executing not the test binary but `slack`, you have to use `--ta`
to tell it what options to pass to the test binary. So if you want, say,
to test only the `Test.Ale.Core.Crypto` module, you type:

```
stack test ale-core-core --ta '-p Test.Ale/Core/Crypto/'
```

You can select any subtree or any individual test this way.
`*` and `**` are also supported.
You can find more details regarding patterns in the [tasty README][tasty#patterns].


Test Kinds
===========

We use tests of two kinds (although, tasty supports more than these):

* [HUnit] for simple sanity checks (commonly called unit-tests)
* [hedgehog] for property testing (other people often use QuickCheck for that)


Structure
==========

All the tests live in files inside of the `test/Test` directory.
It is a good idea to have the tests structure resemble that of the package source code,
so try to put tests that test functionality implemented in some file in `src`
into a file with the same path under `test/Test`.

Tests are discovered automatically thanks to [tasty-discover]. You might, if you want,
go and read its [README][tasty-discover#readme], but, basically,
the only really important piece of information is that it uses prefixes
to distinguish kinds of tests:

* names of HUnit tests must begin with `unit_`,
* names of hedgehog tests must begin with `hprop_`.

The `ale-core-core`'s `src/Test/*` is the place for useful helpers that are used in
tests throughout the codebase. We can’t do without it because Cabal test-suits
cannot depend on test-suits of other packages, so any function that is used
in test-suits of at least two packages goes to suitable `core/src/Test/*` module.


HUnit Tests
============

You’ll mostly need only `Assertion` and `(@?=)` from `Test.HUnit`.
After you import them, you are ready to write your HUnit tests:

```haskell
unit_onePlusOne :: Assertion
unit_onePlusOne = 1 + 1 @?= 2
```

Occasionally you might need `(@?)`, but that’s pretty much all there is to it.


Hedgehog Tests
===============

This is where the fun begins.

Apparently, there is no good tutorial, but you can take a look at
the [examples][hedgehog#examples] or [this blog post][hedgehog#blog].

The general idea is that you do:

```haskell
import Hedgehog (Property, forAll, property, (===))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
```

and then write down your test, generating all the input data with
hedgehog’s generators, e.g:

```haskell
hprop_trivialSum :: Property
hprop_trivialSum = property $ do
    x <- forAll $ Gen.int (Range.constant 0 100)
    y <- forAll $ Gen.int (Range.constant 0 100)
    x + y === x + y
```

There are no `Arbitrary` instances, you just create generators for your data types
and put them into `ale-core-core`'s `src/Test/*` so that they can be reused in test-suits of all
the packages.
The convention is to put generators into a module with the same name as the module
which the data type comes from, prefixed with `Test` and suffixed with `Gen`.
Names of generators use the `gen` prefix, e.g.:

```haskell
-- 'TokenCount' comes from "Ale.Core.Tokens", so put the following definition
-- into "Test.Ale.Core.Tokens.Gen"

genTokenCount :: Gen TokenCount
genTokenCount = tokenCount <$> Gen.word64 Range.constantBounded
```

(If you are writing generators, you will also need to import `Gen` from `Hedgehog`.)

You should not use irrefutable patterns (e.g. `let Right x = ...`):

* If you expect a `Right`, use `evalEither` from `Hedgehog`.
* If you expect a `Just`, use our own `evalMaybe` from `Test.Util`.
* In all other situations do a `case` and call `failure` in the wrong branch,
  better with `footnote` or `footnoteShow` (also from `Hedgehog`).


What to Test
=============

* Every function that doesn’t maintain any complex state (e.g. `State`, `IO`, etc.)
  should have a property test which is basically a specification
  of what the function does.
* It is a good idea to provide generators for all the data types.
* Occasionally it is useful to sprinkle a bunch of HUnit tests here and there.
* Test the properties of all instances.
* We will be writing larger, more elaborate property tests for important
  and integral pieces of functionality.

There is this [tasty-lens] library for testing lenses, but it is based on [smallcheck].
We decided to create a [similar library for Hedgehog][tasty-hedgehog-lens]
but there is no progress so far.

We also have a helper for testing `Serialise` instances called `serialising`
(be careful: it cannot catch non-determinism in serialisation of, e.g., hashmaps,
see [\[ALE-42\]][ALE-42]), so all the `Serialise` instances have to be covered.

Same thing for `FromJSON`/`ToJSON` instances. The helper is called `jsoning`.



  [tasty]:                 https://hackage.haskell.org/package/tasty
  [tasty#patterns]:        https://github.com/feuerbach/tasty/blob/master/README.md#patterns
  [tasty-discover]:        https://hackage.haskell.org/package/tasty-discover
  [tasty-discover#readme]: https://github.com/lwm/tasty-discover/blob/master/README.md
  [HUnit]:                 https://hackage.haskell.org/package/HUnit
  [hedgehog]:              https://hackage.haskell.org/package/hedgehog
  [hedgehog#examples]:     https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/test/Test/Example/Basic.hs
  [hedgehog#blog]:         http://teh.id.au/posts/2017/04/23/property-testing-with-hedgehog/
  [smallcheck]:            https://hackage.haskell.org/package/smallcheck

  [tasty-lens]:            https://hackage.haskell.org/package/tasty-lens
  [tasty-hedgehog-lens]:   https://github.com/serokell/tasty-hedgehog-lens
  [ALE-42]:                https://issues.serokell.io/issue/ALE-42
