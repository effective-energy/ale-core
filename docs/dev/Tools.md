Tools
======

A tool is a small executable meant for performing a simple task related to
the use of the system.

Tools live in the `tools` directory and often consist of a single `Main.hs` file.
To make the life of the user easier, all the tools are expected to provide a
more-or-less similar UX. If you are developing a tool, please, try to do your best!


## Command line arguments

We parse command line arguments with [`optparse-applicative`][optparse-applicative].
Take a look at the [`ale-transfer` tool][ale-transfer], it uses some pretty nice
features, e.g.:

* `argument` for parsing positional (mandatory) arguments
* `option` for parsing options (optional)
* `hsubparser` for subcommands


## The library

To make it easier to create new tools and to make sure that they follow the same
UX patterns, we have a bunch of useful modules in the [`tools-common`] package.

## Paths

Production executables, and as a consequence, tools sometimes need data which is
most conveniently stored in files (e.g. Genesis data). It would be very boring to
always type this paths (and one has to invent them first, which is even more boring!),
so our executables and tools make assumptions about locations of such files.
These assumptions are encoded in [`Ale.Tools.Paths`].

All the data files are stored in a special directory, which should be
`$XDG_DATA_HOME/ale/` or something like this, you can use `ale-path data` to find
out the actual location on your system. More generally, `ale-path` lets you find out
exact locations of various files.

## Keychain

When working with our tools, especially doing manual testing, quite often one
has to specify various public or secret keys as arguments. It is not very convenient:

* copy-pasting base64-encoded keys is bothersome
* one has to somehow generate and keep track of the keys

Keychain aims to resolve these issues by letting one store the keys in a file
and assigning human-readable (and human-typeable!) names to them.
**Keep in mind: this feature is meant to be used _only_ as a convenience during testing;
production tools _must not_ use the Keychain. The file is not protected in any way.**

### Keychain for tools users

Start by generating a fresh keychain using `ale-keychain generate`. It saves the file
to a default location, but you can change it using the `-c` option. The keys will
be named `key1`, `key2`, and so on.

Now whenever a tools needs you to specify a public or a secret key, you can just
give it the name of the key.
You can still give a base64-encoded key by prefixing it with a `!` if you want to.

### Keychain for tools developers

Keychain lives in [`Ale.Tools.Keychain`].
The aim of the design behind the Keychain interface is to make it as easy to integrate
into a tool as possible.

0. Use `getDataDir` (from [`Ale.Tools.Paths`]) to get the data directory.
1. Feed the data directory to `keychainOption` to get an `optparse-applicative`
   option specification and add it to your parser.
2. Give the parsed path to `loadKeychain`.
3. Whenever you need a key argument (secret or public) use `keyReader` to parse it.
   This reader basically just reads a name of a key, but it also handles
   the `!` prefix. Store this key in `Named PublicKey` or `Named SecretKey`
   (`Named` comes from the `ParsableKey` class in [`Ale.Tools.Keychain`]).
4. To resolve a `Named` key into an actual key, use `resolveKey`.

In the name of simplicity, these functions do not do any kind of fancy error reporting,
they just throw exceptions, which is OK for tools, they always exit on invalid arguments
anyway. A consistent exception handling across tools (with nice error output) is TBD.



  [optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative

  [ale-transfer]: /node/tools/transfer/Main.hs
  [`tools-commmon`]: /tools-common/src/
  [`Ale.Tools.Keychain`]: /tools-common/src/Ale/Tools/Keychain.hs
  [`Ale.Tools.Paths`]: /tools-common/src/Ale/Tools/Paths.hs
