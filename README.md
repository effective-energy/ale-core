ALE
====

Build
------

The project uses stack. To build everything run:

```text
stack build
```

Setup
------

In order to start using Ale, you need three things:

1. A set of keys to simulate different users of the system.
2. A secret key for the server to sign blocks.
3. Genesis data.

### Quick start

The setup-script will do everything for you, but if you are a curious
and independent person, see the next subsection.

```text
stack exec ./bin/setup.sh
```

### Slow start

(Prefix all commands with `stack exec --` or start by launching a shell with
correct `PATH`: `stack exec /bin/bash`.)

1. Generate a [keychain]: `ale-keychain generate`.
2. Generate a server key: `ale-keygen -p $(ale-path server-key)`.
3. Generate a token distribution: `ale-distribute -n 10 key1 key2 key3`.
   This gives 10 money-tokens to each of the keys called _key1_, _key2_, and _key3_.
4. Feed you distribution to `ale-genesis`.


Use
----

The steps above generate a number of data files and store them in a designated directory
(you can find out which one by running `ale-path data`) so you have to
do them only once. When done, you can:

* Start Hub: `ale-hub`
* Start a node: `ale-node 8080` (8080 is the port for the REST server to listen)
* Play with provided tools.


  [keychain]: /docs/dev/Tools.md#keychain
