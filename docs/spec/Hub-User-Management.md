
# Overview

There are potentially two ways one can use ALE.  On one of this ways
user will interact with website and trust it to do majority of crypto
work (e.g. generating Key Pair, sign messages they send, etc).  To
support this we should provide various REST endpoints and somehow
store user data.

First of all, we need some database to store user data.  To keep
things simple I suggest to re-use database we already have.  We use
rocksdb that provides key-value interface.

I suggest creating two "tables" (rocksdb doesn't have RDBMS-like
tables so a table is just a set of keys with common prefix) in this
DB.

First table will be used to store all user data: login name, password
hash, public and secret keys.  More data (email, profile pic, bio,
...)  can be added as we move forward and need more features for
website.  All fields except while login, password and key pair are
optional.  This Table is indexed by login.

The second table stores `(token, user, expiration)` triples.  Someone
holding a token is capable to perform operations with this user
account.

## Password storing

The standard way to deal with passwords is to not store plaintext
passwords on the server but to store them salted and hashed.  The
passwords are not stored in plaintext to prevent information leaks in
case DB was compromised.  The passwords are salted to prevent Rainbow
Tables-style mass password cracking attacks against leaked DB.

It's hard to make this right so using a hash function designed for
password hashing makes sense.  One well-known hash function with this
property is `scrypt`.  It has adjustable complexity.  Some random
websites over the Internet suggest setting $N = 2^{15}$, $p = 1$ and
$r = 8$ to fit one hashing attempt time to 100ms on modern hardware.

The salt for `scrypt` is set to "`ALE:`" string concatenated with user
login.  This is done to prevent different users from having same
password hash if their passwords are equal.  Generating a random salt
is impossible because different hash calculation should return same
result because calculations are done client-side.  Using this salt
solves both problems by providing different salts for different users
and maitaining calculations deterministic.

This time is inadequate for server so hashing is done on the client.
After that, received hash is hashed unsalted again with fast SHA256 on
the server.  There is no need in salt during the serverside hashing
because possible inputs space is already large enough and already
salted.

## Tokens

Tokens have several purposes:

* This allows server to omit costly password validation procedure;
* More granular access control: it's possible to be logged in on
  several devices simultaniously while keeping the ability to log out
  on each of them separately.
* Tokens allow having authentication infrastructure separated from the
  rest of the system.

## Token generation

One approach that doesn't involve token storing on the server is to
use HMACed `(username, expiration)` pairs as tokens.  After receiving
a token server can check that token has valid HMAC and not expired
(just compare `expiration` from token with current time).

Secret for HMAC is generated for each user independently and is stored
in DB along with other user's data.

## Storage

Currently, we already have a persistent storage backend (RocksDB) that
stores current state.  Current state and users storage are very
different things but for the sake of simplisity I suggest to reuse
this backend while creating a separate class to abstract away all
web-related operations (in a way similar to how `AleDB` abstracts
state persistence).

We'll need two tables to support this operations:

* A table for user information.  It'll be a mapping from `UserName` to
  `UserData`.  `UserData` has all info about user: password hash and
  all additional info.
* A table for tokens.  It'll be a mapping from `(UserName, TokenId)`
  pairs to `Token`s.  `TokenId` is a textual token representation and
  `Token` has `TokenId` and expiration date inside.

We add prefixes to stored datatypes so this new "tables" will not
interfere with whatever currently exist in DB.

Introducing a separate authentication server may make sense in the
future.  Currently, the system is centralized so to keep things simple
the Hub will perform all web stuff.  Having a class that abstracts
web-related operations will make adding an auth server as simple as
implementing one more instance for this class.

# Endpoints

## `/register`

Params:

| param      | type     | comment            |
|------------|----------|--------------------|
| `email`    | _string_ | User email         |
| `password` | _string_ | Plaintext password |

Request body is a JSON with all extra information.  Possible fields:

| field | type     | comment  |
|-------|----------|----------|
| `bio` | _string_ | User bio |

Returns:

|         |                                     |
|---------|-------------------------------------|
| success | `{"ok": true}`                      |
| failure | `{"ok": false, "error": "<error>"}` |

## `/login`

Params:

| param      | type     |
|------------|----------|
| `email`    | _string_ |
| `password` | _string_ |

Returns:

|         |                                          |
|---------|------------------------------------------|
| success | `{"ok": true, "token": "<token value>"}` |
| failure | `{"ok": false, "error": "<reason>"}`     |

One need to provide token obtained from this endpoint in all
operations that require authentication.
