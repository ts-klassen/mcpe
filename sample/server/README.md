server
=====

An OTP library

Build
-----

    $ rebar3 compile


```
mkdir -p _build/codex
ln -s ../../config.toml _build/codex/config.toml
CODEX_HOME="$(pwd)/_build/codex" ../bin/codex-x86_64-unknown-linux-musl
```
