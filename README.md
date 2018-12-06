# Hex.pm CLI

[![Build Status](https://travis-ci.org/rabbitmq/hexpm-cli.svg?branch=master)](https://travis-ci.org/rabbitmq/hexpm-cli)

**hexpm-cli** is a CLI wrapping the [Rebar3 Hex plugin,
`rebar3_hex`](https://github.com/hexpm/rebar3_hex). It permits
a developer to publish eg. an Erlang.mk-based project to
[Hex.pm](https://hex.pm/).

## Build

To build the escript:

```
make escript
```

The escript is named **`hexpm`**.

## Usage

You can use any `rebar3_hex` tasks: you just need to omit the `hex`
command.

* To register a user:

 ```
 hexpm user register
 ```

* To login using an existing user:

 ```
 hexpm user auth
 ```

* To publish a package:

 ```
 hexpm publish
 ```

See [Hex.pm rebar3
guide](https://hex.pm/docs/rebar3_publish) and [`rebar3_hex`
documentation](https://www.rebar3.org/v3.0/docs/hex-package-management)
for more informations.

## Internals

To achieve that, it simply depends on both `rebar` and `rebar3_hex` and
overrides the `rebar_app_info:app_file_src/1` function to return the
path to the `.app` file instead. This way, `rebar3_hex` reads metadata
from the generated `.app` file instead of `.app.src` which may not exist
in an Erlang.mk-based project.

It also overrides a few more modules to make sure the `rebar3_hex`
plugin is installed and `hexpm` doesn't read the global Rebar3
configuration.

The CLI wrapper directly calls `rebar:main/1` but first, it makes sure
to prepend `hex` to limit commands to the Hex plugin only.
