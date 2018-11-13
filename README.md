# Hot code reloading demo

`hot-code-reloading-demo` aims to present a working hot code reloading example.

Its `v0.1.0` version has the following features:

* Provides a simple "hello world" on its web root -- reachable on port 8080,

* Provides version and state information on the `/s` endpoint,

* Provides continuous version and state information on the `/c' endpoint,

* Publishes a 1M `big-file` on its `/f` endpoint.  The download limit is
  artificially set to 10K/s letting us to have enough time to execute a
  successful upgrade while downloading the content.

Its `v0.2.0` version has the following improvements and additional features:

* Replaces all text content to ALL CAPS content,

* Adds a `/v` endpoint providing simple version information.

## Build

TL;DR: Execute `make all` to compile the code, build the documentation, run the
automatic tests and generate test coverage report.

`hot-code-reloading-demo` needs Erlang 21 and `rebar3` installed on a Linux.  I
prefer using the ASDF version manager for installing Erlang and rebar3, for
details please see: https://github.com/asdf-vm/asdf

Besides the build environment the demo requires 2 specific versions of the
`hot-code-reloading-demo` application that can be built with:

    $ cd hot-code-reloading-demo
    $ git checkout v0.1.0
    $ make release

    $ git checkout v0.2.0
    $ make relup

The build process results in 2 release tarballs in `_build/prod/rel/hcr_demo/`
directory.

## Running the example

The `demo.sh` helps you prepare, install and upgrade the application in its
increasing version order.

After installing `v0.1.0` you may start downloading `/f` and demonstrate the
app's stateful behaviours calling its web endpoints at `/`, `/s` and `/c`, e.g.
with `curl`:

    $ curl http://<<the-host>>:8080/s
    version: 0.2.0
    call counter: 7
    timed counter: 78

Resuming the `demo.sh` process (i.e. pressing enter) will upgrade the
application to `v0.2.0`.

## Documentation

`make doc` will generate documentation from docstrings and make it
available in `doc/index.html`.

## Running tests manually

A single test suite can be executed alone, e.g.:

    SUITE=test/<app>_SUITE make ct-suite
