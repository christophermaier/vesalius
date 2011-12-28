Vesalius
==========

A reporting frontend for [xref](http://www.erlang.org/doc/apps/tools/xref_chapter.html).

## Building

Vesalius currently uses [rebar](http://github.com/basho/rebar).  You can use your own, or use the `rebar` script that is included with `vesalius`.

    rebar compile escriptize

This will generate the `vesalius` escript in the root of the project.

## Usage Examples

    vesalius --paths=deps/* --modules=foo,bar

Analyze all modules in the `ebin` directories in the expansion of `deps/*`, reporting only on the modules `foo` and `bar`.

    vesalius --paths=deps/* --apps=foo,bar

Same as above, but report on all the modules in the `foo` and `bar` apps.  It is assumed they are either on the code path or contained within one of the `paths` directories.

    vesalius --paths=deps/* --apps=foo,bar --modules=baz

Same as above, but report on all the modules in the `foo` and `bar` apps, as well as the `baz` module.

All these command-line options are very rough, and subject to change.

## Name

`vesalius` is named after [Andreas Vesalius](http://en.wikipedia.org/wiki/Andreas_Vesalius), considered to be the founder of modern human anatomy.
