A simple and probably inefficient ini configuration parser and formatter based on `String`.

# Usage

Sections and entries in a configuration are parsed in a non-unique and position-preserving way.

```
parseConfig "global = 23\nglobal = 24\n[section1]\nlocal = 42\nlocal = 43"
```
The resulting `Config` will provide transparent access to all sections. A `Config` may be formatted to a `String` with `formatConfig`. The functions `readConfigFile` and `writeConfigFile` are provided for convenience.

## Other content

Semantically unimportant content, such as comments and whitespaces, is ignored at the moment.

# API Stability

Note that the `Monoid` instance of `Config` and the experimental `Builder` interface will be changed or removed in the future. The `Text.Config.Ini` module however will be stable.
