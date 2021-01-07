# Tlex: A Generator for Lexical Analysers

![Haskell CI](https://github.com/mizunashi-mana/tlex/workflows/Haskell%20CI/badge.svg)

## Usage

See https://hackage.haskell.org/package/tlex

## Publish

Set username / password on `~/.cabal/config`, then:

```bash
env CANDIDATE=no ./script/publish.bash \
    tlex-core-0.1.0.0 \
    tlex-0.1.0.0 \
    tlex-encoding-0.1.0.0 \
    tlex-th-0.1.0.0 \
    tlex-debug-0.1.0.0
```

## License

This project or module is dual-licensed under [the MPL 2.0](http://mozilla.org/MPL/2.0/) and [the Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0). You can choose which one to use.

Please read [LICENSE](https://github.com/mizunashi-mana/tlex/blob/master/LICENSE) before distributing.
