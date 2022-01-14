# text-rope [![Hackage](http://img.shields.io/hackage/v/text-rope.svg)](https://hackage.haskell.org/package/text-rope) [![Stackage LTS](http://stackage.org/package/text-rope/badge/lts)](http://stackage.org/lts/package/text-rope) [![Stackage Nightly](http://stackage.org/package/text-rope/badge/nightly)](http://stackage.org/nightly/package/text-rope)

A wrapper around `Text` for fast line/column navigation and logarithmic concatenation.

Here are benchmarks for 1000 edits over 70K text:

```
Split at position
  Unicode
    text-rope:
      3.09 ms ±  94 μs
    yi-rope:
      49.4 ms ± 1.4 ms, 15.98x
  UTF-16
    text-rope:
      3.09 ms ± 106 μs
    rope-utf16-splay:
      10.6 ms ± 337 μs, 3.44x
Split at offset
  Unicode
    text-rope:
      3.04 ms ± 112 μs
    core-text:
      14.0 ms ± 297 μs, 4.59x
    yi-rope:
      6.25 ms ± 102 μs, 2.06x
  UTF-16
    text-rope:
      3.40 ms ±  70 μs
    rope-utf16-splay:
      8.92 ms ± 169 μs, 2.62x
```

For 10000 edits over 700K text:

```
Split at position
  Unicode
    text-rope:
      62.9 ms ± 4.2 ms
    yi-rope:
      568  ms ±  31 ms, 9.04x
  UTF-16
    text-rope:
      61.0 ms ± 3.9 ms
    rope-utf16-splay:
      325  ms ±  13 ms, 5.32x
Split at offset
  Unicode
    text-rope:
      59.6 ms ± 3.8 ms
    core-text:
      209  ms ±  15 ms, 3.50x
    yi-rope:
      105  ms ± 7.4 ms, 1.76x
  UTF-16
    text-rope:
      63.6 ms ± 5.3 ms
    rope-utf16-splay:
      230  ms ± 9.0 ms, 3.62x
```

For 100000 edits over 7M text:

```
Split at position
  Unicode
    text-rope:
      963  ms ±  43 ms
    yi-rope:
      6.379 s ± 138 ms, 6.62x
  UTF-16
    text-rope:
      988  ms ±  19 ms
    rope-utf16-splay: T
      57.408 s ± 4.24 s, 58.12x
Split at offset
  Unicode
    text-rope:
      1.014 s ±  70 ms
    core-text:
      3.008 s ± 138 ms, 2.97x
    yi-rope:
      1.716 s ±  76 ms, 1.69x
  UTF-16
    text-rope:
      1.065 s ±  53 ms
    rope-utf16-splay:
      38.852 s ± 568 ms, 36.49x
```
