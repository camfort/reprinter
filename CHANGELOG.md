## 0.3.0.0 (2021-03-03)
* `Reprinting m` is now `Reprinting i m`, where `i` is the input type, which
  must be "`String`-like" (containers holding some "list" of `Char`s).
  Previously, `i` was limited to `Text`. By default, `ByteString`, `Text` and
  `String` are supported.
* Add an example module taking prompts from the 2017 paper, and rewrite the
  tests to use the definitions in there.
* Support at least GHC 8.6, 8.8, 8.10, 9.0
